# NanoPi R3S（OpenWrt 軟路由）安全強化紀錄

> 紀錄日期：2026-06-19
> 對象：家裡的 NanoPi R3S 軟路由
> 連線方式：`ssh root@192.168.1.1`（**只能金鑰登入**，見下方）

---

## 這台是什麼 / 網路角色

| 項目 | 值 |
|---|---|
| 硬體 | FriendlyElec **NanoPi R3S**（rockchip aarch64，雙網口）|
| 系統 | OpenWrt 25.12.4（套件管理用 `apk`，不是舊的 `opkg`）|
| 角色 | 純有線軟路由 + 防火牆 + DHCP + DNS（本身無 WiFi）|
| WAN（eth0）| static `172.16.1.18/16`，閘道 `172.16.0.254` ← **大樓共用網路後面**，對外公開 IP 是大樓的 `<HOME_PUBLIC_IP>`（全棟共用，我們不可控）|
| LAN（br-lan / eth1）| `192.168.1.1/24`，發 DHCP |
| Tailscale | 跑在 **M6 工作機（192.168.1.230）**，不是這台 R3S。R3S 不裝 Tailscale。|

**威脅模型**：對外那側其實是「大樓的共用網路」，主要要防的是**同棟其他住戶在
172.16.0.0/16 上掃描我們的 R3S**。對策就是「對 WAN 完全隱身、不暴露任何服務」。

---

## 已做的設定（依時間順序）

### 1. 套件更新（apk）
把 16 個可更新套件全部更新（含 dnsmasq 2.91→2.93 安全修補、整套 LuCI）。
```sh
apk update && apk upgrade
```
> 之後維護：`apk update && apk list -u`（看可更新）→ `apk upgrade`。

### 2. SSH 改「只能金鑰登入」🔑
- 已把本機公鑰（`~/.ssh/id_ed25519.pub`，`<your-email>`）加到
  `/etc/dropbear/authorized_keys`。
- 關閉密碼登入：
```sh
uci set dropbear.main.PasswordAuth='off'
uci set dropbear.main.RootPasswordAuth='off'
uci commit dropbear && /etc/init.d/dropbear restart
```
- **驗證過**：金鑰可登入、密碼登入被拒（`Permission denied (publickey)`）。

> ⚠️ **這把金鑰是進 R3S 的唯一鑰匙**，務必備份。弄丟只能用實體 console 進去重設。
> `./OPENWRT_PW` 那組密碼 SSH 已用不到，但 **LuCI 網頁登入和實體 console 還是用它**，別刪、別 commit 進 git。

### 3. 全網廣告過濾（adblock）
- 裝 `adblock` + `luci-app-adblock`，後端用現有 dnsmasq。
- 啟用 3 個清單：`adguard`、`adguard_tracking`、`certpl`（共 ~26 萬筆網域）。
- 每週六 05:00 自動更新清單（cron）：`0 5 * * 6 /etc/init.d/adblock reload`
- LuCI 管理：**Services → Adblock**；誤擋時把網域加進 allowlist，或 `/etc/init.d/adblock suspend` 暫停。

### 4. WAN 隱身 + 服務只綁 LAN（縱深防禦）
目標：對大樓那側（WAN）完全靜默、不開任何 port；對自己家裡（LAN）一切正常。

```sh
# (a) WAN 隱身：REJECT -> DROP（探測一律靜默丟棄，不回應）
uci set firewall.@zone[1].input='DROP'          # @zone[1] = wan

# (b) 關閉 IPv4 ping 回應（完整隱身）
uci set firewall.@rule[1].enabled='0'           # @rule[1] = Allow-Ping

# (c) SSH 只綁 LAN（不再聽 WAN）
uci set dropbear.main.Interface='lan'

# (d) LuCI 只綁 LAN IP
uci set uhttpd.main.listen_http='192.168.1.1:80'
uci set uhttpd.main.listen_https='192.168.1.1:443'

# (e) DNS 不綁 WAN 介面
uci add_list dhcp.@dnsmasq[0].notinterface='wan'

uci commit
/etc/init.d/firewall reload
/etc/init.d/uhttpd restart
/etc/init.d/dnsmasq restart
/etc/init.d/dropbear restart        # 重啟會斷現有 SSH，重連即可
```

> **IPv6 的 ping 沒關**：WAN 沒有全域 IPv6（只有 link-local），所以無所謂；而且
> ICMPv6 是 IPv6 正常運作的必需品，不能整個擋，故保留 `Allow-ICMPv6-Input`。

---

## 目前已驗證的狀態（2026-06-19）

- ✅ WAN IP `172.16.1.18` 上**沒有任何監聽服務**。
- ✅ 所有服務只在 `127.0.0.1` / `192.168.1.1` / LAN IPv6 上。
- ✅ 防火牆 WAN `input = DROP`，且**不回 IPv4 ping**。
- ✅ SSH 只在 `192.168.1.1:22`（+ LAN IPv6），金鑰登入。
- ✅ LuCI 只在 `192.168.1.1:80/443`。
- ✅ LAN 的 DHCP / DNS 正常，全家設備上網正常。
- ✅ M6 的 Tailscale 不受影響（它是對外主動連，不需要路由器開 inbound）。

**從大樓網路那側看 R3S**：ping 不回、所有 port 探測都無回應＝完全隱形。進入家裡
網路/工作機的唯一路徑是 Tailscale。

---

## ⚠️ 重要操作筆記

### uhttpd（LuCI）重開機後可能連不進去
uhttpd 現在綁死在 `192.168.1.1`。**萬一重開機後 LuCI 連不上**，是因為網頁服務比
LAN 介面早一步啟動（race condition）。解法：SSH 進去跑一行就好：
```sh
/etc/init.d/uhttpd restart
```
（SSH 和 DNS 用介面綁定，不會有這問題，只有 uhttpd 是綁 IP 才會。）

### 管理方式
- 日常：LuCI 網頁 `http(s)://192.168.1.1`（帳號 root，密碼用 `OPENWRT_PW` 那組）。
- 命令列：`ssh root@192.168.1.1`（金鑰自動帶入）。
- 兩者都**只能從 LAN（家裡）連**，從大樓網路那側連不到。

---

## 如何還原（萬一要回到預設行為）

```sh
# WAN 不隱身（回 REJECT）
uci set firewall.@zone[1].input='REJECT'
# 恢復 ping 回應
uci set firewall.@rule[1].enabled='1'
# SSH / LuCI / DNS 回到聽全介面
uci delete dropbear.main.Interface
uci set uhttpd.main.listen_http='0.0.0.0:80' && uci add_list uhttpd.main.listen_http='[::]:80'
uci set uhttpd.main.listen_https='0.0.0.0:443' && uci add_list uhttpd.main.listen_https='[::]:443'
uci del_list dhcp.@dnsmasq[0].notinterface='wan'
# 重新開放密碼登入（不建議）
uci set dropbear.main.PasswordAuth='on'; uci set dropbear.main.RootPasswordAuth='on'

uci commit && /etc/init.d/firewall reload && /etc/init.d/uhttpd restart \
  && /etc/init.d/dnsmasq restart && /etc/init.d/dropbear restart
```

---

## 相關檔案
- `tailscale_derp.md` — 為什麼大樓網路上做不了 DERP + AWS 現役紀錄
- `HiNet_DERP.md` — 之後若接 HiNet 固定 IP，在這台 R3S 自架 DERP 的計畫

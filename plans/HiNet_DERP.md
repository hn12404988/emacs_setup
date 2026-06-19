# HiNet 固定 IP 自架 DERP 計畫（取代 AWS）

> **目標**：自己簽一條中華電信（HiNet）固定 IP 線路，讓家裡 R3S 直接拿到
> 可控的真公開 IP，在 R3S 上自架 DERP，然後**關掉 AWS Taipei DERP**。
>
> **可行性**：✅ 技術上成立。與目前「大樓網路」方案的差別見下。
>
> 相關檔案：`tailscale_derp.md`（為什麼大樓網路做不到 + AWS 現役紀錄）、
> `derp/`（AWS Terraform，退役前保留）。

---

## 核心原理：為什麼換 HiNet 就能做

之前 R3S 做不了 DERP 的**唯一卡點**：公開 IP 在大樓路由器上、全棟共用、
我們無法控制 inbound（詳見 `tailscale_derp.md`）。

換成自己的 HiNet 固定 IP 後：

```
HiNet 機房
   │
   ▼
你家 ONT（小烏龜，設 bridge 模式）
   │
   ▼
R3S WAN ← 直接拿到「你自己的真公開 IP」（你完全控制）
   │
   ▼
R3S LAN 192.168.1.0/24（DHCP / adblock / 現有設定全不變）
```

關鍵變化：
1. **R3S 站在網路最邊緣**，手上是你自己的公開 IP → inbound 443/80/3478 由
   你自己的防火牆放行，**不再需要任何人配合**。
2. **固定 IP → 不需要 DDNS**，A record 設一次永遠有效。
3. **port 80 通 → Let's Encrypt HTTP-01 直接可用**，不必搞 DNS-01。

> 這三點正好把 `tailscale_derp.md` 裡列的三個卡關全部解掉。

---

## 成本分析（換算 ~32 TWD/USD）

| 方案 | 每月固定費 | 換算 | 備註 |
|---|---|---|---|
| 現況：大樓網路 + AWS DERP | NT$250 + US$15 | **≈ NT$730** | AWS 含 EC2+EIP+Route53+EBS |
| 目標：HiNet 固定 IP + R3S 自架 DERP | NT$700 + ~NT$0 | **≈ NT$700** | R3S 是現有硬體，電費可忽略 |

**結論**：兩者每月幾乎一樣（HiNet 還略省一點點），但 HiNet 方案多換到：
- 自己掌控、無雲端依賴
- 延遲可能更低（你人/公司都在台灣，連回家裡的中繼）
- 順便升級家裡網路（HiNet 專線 vs 大樓共用，速度/穩定/不跟鄰居搶頻寬）

### 退役 AWS 後的殘留費用（誠實揭露）

關掉 EC2 + EIP 後，仍會有：
- **Route 53 hosted zone ~US$0.50/月** + domain `<DERP_DOMAIN>` 續約 ~US$3/年。
- 想歸零：把 `<DERP_DOMAIN>` 的 DNS 搬到 **Cloudflare（免費）**，Route 53
  zone 就能刪，達到真正 ~NT$0 雲端費。

### 一次性費用（要先問清楚 HiNet）

- 安裝費 / 設定費（簽約常可折抵或免）。
- 固定 IP 是**「固定制」加值方案**，跟一般浮動制（光世代）不同，要指定申請。

---

## 申請前要確認的三件事（打 HiNet 123 或業務時問）

1. **要「固定 IP（固定制）」**，不是浮動制。問清楚月費、是給 **1 個** 還是
   **8 個（/29，6 個可用）** 固定 IP。DERP 只需要 1 個。
2. **ONT（小烏龜）可設 bridge 模式**，讓 R3S 用 PPPoE 直接撥到並持有公開 IP
   →（**這步最重要，避免又變成雙重 NAT**）。固定制通常就是這種架構。
3. **inbound 不擋埠**：固定制一般不擋 80/443 inbound（這正是我們要的）。順口
   確認一下。

---

## 實作步驟

### 步驟 0：簽約 + 裝機
- 申請 HiNet 固定制，拿到：PPPoE 帳號/密碼、固定 IP（或 /29 網段）、ONT。
- 確認 ONT 設 **bridge**（純數據機，不做 NAT/路由）。

### 步驟 1：R3S WAN 改接 HiNet（實體 + 設定）
- 把 R3S 的 WAN 線（eth0）從大樓光纖盒**改接到 HiNet ONT**。
- WAN 改成 PPPoE（OpenWrt）：
```sh
uci set network.wan.proto='pppoe'
uci set network.wan.username='<HiNet PPPoE 帳號>'
uci set network.wan.password='<HiNet PPPoE 密碼>'
uci set network.wan.ipv6='auto'        # HiNet 多半有 IPv6，順便開
uci commit network && /etc/init.d/network restart
```
> 若固定制是「靜態指定」而非 PPPoE，改用 `proto='static'` 填 IP/mask/gw/DNS。
> PPPoE 的 MTU 1492 OpenWrt 會自動處理；WireGuard 自己管 MTU，不受影響。

- 驗證 WAN 真的拿到公開 IP（不是 172.16.x）：
```sh
ip -4 addr show pppoe-wan; wget -qO- https://api.ipify.org   # 兩者應一致＝你的固定 IP
```

### 步驟 2：DNS A record 指到固定 IP
- 在 Route 53（或搬去 Cloudflare）把 `derp.<DERP_DOMAIN>` 的 A record
  指到你的 HiNet 固定 IP。
- **固定 IP → 設一次就好，不需要 DDNS。**

### 步驟 3：在 R3S 準備 derper 二進位
R3S 是 `aarch64` + **musl**（OpenWrt，非 glibc）：
- **(推薦) 交叉編譯**：他機 `GOOS=linux GOARCH=arm64` 靜態編譯
  `tailscale.com/cmd/derper`，binary 丟到 R3S `/usr/local/bin/derper`。
- 或 R3S 上裝 Go 直接 `go install`（注意空間/記憶體）。

### 步驟 4：procd init script 啟動 derper
`/etc/init.d/derper`，這次因為 80 埠可達，可用標準 Let's Encrypt：
```
/usr/local/bin/derper \
  -a :443 -http-port 80 -stun-port 3478 \
  -hostname derp.<DERP_DOMAIN> \
  -certmode letsencrypt -certdir /etc/derper \
  -c /etc/derper/derper.conf
```
> derper 1.98+ **必須** `-c <config>`，否則起不來（不存在會自動建，含 server key）。

### 步驟 5：R3S 防火牆放行 DERP 埠（input 到 router）
```sh
uci add firewall rule
uci set firewall.@rule[-1].name='Allow-DERP'
uci set firewall.@rule[-1].src='wan'
uci set firewall.@rule[-1].proto='tcp udp'
uci set firewall.@rule[-1].dest_port='80 443 3478'
uci set firewall.@rule[-1].target='ACCEPT'
uci commit firewall && /etc/init.d/firewall reload
```
（derper 跑在 R3S 本機，所以是允許 input，不是 DNAT 轉內網。）

### 步驟 6：Tailscale ACL（標準 443，不需自訂埠）
`https://login.tailscale.com/admin/acls`：
```jsonc
"derpMap": {
  "OmitDefaultRegions": false,        // 保留官方香港當 fallback（停電時用）
  "Regions": {
    "900": {
      "RegionID": 900, "RegionCode": "twn",
      "RegionName": "Taiwan (home R3S)",
      "Nodes": [{
        "Name": "twn-1", "RegionID": 900,
        "HostName": "derp.<DERP_DOMAIN>"
        // 用標準 443，不必填 DERPPort
      }]
    }
  }
}
```

### 步驟 7：驗證 DERP 正常
```sh
curl -sS https://derp.<DERP_DOMAIN>/      # 看到 derper banner page
tailscale netcheck                          # 看到 twn region，延遲應最低
tailscale ping <peer>                        # output 出現 "via DERP(twn)" 即成功
```

### 步驟 8：確認穩定後，關掉 AWS
```sh
cd derp/ && terraform destroy               # 刪 EC2/EIP/VPC/SG/Route53 record
```
> 跑幾天確定 R3S DERP 穩定再做。`terraform destroy` 不會刪 hosted zone 與
> domain；若要連 Route 53 zone 一起省，先把 DNS 搬 Cloudflare 再刪 zone。

---

## 取捨與風險（誠實面對）

| 項目 | AWS（現況） | HiNet + R3S（本計畫） |
|---|---|---|
| 月費 | ~US$15 | ~NT$0（線路費跟原本網路費打平）|
| 延遲 | 低（Taipei region）| 可能更低（就在你家）|
| **Uptime** | 高（雲端）| **較低：家裡停電 / R3S 重開 / 跳電 → DERP 跟著掛** |
| 掛掉時 | — | 自動退回官方香港 DERP（`OmitDefaultRegions:false`），SSH 變慢但不斷線 |
| 維運 | Terraform 一鍵 | 要自己顧 binary / 憑證續期 / 開機自啟 |
| 控制權 | AWS | 完全自己 |

**最大的取捨是 uptime**：家裡不像機房有 UPS。但因為保留官方香港當 fallback，
最壞情況只是「停電期間 SSH 退回 81ms 變慢」，不是完全連不上。對個人用途可接受。

---

## 與其他方案的關係

- 若**不想花 HiNet 的錢**：回去看 `tailscale_derp.md` 的替代方案（Oracle 免費
  東京 / AWS 降規 t4g / mosh + 官方香港）。
- 本計畫是「願意把家用網路升級成 HiNet 固定 IP」時的最佳解：花差不多的錢，
  網路升級 + DERP 自主 + 關掉雲端。

# Tailscale DERP 計畫 — 目標：搬到家裡 R3S

> **本檔目標**：把 DERP 從 AWS 搬到家裡的 NanoPi R3S（軟路由），省掉雲端
> ~$15/月。
>
> **目前狀態（2026-06-19 實測後結論）**：⛔ **在目前的大樓網路上做不到。**
> R3S 在大樓的共用 NAT 後面，沒有任何方式取得 inbound 埠轉發。詳見下方
> 「網路調查」與「卡關原因」。
>
> **現役 production：AWS Taipei DERP，請先別關**（理由見最後一節）。

---

## 背景問題（不變）

公司網路（某企業級防火牆，Symmetric NAT）到家裡 Tailscale
node **無法 hole punching 直連**，只能走 DERP 中繼。官方最近的 DERP 在香港，
公司端延遲 81ms，SSH 明顯卡頓。解法是「在台灣放一個自架 DERP」。

目前用 AWS Taipei 解決了。本檔要評估「改放家裡 R3S」是否可行。

---

## 網路調查（2026-06-19，於 R3S 上實測）

### 拓樸

```
[LAN 192.168.1.0/24]
   │  R3S 做 NAT
   ▼
[R3S WAN 172.16.1.18/16]        ← 大樓 DHCP 發的私有 IP，閘道 172.16.0.254
   │
   ▼
[大樓網路 172.16.0.0/16]         ← 全棟住戶共用網段
   │  大樓路由器 (172.16.0.254) 做 NAT
   ▼
[公開 IP <HOME_PUBLIC_IP>]        ← HiNet 真公開 IP，全棟「共用同一個」
   │
   ▼
HiNet (168.95.x) → 網際網路
```

### 實測數據

| 項目 | 結果 | 來源 |
|---|---|---|
| R3S WAN IPv4 | `172.16.1.18/16`，gw `172.16.0.254` | `ip addr` / `ip route` |
| 對外公開 IPv4 | `<HOME_PUBLIC_IP>`（HiNet，**非 CGNAT**） | `wget ifconfig.me` |
| NAT 層數 | 大樓 1 層（→公開 IP），R3S 自己再 1 層 | `traceroute`：hop1 172.16.0.254 → hop3 168.95.171.166(HiNet) → 1.1.1.1 |
| IPv6 | WAN **只有 link-local**，無全域位址，無 v6 預設路由 | `ip -6 addr` / `ip -6 route` |
| UPnP-IGD | 大樓路由器**未提供**（"No IGD found"） | `upnpc -l` |
| NAT-PMP / PCP | 無法測（無 client），但 UPnP 既關，推定也無 | — |

---

## 卡關原因（為什麼「直接做」不行）

DERP 的本質是中繼站：外部 client 必須能**主動連進** derper 的 443 / 80 /
3478 埠。要做到這件事，封包打到公開 IP `<HOME_PUBLIC_IP>` 後，必須被轉發到
R3S。但：

1. **公開 IP 在大樓路由器上，全棟共用，我們沒有管理權** → 無法手動設 port
   forwarding。
2. **大樓路由器沒開 UPnP / NAT-PMP** → 無法程式化自動開埠。
3. **共用 IP 上 443 只能指向一個住戶** → 即使大樓肯設，也會跟別人衝突。
4. **沒有全域 IPv6** → 連「用 IPv6 繞過 IPv4 NAT」這條路都沒有。

### 「固定 IP / DDNS」在這裡是假議題

- 缺的**不是固定 IP**，是「對 inbound 埠的控制權」。
- DDNS 只是把 `derp.<DERP_DOMAIN>` 指到目前公開 IP；但封包打到大樓路由器後
  仍會被丟掉（它不轉發給你）。**DDNS 在這個拓樸下完全救不了。**

---

## 要「解鎖」需要下列任一條件（含可行性）

| # | 條件 | 怎麼做 | 可行性 |
|---|---|---|---|
| 1 | 大樓肯做 port forwarding | 請管理方把公開 IP 的某個**高位埠**（443 多半被佔/被擋）轉發到 `172.16.1.18`，再由 R3S 轉給 derper | 低：要協調、共用 IP 易衝突、之後可能被改掉 |
| 2 | 取得自己的公開 IP | 拉一條中華電信線路 / 固定 IP / 把光纖盒設 bridge 給 R3S 直接拿公開 IP | 要花錢（正是你想避免的） |
| 3 | 大樓開放可路由 IPv6（inbound 允許）| derper 綁 IPv6 即可繞過 IPv4 NAT | 不在你掌控，目前完全沒有 v6 |

> 結論：**在不花錢、不靠大樓配合的前提下，R3S 自架 DERP 無解。**

---

## 實作步驟（⚠️ 只有在上面「解鎖條件」之一成立後才執行）

假設條件 1 或 2 成立、R3S 已經能收到某個 inbound 埠（以下用 `EXTPORT`
代表，例如 `443` 或大樓給的高位埠）。

### 0. 前置決策：憑證怎麼來

derper 用 Let's Encrypt autocert（`-certmode letsencrypt`）需要 **port 80
能被 LE 從外部連到**做 HTTP-01 驗證。
- 若大樓只給高位埠、80 連不進來 → **HTTP-01 不能用**，改用 **DNS-01**：
  用 `acme.sh` 或 `lego` 透過 Route 53 / Cloudflare DNS 驗證取得憑證，再用
  `-certmode manual -certdir <憑證目錄>` 餵給 derper。

### 1. 在 R3S 上準備 derper 二進位

R3S 是 `aarch64` + **musl**（OpenWrt，不是 glibc）。兩種做法：
- **(推薦) 交叉編譯**：在別台機器用 Go 設 `GOOS=linux GOARCH=arm64`，
  靜態編譯 `tailscale.com/cmd/derper`，把 binary 丟到 R3S
  `/usr/local/bin/derper`。
- 或在 R3S 上裝 Go 直接 `go install`（R3S 空間/記憶體要夠）。

### 2. procd init script 啟動 derper

`/etc/init.d/derper`（procd 格式），核心參數沿用 AWS 經驗：
```
/usr/local/bin/derper \
  -a :EXTPORT_INTERNAL -http-port 80 -stun-port 3478 \
  -hostname derp.<DERP_DOMAIN> \
  -certmode manual -certdir /etc/derper/certs \
  -c /etc/derper/derper.conf
```
> derper（tailscale 1.98+）**必須給 `-c <config>`**，否則起不來；檔案不存在
> 會自動建（含 server private key）。

### 3. R3S 防火牆開埠 + 轉發

```sh
# 在 WAN zone 允許 derper 埠（traffic rule）
uci add firewall rule
uci set firewall.@rule[-1].name='Allow-DERP'
uci set firewall.@rule[-1].src='wan'
uci set firewall.@rule[-1].proto='tcp udp'
uci set firewall.@rule[-1].dest_port='EXTPORT 3478'
uci set firewall.@rule[-1].target='ACCEPT'
uci commit firewall && /etc/init.d/firewall reload
```
（derper 跑在 R3S 本機，所以是「允許 input 到 router」，不是 DNAT 到內網。）

### 4. DDNS（只有在 inbound 已通、且用動態 IP 時才需要）

```sh
apk add ddns-scripts luci-app-ddns
```
- LuCI → Services → Dynamic DNS 設一筆，把 `derp.<DERP_DOMAIN>` 指到目前
  公開 IP。
- ⚠️ ddns-scripts **沒有乾淨的 Route 53 內建支援**；Cloudflare DNS 較容易接。
  若沿用 Route 53，要寫一支用 AWS API 的 custom update script。
- IP 變動時會有幾分鐘空窗 → 但 `derpMap` 設 `OmitDefaultRegions: false`，
  空窗期自動退回官方香港 DERP，不會整個斷。

### 5. Tailscale ACL（改 HostName + 自訂埠）

`https://login.tailscale.com/admin/acls`，最外層 JSON：
```jsonc
"derpMap": {
  "OmitDefaultRegions": false,
  "Regions": {
    "900": {
      "RegionID": 900, "RegionCode": "twn",
      "RegionName": "Taiwan (home R3S)",
      "Nodes": [{
        "Name": "twn-1", "RegionID": 900,
        "HostName": "derp.<DERP_DOMAIN>",
        "DERPPort": EXTPORT          // 用大樓給的高位埠時必填
      }]
    }
  }
}
```

### 6. 驗證

```sh
tailscale netcheck                 # 應看到 twn region，延遲最低
tailscale ping <peer>              # output 出現 "via DERP(twn)" 即成功
curl -sS https://derp.<DERP_DOMAIN>:EXTPORT/   # 看到 derper banner
```

---

## 若家裡始終解不開：省掉 AWS 成本的替代方案

| 方案 | 月費 | 延遲（公司在台灣→DERP） | 備註 |
|---|---|---|---|
| **A. Oracle Cloud Always Free（ARM, Tokyo/Osaka）** | **$0** | ~35–50ms | 永久免費；無台灣 region，東京最近；可重用現有 derper/cloud-init 知識 |
| B. AWS 降規成 `t4g.micro`（ARM） | ~$12 | 最低（Taipei） | 保留最佳延遲，省一點；cloud-init 已支援 arch 自動偵測 |
| C. 不自架，用官方香港 DERP + `mosh` | $0 | 81ms（但 mosh 本地回顯，SSH 體感不卡） | 零維運；針對「SSH 卡頓」這個真實症狀最省事 |
| D. 維持現狀 AWS Taipei | ~$15 | 最低 | 目前 production |

> 建議流程：**先別關 AWS** → 想省錢就先試 A（Oracle 東京），實測
> `tailscale netcheck` 延遲可接受再退役 AWS；若延遲不滿意，退而用 B 或 C。

---

## 現役 production：AWS Taipei DERP（保留作為現役 + 備援）

> 完整部署紀錄與 Terraform 在 `derp/`。以下為精簡索引，等 R3S 或替代方案
> 驗證成功後再 `terraform destroy` 退役。

| 項目 | 值 |
|---|---|
| Region / Instance | `ap-east-2`(Taipei) / `t3.micro` / Ubuntu 24.04 |
| Static IP | EIP `<AWS_EIP>` |
| Domain / DERP host | `<DERP_DOMAIN>`(Route 53) / `derp.<DERP_DOMAIN>` |
| TLS | Let's Encrypt via derper autocert（90 天自動續）|
| Tailscale RegionID | `900` |
| IaC | Terraform，`derp/`；AWS CLI profile `willy` |
| 月成本 | ~$15（EC2 ~$10 + EIP ~$3.65 + Route53 $0.5 + EBS ~$1 + egress）|
| 退役 | `cd derp/ && terraform destroy`（不會刪 hosted zone / domain）|

### 可重用的 derper gotchas（搬到 R3S 一樣會踩）

1. derper 1.98+ **必須** `-c <config>`，否則起不來（config 不存在會自動建）。
2. Let's Encrypt **HTTP-01 需要 port 80 從外部可達**；家裡若 80 不通改用
   **DNS-01**（acme.sh / lego + Route53/Cloudflare），derper 用 `-certmode manual`。
3. Let's Encrypt 有 rate limit；連續失敗先用 `--staging` 測，或砍
   `acme_account+key` 重抓。
4. DERP **只中繼已加密的 WireGuard 封包**，運營者看不到內容（E2E），但
   metadata（誰連誰、量、時間）看得到 → **自架比用別人的安全**，公司情境別用
   別人公開的 DERP。

---

## 歷史失敗紀錄（避免重踩，保留）

- **GCP `asia-east1`**：新付費帳號被隱性 region/family 限制（e2/n1 三個 zone
  全擋 capacity），24–48h 才解；直接換 AWS。
- **DuckDNS**：Let's Encrypt 驗證 server 從美/歐 query DuckDNS timeout（知名
  問題）→ 改用真 domain + Route 53。

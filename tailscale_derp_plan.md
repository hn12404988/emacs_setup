# Tailscale DERP 自架計畫

## 背景問題

公司網路到家裡 nanopi m6 的 Tailscale 連線**無法建立直連**，只能走
DERP 中繼。最近的官方 DERP 在香港，延遲 81ms，SSH 明顯卡頓。

## 診斷結果（`tailscale netcheck`）

| 項目 | 家裡（nanopi m6） | 公司 Wi-Fi | 意義 |
|---|---|---|---|
| UDP | ✅ true | ✅ true | UDP 沒被擋 |
| MappingVariesByDestIP | ✅ false | ❌ **true** | **公司是 Symmetric NAT** |
| PortMapping | UPnP | （無） | 公司防火牆沒開 UPnP |
| Public IPv4 | 118.166.153.2 | 211.72.236.250 | 兩端都有真的公開 IP |
| Nearest DERP | Hong Kong 41ms | Hong Kong 81ms | 官方沒台灣節點 |

**結論：**公司防火牆（Check Point Quantum Spark 1595）做 Symmetric
NAT，導致 hole punching 失敗。UDP 本身沒被擋，是 NAT mapping 行為
的問題。

## 嘗試過的方案

### 方案 A：請 IT 改防火牆 NAT 行為
- 要求：改成 Endpoint-Independent Mapping (EIM)
- 結果：❌ Check Point Quantum Spark 網頁 UI 沒有這個 toggle，需開
  Check Point 技術支援 case，IT 不想處理

### 方案 B：請 IT 做 Port Forwarding
- 要求：公司防火牆把 UDP 41641 轉發到我筆電內網 IP（配合 DHCP
  reservation）
- 結果：❌ IT 拒絕

### 方案 C（採用）：自架 DERP 在家
- 接受走 DERP 中繼的現實，但把 DERP 放在離我最近的地方（自己家）
- 公司 → 家 DERP → 家 Tailscale node，最後一段是 localhost
- 預期延遲：公司 ↔ 家的直接 RTT，大約 10-30ms（vs 目前 81ms 去香港）

## 為什麼 nanopi m6 適合當 DERP server

- 24/7 運作
- 家用網路有真的公開 IPv4（中華電信 118.166.x.x，非 CGNAT）
- 家用路由器支援 UPnP 和 port forwarding（Realtek 路由器）
- DERP 是個 Go binary，RAM <50MB，對 nanopi m6 幾乎沒負擔
- 和 Tailscale node 跑在同一台，最後一段是 localhost，反而更快

## 動手前要先驗證的事

### 1. ISP 有沒有擋 inbound 443
中華電信家用光世代**預設可能擋 80/443**。測試：
```bash
# nanopi m6 上跑臨時 listener
sudo python3 -m http.server 443
```
然後在公司網路用瀏覽器連 `https://118.166.153.2`
- 連得到 → 用 443
- 連不到 → 改用別的 port（例如 8443）在 DERP config 指定

順便測 80（Let's Encrypt ACME HTTP-01 challenge 會用到）。如果 80
被擋，改用 DNS-01 challenge 發憑證。

### 2. 家用路由器設定準備
- 幫 nanopi m6 設 DHCP reservation（綁 MAC，固定內網 IP）
- 準備好 port forwarding：外部 TCP 443 (或 8443) → nanopi m6

## 實作步驟（之後動手時參考）

### 1. 申請 DDNS domain
選項：
- **DuckDNS**（免費，最簡單）：`willy.duckdns.org`
- **Cloudflare DDNS**（需自有 domain，更穩）

在 nanopi m6 或路由器跑 DDNS updater：
```bash
# DuckDNS 範例，cron 每 5 分鐘跑
*/5 * * * * curl -s "https://www.duckdns.org/update?domains=<name>&token=<token>"
```

### 2. 安裝 derper
```bash
# 需要 Go 1.21+
go install tailscale.com/cmd/derper@latest
# binary 會在 ~/go/bin/derper
```

### 3. 設 systemd service
`/etc/systemd/system/derper.service` 大致如下：
```ini
[Unit]
Description=Tailscale DERP server
After=network.target

[Service]
ExecStart=/home/m6/go/bin/derper \
  -a :443 \
  -hostname willy.duckdns.org \
  -certmode letsencrypt \
  -certdir /var/lib/derper/certs
Restart=always
User=m6

[Install]
WantedBy=multi-user.target
```

### 4. Tailscale admin console 加入自架 DERP
修改 ACL policy file（https://login.tailscale.com/admin/acls）：
```json
{
  "derpMap": {
    "OmitDefaultRegions": false,
    "Regions": {
      "900": {
        "RegionID": 900,
        "RegionCode": "twn",
        "RegionName": "Taiwan (self-hosted)",
        "Nodes": [{
          "Name": "twn-1",
          "RegionID": 900,
          "HostName": "willy.duckdns.org"
        }]
      }
    }
  }
}
```

### 5. 驗證
```bash
tailscale netcheck      # 應該看到 twn 節點且延遲最低
tailscale ping m6       # 看走哪個 DERP，延遲多少
```

## 安全性注意事項

- DERP 只中繼**已加密**的 WireGuard 封包，DERP 運營者看不到流量內
  容（E2E 加密）
- 但 metadata 看得到：誰連誰、流量大小、時間
- 自架比用別人的更安全（尤其是公司電腦連回家這種敏感情境）
- 別人公開提供的 DERP 不建議用在公司 context

## 暫時的現況

在自架 DERP 完成前，繼續走香港 DERP（81ms）。可以用，只是 SSH 輸
入比較卡。

## TODO 檢查清單

- [ ] 測中華電信有沒有擋 inbound 443（和 80）
- [ ] 決定 DDNS 服務（DuckDNS or Cloudflare）
- [ ] 家用路由器設 nanopi m6 的 DHCP reservation
- [ ] 家用路由器設 port forwarding
- [ ] 安裝 derper
- [ ] 設 systemd service
- [ ] 設 DDNS updater（cron 或路由器內建）
- [ ] 申請 Let's Encrypt 憑證（derper 會自動跑）
- [ ] 在 Tailscale ACL 加入自架 DERP
- [ ] 驗證：`tailscale netcheck` 看到 twn 節點，延遲 <30ms
- [ ] 驗證：`tailscale ping m6` 確認走台灣 DERP

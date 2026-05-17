# Tailscale DERP 自架紀錄

> 已完成上線。實際上線方案是 AWS Taipei (ap-east-2)，而非原本計畫的家裡
> nanopi m6。Terraform IaC 在 `derp/` 目錄。

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

**結論：** 公司防火牆（Check Point Quantum Spark 1595）做 Symmetric
NAT，導致 hole punching 失敗。UDP 本身沒被擋，是 NAT mapping 行為
的問題。

## 嘗試過的方案

### 方案 A：請 IT 改防火牆 NAT 行為（失敗）
- 要求：改成 Endpoint-Independent Mapping (EIM)
- 結果：❌ Check Point Quantum Spark 網頁 UI 沒有這個 toggle，需開
  Check Point 技術支援 case，IT 不想處理

### 方案 B：請 IT 做 Port Forwarding（失敗）
- 要求：公司防火牆把 UDP 41641 轉發到我筆電內網 IP
- 結果：❌ IT 拒絕

### 方案 C：自架 DERP（成功，最終採用）
接受走 DERP 中繼的現實，但把 DERP 放在離我最近的地方（台灣 region）。

**最終選 AWS Taipei (ap-east-2)，不是家裡 nanopi。** 為什麼見下節。

## 為什麼選 AWS Taipei，不是家裡 nanopi？

原本計畫架在家裡 nanopi（免費、跟 Tailscale node 同一台、最後一段
localhost），但執行過程中考慮到：

- 中華電信家用網路**可能擋 inbound 443/80**（要先測），擋了要改 port
- 家裡停電 / 路由器重啟 / nanopi 掛 → DERP 跟著掛
- 需要設 DHCP reservation、port forwarding、DDNS、cron、cert 自動續
- 維運心智成本不低，省下的錢是 $15/月

雲端方案省事很多，AWS Taipei 剛好 2025 年開了 region，**地理上跟自
家一樣近**，乾脆用雲端。

## 失敗過的雲端嘗試（記錄一下避免下次再踩）

### 嘗試 1：GCP `asia-east1`（彰化）
- ❌ 新建的個人 GCP 帳號，所有 e2/n1/f1 family 在三個 zone 都報
  "does not have enough resources"
- 不是 zone 容量問題，是 **AWS 給新付費帳號的隱性 region 限制**，
  通常 24-48h 解除
- 在 us-central1 開 e2-micro 正常 → 確認是 region-specific
- 在 asia-east1 開 n2-standard-2 正常 → 確認是 family-specific（e2 / n1
  被擋）
- 沒等下去，直接換到 AWS

### 嘗試 2：DuckDNS（DNS provider）
- ❌ Let's Encrypt 的 verification servers 從美國/歐洲 query DuckDNS
  時 timeout（DuckDNS DNS server 從 LE 那邊看就是慢）
- 從台灣 query DuckDNS 沒事，但 Google DNS query DuckDNS 也要 4.2 秒
- LE 直接報 `query timed out looking up A`
- 是 DuckDNS 長期且知名的問題，搜 "duckdns letsencrypt timeout" 滿坑
  滿谷
- 解法：換成註冊真的 domain + Route 53（或 Cloudflare DNS）

## 最終架構

```
                                     ┌─────────────────────────┐
公司筆電 ──────────────────┐         │ AWS ap-east-2 (Taipei)  │
                          ↓         │                         │
                ┌──────────────┐    │  ┌─────────────────┐    │
                │  Internet    │ ───┼─→│ EC2 t3.micro    │    │
                └──────────────┘    │  │ (derper)        │    │
                          ↑         │  │  - :443 TLS     │    │
家裡 nanopi (Tailscale) ──┘         │  │  - :80 ACME     │    │
                                    │  │  - :3478 STUN   │    │
                                    │  └─────────────────┘    │
                                    │   EIP: 43.213.234.207   │
                                    └─────────────────────────┘
                                                ↑
                                                │ A record
                                                │
                                       ┌────────┴─────────┐
                                       │ Route 53         │
                                       │ derp.willyderp.click │
                                       └──────────────────┘
```

| 項目 | 值 |
|---|---|
| Cloud | AWS |
| Region | `ap-east-2` (Taipei) |
| Instance | `t3.micro` (2 vCPU burst, 1 GB RAM) |
| AMI | Ubuntu 24.04 LTS amd64 |
| Static IP | Elastic IP `43.213.234.207` |
| Domain | `willyderp.click` (註冊 via Route 53, .click TLD) |
| DERP hostname | `derp.willyderp.click` |
| DNS | Route 53 hosted zone (auto-created with domain) |
| TLS | Let's Encrypt via derper autocert |
| Tailscale RegionID | `900` (custom region) |
| IaC | Terraform，檔案在 `derp/` |
| AWS CLI profile | `willy` (個人 IAM user, AdministratorAccess) |

## Terraform 檔案結構（`derp/`）

```
derp/
├── main.tf                    # VPC / IGW / subnet / route table / SG /
│                              # key pair / EIP / EC2 / Route 53 record
├── variables.tf               # region, instance_type, hostname, zone...
├── outputs.tf                 # eip、ssh_command、next_steps
├── cloud-init.yaml            # VM 開機腳本（寫 systemd unit、裝 Go、
│                              # build derper、setcap、啟動）
├── terraform.tfvars           # 實際值（gitignored）
├── terraform.tfvars.example   # 範本
└── .gitignore
```

## 部署流程（從零開始）

### 前置
1. AWS 帳號 + 開好 ap-east-2 region
2. IAM user 有 AdministratorAccess
3. 本機 aws CLI profile 設好：`aws configure --profile willy`
   （Terraform 透過 `aws_profile = "willy"` 變數直接讀這個 profile，
   不需要另外設 environment variable）
4. 已註冊 domain via Route 53（hosted zone 自動建）

### Apply
```bash
cd derp/
cp terraform.tfvars.example terraform.tfvars
$EDITOR terraform.tfvars   # 填 derper_hostname 和 route53_zone

terraform init
terraform apply
```

Terraform 會：
1. 建 VPC、IGW、subnet、route table、security group
2. 建 EC2 + 灌 cloud-init
3. 配 EIP
4. 在 Route 53 加 A record
5. cloud-init 在 VM 上：裝 Go、`go install` derper、setcap、起 systemd

幾分鐘後驗證：
```bash
curl -sS https://derp.willyderp.click/   # 看到 derper banner page
```

### Tailscale ACL
進 https://login.tailscale.com/admin/acls，最外層 JSON 加：

```jsonc
{
  // ... 現有的 acls 等等
  "derpMap": {
    "OmitDefaultRegions": false,   // 保留官方 DERP 當 fallback
    "Regions": {
      "900": {
        "RegionID":   900,         // 自架 region 必須 ≥900
        "RegionCode": "twn",
        "RegionName": "Taiwan (AWS self-hosted)",
        "Nodes": [{
          "Name":     "twn-1",
          "RegionID": 900,
          "HostName": "derp.willyderp.click",
        }],
      },
    },
  },
}
```

按 Save。Tailscale clients 約 5 分鐘自動 refresh。

### 驗證
```bash
tailscale netcheck
# 應該看到 twn region 在列表中，延遲應該是最低的

tailscale ping <peer-node>
# output 提到 "via DERP(twn)" 就成功
```

## 維運與 Gotchas

### 1. derper 需要 `-c <config>` flag
新版 derper（tailscale 1.98+）要求 `-c <path>`，沒給會跑不起來。Config
檔不存在的話 derper 會自動建（含 server private key）。
```
ExecStart=/usr/local/bin/derper -a :443 -http-port 80 -stun-port 3478 \
  -hostname derp.willyderp.click -certmode letsencrypt \
  -c /var/lib/derper/derper.conf -certdir /var/lib/derper
```

### 2. cloud-init runcmd 用 `/bin/sh`，不是 bash
踩過：`set -o pipefail` 是 bash-only，dash 會直接 fail。解法是
runcmd 呼叫獨立 `.sh` script，script 用 `#!/bin/bash` shebang。

### 3. cloud-init 用 Terraform templatefile 渲染 → shell `$` 要 `$$`
`${...}` 是 Terraform 變數替換，shell 變數要寫成 `$$VAR`，最終
渲染後變回 `$VAR`。

### 4. EIP 從外部跟從內部（hairpin）
AWS NAT 通常**支援 hairpin**（同一台 VM 用 public IP 連自己也行），
但偶爾有 propagation delay。剛建好的 instance 從外部連如果 timeout，
等 1-2 分鐘 SG 規則 propagate 完。

### 5. Let's Encrypt rate limit
連續失敗會被擋。如果踩到，等 1 小時或用 `--staging` 環境測。autocert
失敗會 cache，**改 hostname 或測試前先 `rm /var/lib/derper/acme_account+key`**
觸發重抓。

### 6. AWS Security Group description 不能有 `'`
`description = "Let's Encrypt"` 會被 Terraform reject。改成
`"Lets Encrypt"`。

### 7. 新雲端帳號可能在熱門 region 卡 capacity
我們在 GCP `asia-east1` 踩到（所有 e2/n1 family 三個 zone 全擋）。
AWS `ap-east-2` 沒踩到，但理論上可能。判斷方法：去比較冷門的
region 開同樣機型，如果能開，就是 region-specific 對新帳號限制，
等 24-48h 即可。

## 安全性

- DERP **只中繼已加密的 WireGuard 封包**，DERP 運營者看不到流量內容
  （E2E 加密）
- 但 metadata 看得到：誰連誰、流量大小、時間
- 自架比用別人的更安全（公司電腦連回家這種敏感情境）
- 別人公開提供的 DERP **不**建議用在公司 context
- TLS cert 是 Let's Encrypt，自動 90 天續，autocert 處理

## 月成本

| 項目 | USD |
|---|---|
| EC2 t3.micro 24/7 (ap-east-2) | ~$10 |
| Elastic IP（綁 running instance）| ~$3.65 |
| Route 53 hosted zone | $0.50 |
| EBS 10 GB gp3 | ~$1 |
| Egress（SSH 用量輕，1–5 GB/月）| $0.20–1 |
| **小計** | **~$15/月** |
| Domain 續約（.click）| ~$3/年 |

## 砍掉怎麼做

```bash
cd derp/
terraform destroy
```

會刪掉 EC2、EIP、VPC、SG、Route 53 record，但**不會刪 hosted zone
和 domain**（那是 Route 53 console 操作的）。

要連 domain 一起砍：
1. Route 53 console → Registered domains → 關 auto-renew
2. 等到期自動釋出（或想立刻砍找 AWS support 開 case）

## 後續可能的優化

- [ ] 換 ARM instance（`t4g.micro`，省 ~20% 月費；cloud-init 已支援 arch
  auto-detect）
- [ ] 用 AWS Systems Manager Session Manager 取代 SSH（免 22 port，免 key 管理）
- [ ] 加 CloudWatch alarm 監看 derper service uptime
- [ ] 把 Terraform state 從本機搬到 S3 backend（多機協作時）
- [ ] 加 GitHub Actions 自動跑 `terraform plan` on PR

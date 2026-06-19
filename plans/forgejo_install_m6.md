# Forgejo 安裝 Runbook（M6）— 手把手，給你自己執行

> 紀錄日期：2026-06-19
> 設計來源：`forgejo_home_cloud.md`
> **執行者：你（Willy）本人**。Claude 不代勞，只負責這份說明 + 之後幫你 debug。
> 目標機器：**M6**（NanoPi M6 / Armbian trixie / arm64），你現在所在的這台。

---

## 今天的範圍

✅ **要做**：在 M6 上把 Forgejo 跑起來，只綁 `127.0.0.1`，再用 `tailscale serve` 從 tailnet（tailnet 內、含你筆電）用 HTTPS 連到。

🚫 **今天不碰**（之後另開計畫）：備份、AWS / S3、加密、private key。這份 runbook 完全不會提到那些。

### 你要先知道的前置條件

1. **免密碼 sudo**：已確認可用，指令裡的 `sudo` 不會中途要密碼。
2. **Phase 6 有先決條件**：`tailscale serve` 要能發 HTTPS，你得**先去 Tailscale 後台**幫這台 M6 開啟 **MagicDNS** 和 **HTTPS Certificates**。沒開的話 Phase 6 會報錯。前 5 個 Phase 跟後台無關，可以照跑。
3. 本機 MagicDNS 名稱固定是：`nanopi-m6.tail2bfb5b.ts.net`。

### 這份計畫用到的固定值（已幫你填好）

| 變數 | 值 |
|---|---|
| `ROOT_URL` | `https://nanopi-m6.tail2bfb5b.ts.net/` |
| 管理員帳號 | `willy` |
| 管理員 email | `willie.chang@positivegrid.com` |
| Forgejo 監聽 | `127.0.0.1:3000`（只有本機，對外靠 tailscale serve）|
| 資料目錄 | `/var/lib/forgejo` |
| 設定檔 | `/etc/forgejo/app.ini` |
| 執行檔 | `/usr/local/bin/forgejo` |
| 服務使用者 | `git` |

---

## Phase 1 — 下載並安裝 Forgejo 執行檔

- [ ] **1.1 找出最新穩定版版本號**

```bash
FORGEJO_VERSION=$(curl -fsSL https://codeberg.org/api/v1/repos/forgejo/forgejo/releases/latest \
  | grep -oP '"tag_name"\s*:\s*"\Kv?[0-9]+\.[0-9]+\.[0-9]+')
echo "$FORGEJO_VERSION"
```

預期：印出像 `v11.0.1` 之類的版本號。
> 萬一抓不到，去 https://codeberg.org/forgejo/forgejo/releases 看最新穩定版，手動設：`FORGEJO_VERSION=v11.0.1`（換成你看到的）。

- [ ] **1.2 下載 binary + 校驗碼（到 /tmp）**

```bash
cd /tmp
VER="${FORGEJO_VERSION#v}"                       # 去掉開頭的 v，例如 11.0.1
BASE="https://codeberg.org/forgejo/forgejo/releases/download/${FORGEJO_VERSION}"
curl -fSLO "${BASE}/forgejo-${VER}-linux-arm64"
curl -fSLO "${BASE}/forgejo-${VER}-linux-arm64.sha256"
```

預期：`/tmp` 下出現 `forgejo-<ver>-linux-arm64` 和對應的 `.sha256`。

- [ ] **1.3 驗證下載完整（防截斷/損毀）**

```bash
sha256sum -c "forgejo-${VER}-linux-arm64.sha256"
```

預期：`forgejo-<ver>-linux-arm64: OK`。
> 若顯示 `FAILED`，**不要繼續**——重下載。

- [ ] **1.4 安裝到 /usr/local/bin 並確認版本**

```bash
sudo install -m 755 "forgejo-${VER}-linux-arm64" /usr/local/bin/forgejo
forgejo --version
```

預期：`Forgejo version 11.0.x built with ...`。

---

## Phase 2 — 建立 git 使用者與目錄

- [ ] **2.1 建立專屬系統使用者 `git`**

```bash
sudo useradd --system --create-home --home-dir /home/git --shell /bin/bash --comment 'Forgejo' git
id git
```

預期：`id git` 印出 `uid=... (git) gid=... (git) ...`。
> 若顯示「already exists」表示已有，略過即可。

- [ ] **2.2 建立資料目錄與設定目錄、設好權限**

```bash
sudo mkdir -p /var/lib/forgejo/{custom,data,log}
sudo chown -R git:git /var/lib/forgejo
sudo chmod -R 750 /var/lib/forgejo

sudo mkdir -p /etc/forgejo
sudo chown root:git /etc/forgejo
sudo chmod 770 /etc/forgejo
```

- [ ] **2.3 確認目錄就緒**

```bash
ls -ld /var/lib/forgejo /etc/forgejo
```

預期：`/var/lib/forgejo` 屬於 `git git`；`/etc/forgejo` 屬於 `root git`。

---

## Phase 3 — 產生密鑰並寫設定檔 app.ini

> 下面**整段一次貼**（變數要在同一個 shell 裡才會帶進設定檔）。
> `forgejo generate secret` 只是印一串隨機字串，不會動到任何檔案。

- [ ] **3.1 產生三個密鑰並寫出 app.ini**

```bash
SECRET_KEY=$(forgejo generate secret SECRET_KEY)
INTERNAL_TOKEN=$(forgejo generate secret INTERNAL_TOKEN)
JWT_SECRET=$(forgejo generate secret JWT_SECRET)

sudo tee /etc/forgejo/app.ini >/dev/null <<EOF
APP_NAME = Forgejo
RUN_USER = git
RUN_MODE = prod
WORK_PATH = /var/lib/forgejo

[server]
PROTOCOL  = http
HTTP_ADDR = 127.0.0.1
HTTP_PORT = 3000
DOMAIN    = nanopi-m6.tail2bfb5b.ts.net
ROOT_URL  = https://nanopi-m6.tail2bfb5b.ts.net/
DISABLE_SSH    = true
APP_DATA_PATH  = /var/lib/forgejo/data
LFS_START_SERVER = false

[database]
DB_TYPE = sqlite3
PATH    = /var/lib/forgejo/data/forgejo.db

[repository]
ROOT = /var/lib/forgejo/data/forgejo-repositories

[security]
INSTALL_LOCK       = true
SECRET_KEY         = ${SECRET_KEY}
INTERNAL_TOKEN     = ${INTERNAL_TOKEN}
PASSWORD_HASH_ALGO = pbkdf2

[oauth2]
JWT_SECRET = ${JWT_SECRET}

[service]
DISABLE_REGISTRATION = true
REQUIRE_SIGNIN_VIEW  = true

[log]
MODE      = console
LEVEL     = info
ROOT_PATH = /var/lib/forgejo/log

[session]
PROVIDER = file
EOF
```

> 說明：`REQUIRE_SIGNIN_VIEW = true` = 連看都要先登入（個人私有最安全）。
> 之後若想在 tailnet 內匿名瀏覽，把它改成 `false`。
> `LFS_START_SERVER = false` = 今天先不開 Git LFS，之後要再開。

- [ ] **3.2 收緊設定檔權限**

```bash
sudo chown root:git /etc/forgejo/app.ini
sudo chmod 640 /etc/forgejo/app.ini
sudo chmod 750 /etc/forgejo
```

- [ ] **3.3 確認 app.ini 內容正確（密鑰已填入、非空）**

```bash
sudo grep -E 'ROOT_URL|SECRET_KEY|INTERNAL_TOKEN|JWT_SECRET' /etc/forgejo/app.ini
```

預期：`ROOT_URL` 是那串 ts.net 網址；三個密鑰後面都有一長串值（不是空白）。

---

## Phase 4 — 初始化資料庫 + 建立管理員

> 先初始化、建帳號，**再**啟動服務 → 避免兩個程序同時寫 SQLite。

- [ ] **4.1 初始化資料庫結構**

```bash
sudo -u git /usr/local/bin/forgejo migrate --config /etc/forgejo/app.ini
```

預期：一連串 `Migration ...` 訊息，最後沒有 error；`/var/lib/forgejo/data/forgejo.db` 出現。

- [ ] **4.2 建立管理員帳號（隨機密碼）**

```bash
sudo -u git /usr/local/bin/forgejo admin user create \
  --admin --username willy \
  --email willie.chang@positivegrid.com \
  --random-password \
  --config /etc/forgejo/app.ini
```

預期：印出 `generated random password is '....'`。
> ⚠️ **把這串密碼抄下來**，第一次網頁登入要用（系統會要你登入後改密碼）。

---

## Phase 5 — 建立 systemd 服務並啟動（本地驗證）

- [ ] **5.1 寫 systemd unit**

```bash
sudo tee /etc/systemd/system/forgejo.service >/dev/null <<'EOF'
[Unit]
Description=Forgejo (Beyond coding. We forge.)
After=network.target

[Service]
Type=simple
User=git
Group=git
WorkingDirectory=/var/lib/forgejo/
ExecStart=/usr/local/bin/forgejo web --config /etc/forgejo/app.ini
Restart=always
RestartSec=2s
Environment=USER=git HOME=/home/git GITEA_WORK_DIR=/var/lib/forgejo
NoNewPrivileges=true
PrivateTmp=true

[Install]
WantedBy=multi-user.target
EOF
```

> 這個 heredoc 用 `'EOF'`（有引號）= 內容**不做變數展開**，原樣寫入。

- [ ] **5.2 載入、開機自啟、立即啟動**

```bash
sudo systemctl daemon-reload
sudo systemctl enable --now forgejo
sudo systemctl status forgejo --no-pager
```

預期：`Active: active (running)`。
> 若失敗，看日誌：`journalctl -u forgejo -n 50 --no-pager`，把輸出貼給我。

- [ ] **5.3 驗證只綁在本機、且有回應**

```bash
sudo ss -tlnp | grep ':3000'
curl -fsS -o /dev/null -w 'HTTP %{http_code}\n' http://127.0.0.1:3000/
```

預期：
- `ss` 顯示 **`127.0.0.1:3000`**（不是 `0.0.0.0` 也不是 `192.168.x`）——代表 LAN/WAN 都不曝露。
- `curl` 印出 `HTTP 200` 或 `HTTP 303`（轉到登入頁），都正常。

✅ 到這裡，Forgejo 已經在 M6 本機跑起來了。

---

## Phase 6 — 用 tailscale serve 對 tailnet 開 HTTPS

> ⛔ **先決條件**：先去 **Tailscale 後台**幫 M6 開 **MagicDNS** 與 **HTTPS Certificates**。
> 沒開的話，下面的 `tailscale serve` 會報「HTTPS 未啟用」之類的錯。

- [ ] **6.1 把本機 3000 反代成 tailnet HTTPS**

```bash
sudo tailscale serve --bg --https=443 http://127.0.0.1:3000
tailscale serve status
```

預期：`tailscale serve status` 顯示 `https://nanopi-m6.tail2bfb5b.ts.net/ → http://127.0.0.1:3000`。
> 你的 tailscale 版本若旗標不同，跑 `tailscale serve --help` 對一下用法。
> 注意：用的是 **`serve`（只在 tailnet 內）**，**不是 `funnel`**（funnel 會對整個網際網路公開，今天不要用）。

- [ ] **6.2 從 M6 本機驗證 HTTPS**

```bash
curl -fsS -o /dev/null -w 'HTTP %{http_code}\n' https://nanopi-m6.tail2bfb5b.ts.net/
```

預期：`HTTP 200` 或 `HTTP 303`。

- [ ] **6.3 從你的筆電驗證（筆電要在 tailnet 裡）**

瀏覽器打開：`https://nanopi-m6.tail2bfb5b.ts.net`
用 `willy` + Phase 4.2 的隨機密碼登入，登入後改成你自己的密碼。

預期：看到 Forgejo 首頁、能登入。

---

## 驗收清單（全部打勾才算今天完成）

- [ ] `forgejo --version` 有版本號。
- [ ] `systemctl is-active forgejo` → `active`。
- [ ] `sudo ss -tlnp | grep 3000` 只看到 `127.0.0.1:3000`（沒有對 LAN/WAN 曝露）。
- [ ] 從筆電用 `https://nanopi-m6.tail2bfb5b.ts.net` 連得到、能登入。

---

## 如何還原（萬一要整個移除重來）

```bash
sudo systemctl disable --now forgejo
sudo rm -f /etc/systemd/system/forgejo.service
sudo systemctl daemon-reload
sudo tailscale serve reset                 # 清掉 serve 設定
sudo rm -rf /var/lib/forgejo /etc/forgejo /usr/local/bin/forgejo
sudo userdel -r git                        # 連 git 使用者與家目錄一起移除
```

---

## 附錄（選用，今天可不做）— 給 magit-forge 用的 access token

之後要在 Emacs 用 magit-forge（見 `magit_forge.md`）時，產生一個 token：

```bash
sudo -u git /usr/local/bin/forgejo admin user generate-access-token \
  --username willy --token-name emacs-forge \
  --scopes "read:repository,write:repository,read:issue,write:issue" \
  --config /etc/forgejo/app.ini
```

預期：印出一串 token。再依 `magit_forge.md` 的格式寫進 `~/.authinfo`（machine 換成 `nanopi-m6.tail2bfb5b.ts.net`）。
> 這部分牽涉 Emacs forge 設定，等你要用時我們再單獨處理。

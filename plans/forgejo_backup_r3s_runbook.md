# Forgejo 異地備份 Runbook（M6 → R3S）— 實作步驟與紀錄

> 紀錄日期：2026-06-20
> 設計來源：`forgejo_backup_r3s.md`
> **本次由 Claude 代為執行**（使用者授權「proceed until done」，覆蓋設計 §7.3 的「你本人執行」分工）。
> 目標：M6 每晚自動 dump Forgejo（含 app.ini），打包推到 R3S，留 30 天，並做一次還原演練。

---

## 環境事實（2026-06-20 查證）

| 項目 | 值 |
|---|---|
| Forgejo | v15.0.3，service `active`，資料 `/var/lib/forgejo`（`git:git`），設定 `/etc/forgejo/app.ini`（`640 root:git`，git 可讀）|
| M6 LAN IP | `192.168.1.230`（→ R3S 的來源 IP，用於 `from=` 鎖定）|
| R3S | OpenWrt，`root@192.168.1.1`；**無 rsync、無 sftp-server**；有 scp/tar/gzip/find/cat/mv；busybox `find` **無 `-delete`** |
| 既有 SSH | `m6` 使用者 → `root@R3S` 可用（唯一現成路徑）；`authorized_keys` 目前 1 行 |
| root@M6 → R3S | 尚不通（無金鑰、無 known_hosts）→ 本 runbook 佈署一把 root 專用金鑰 |

**傳輸方式定案**：R3S 沒有 rsync / sftp-server，故用 `ssh 'cat > 檔'`（最不挑工具），
以 `.partial` + `mv` 達成原子寫入。**輪替**用 `find ... -mtime +30 -exec rm -f {} +`（busybox 無 `-delete`）。

---

## Phase 0 — 前置確認

- [ ] **0.1** `runuser` 存在（systemd service 以 root 跑、再降權到 git 用，比 `sudo` 不挑 tty）。
- [ ] **0.2** `sudo -u git forgejo dump` 能產生有效 zip（小規模試跑）。
- [ ] **0.3** M6 → R3S 來源 IP 確認為 `192.168.1.230`（決定 `from=` 值）。

## Phase 1 — root 專用金鑰 + R3S 備份目錄

- [ ] **1.1** 在 M6 產生 root 專用金鑰 `/root/.ssh/id_forgejo_backup`（ed25519，無 passphrase）。
- [ ] **1.2** 把 R3S 的 host key 放進 root 的 `known_hosts`（**沿用 m6 已信任的那把**，不重新 TOFU）。
- [ ] **1.3** 經由 m6 既有 SSH，**備份** R3S `authorized_keys`，再 **append**（不覆蓋）root 公鑰，
  前綴限制 `from="192.168.1.230",no-pty,no-port-forwarding,no-agent-forwarding,no-X11-forwarding`。
- [ ] **1.4** 在 R3S 建 `/backup/forgejo/`。
- [ ] **1.5** 驗證 `root@M6 → R3S` 用新金鑰可登入、可寫 `/backup/forgejo/`。

## Phase 2 — 備份腳本

- [ ] **2.1** 寫 `/usr/local/bin/forgejo-backup.sh`（root 擁有，0750）。內容見下方「腳本」。
- [ ] **2.2** 手動試跑，確認：R3S 上出現 `forgejo-DATE.tar.gz`、大小與本機相符、本機暫存已清空。
- [ ] **2.3** 驗證打包內容：tarball 內含 `dump.zip` + `app.ini`。

## Phase 3 — systemd service + timer

- [ ] **3.1** 寫 `forgejo-backup.service`（`Type=oneshot`，root）。
- [ ] **3.2** 寫 `forgejo-backup.timer`（`OnCalendar=*-*-* 03:30:00`，`Persistent=true`）。
- [ ] **3.3** `daemon-reload`、`enable --now` timer；`systemctl start forgejo-backup.service` 跑一次端到端，確認 journal 無錯、R3S 有新檔。
- [ ] **3.4** `systemctl list-timers` 看到下次排程。

## Phase 4 — 還原演練

- [ ] **4.1** 從 R3S 取回最新 tarball 到 scratch，解開。
- [ ] **4.2** `dump.zip` 完整性檢查（unzip -t 或 python zipfile）。
- [ ] **4.3** 從 dump 內取出 SQLite，確認 `user` / `repository` 表有資料（證明可還原，不必另起一台）。
- [ ] **4.4** 確認 `app.ini` 內三把密鑰非空。

## Phase 5 — 回填文件

- [ ] **5.1** 更新 `R3S_hardening.md`：記錄新增的備份金鑰（含 `from=` 限制）、`/backup/forgejo/`。
- [ ] **5.2** 更新設計文件 `forgejo_backup_r3s.md` 狀態為「已實作」。
- [ ] **5.3** commit 全部。

---

## 腳本：`/usr/local/bin/forgejo-backup.sh`

```sh
#!/bin/sh
set -eu

CONF=/etc/forgejo/app.ini
WORKDIR=/var/lib/forgejo
STAGING=/var/lib/forgejo/backups
R3S=root@192.168.1.1
R3S_KEY=/root/.ssh/id_forgejo_backup
R3S_DIR=/backup/forgejo
RETAIN_DAYS=30
TS=$(date +%Y%m%d-%H%M)
BUNDLE="forgejo-${TS}.tar.gz"
SSH="ssh -i ${R3S_KEY} -o BatchMode=yes -o ConnectTimeout=10"

# 1) dump 以 git 身分執行，輸出到 git 可寫的暫存
install -d -o git -g git -m 700 "$STAGING"
rm -f "$STAGING"/dump.zip "$STAGING"/app.ini "$STAGING"/forgejo-*.tar.gz
runuser -u git -- env GITEA_WORK_DIR="$WORKDIR" \
  /usr/local/bin/forgejo dump --config "$CONF" --file "$STAGING/dump.zip" --type zip --tempdir "$STAGING"

# 2) 把 dump 與 app.ini 包成一個 tarball
cp "$CONF" "$STAGING/app.ini"
tar -czf "$STAGING/$BUNDLE" -C "$STAGING" dump.zip app.ini

# 3) 推到 R3S（無 rsync/sftp → ssh|cat；.partial+mv 原子寫入）
$SSH "$R3S" "mkdir -p '$R3S_DIR'"
$SSH "$R3S" "cat > '$R3S_DIR/$BUNDLE.partial' && mv '$R3S_DIR/$BUNDLE.partial' '$R3S_DIR/$BUNDLE'" < "$STAGING/$BUNDLE"

# 4) 驗證遠端大小 == 本機大小
LOCAL_SIZE=$(stat -c %s "$STAGING/$BUNDLE")
REMOTE_SIZE=$($SSH "$R3S" "wc -c < '$R3S_DIR/$BUNDLE'" | tr -d ' ')
[ "$LOCAL_SIZE" = "$REMOTE_SIZE" ] || { echo "SIZE MISMATCH local=$LOCAL_SIZE remote=$REMOTE_SIZE" >&2; exit 1; }
echo "pushed $BUNDLE ($LOCAL_SIZE bytes)"

# 5) R3S 輪替（busybox find 無 -delete）
$SSH "$R3S" "find '$R3S_DIR' -name 'forgejo-*.tar.gz' -mtime +$RETAIN_DAYS -exec rm -f {} +"

# 6) 清掉 M6 暫存（M6 本身就是第 1 份）
rm -f "$STAGING"/dump.zip "$STAGING"/app.ini "$STAGING/$BUNDLE"
echo "backup OK: $BUNDLE"
```

## systemd unit：`/etc/systemd/system/forgejo-backup.service`

```ini
[Unit]
Description=Forgejo nightly backup (dump + app.ini -> R3S)
After=network-online.target forgejo.service
Wants=network-online.target

[Service]
Type=oneshot
ExecStart=/usr/local/bin/forgejo-backup.sh
```

## systemd timer：`/etc/systemd/system/forgejo-backup.timer`

```ini
[Unit]
Description=Run Forgejo backup nightly at 03:30

[Timer]
OnCalendar=*-*-* 03:30:00
Persistent=true

[Install]
WantedBy=timers.target
```

---

## 還原（完整步驟，演練即依此驗證）

1. 取回：`ssh -i /root/.ssh/id_forgejo_backup root@192.168.1.1 'cat /backup/forgejo/forgejo-DATE.tar.gz' > restore.tar.gz`
2. 解開：`tar xzf restore.tar.gz` → 得到 `dump.zip` + `app.ini`
3. 依 Forgejo 文件還原 dump：停服務 → 解開 `dump.zip` → 還原 repos 與 SQLite 到資料目錄 → 放回 `app.ini` → 重啟。
4. 驗證：能登入、repo 在、可 clone。
```

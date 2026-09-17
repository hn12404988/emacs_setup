# forgejo-backup — M6 每 4 小時把 Forgejo 備份推到 R3S + AWS S3

本目錄是**已部署**的檔案的版本控制副本。實際部署位置：

| repo 檔案 | 部署到 M6 |
|---|---|
| `forgejo-backup.sh` | `/usr/local/bin/forgejo-backup.sh`（root:root 0750）|
| `forgejo-backup.service` | `/etc/systemd/system/forgejo-backup.service` |
| `forgejo-backup.timer` | `/etc/systemd/system/forgejo-backup.timer`（每 4 小時：03:30 / 07:30 / 11:30 / 15:30 / 19:30 / 23:30，`Persistent=true`）|
| `bashrc-block.sh` | 貼在 `~/.bashrc` 末端（`# >>> forgejo-backup >>>` 區塊）→ 提供 `backupstatus` / `backupnow` |

設計與步驟見：`../plans/forgejo_backup_r3s.md`（設計）、`../plans/forgejo_backup_r3s_runbook.md`（runbook）。

## 它做什麼

每 4 小時（03:30、07:30、11:30、15:30、19:30、23:30）以 root 觸發 → 以 `git` 身分 `forgejo dump`（產生**自包含** zip：repos + `forgejo-db.sql`
+ 原始 `data/forgejo.db` + `app.ini`）→ 再把一份**消毒過的 `~/.bashrc`** 塞進同一個 zip（見下）→ 同一份 zip 分別推往：

1. **R3S**：透過 SSH（`/root/.ssh/id_forgejo_backup`）推到
   `R3S:/backup/forgejo/forgejo-YYYYMMDD-HHMM.zip`，並在 R3S 刪掉 **7 天**前的舊檔（`find -mtime +7`）。
2. **AWS S3**：用專用 IAM profile `forgejo-backup` 上傳到
   `s3://forgejo-backup-020195185189-ap-east-2-an/forgejo-YYYYMMDD-HHMM.zip`。
   - 該 IAM 金鑰**只能 `s3:PutObject`**，不能讀 / 刪 / 列。
   - 上傳後比對 `ETag` 與本地 `md5sum`（單一 PUT，ETag 即 MD5）。
   - 不做用戶端加密；bucket 已開 SSE-S3 AES256。
   - 過期由 bucket lifecycle 管理：**7 天**自動刪除（rule `expire-forgejo-backups-7d`）。
     S3 lifecycle 以 UTC 每日批次執行，實際刪除可能落在滿 7 天後的那個 UTC 午夜之後（最多約 8 天）。

兩個目的地互相獨立：R3S 失敗不擋 S3，反之亦然；任一失敗 → service 以非零結束（systemd 記 failure）。

**保留量**：每 4 小時 1 份 × 7 天 = **最多 42 份**。單份 dump 目前約 48MB → R3S 約 **2GB**、S3 約 **2GB**。
R3S 根分割區有 107GB 可用，無壓力。

> 3-2-1：R3S 是**第 2 份（本地副本，快速還原）**；S3 是**第 3 份（異地，抗火災/失竊）**。

## zip 裡的 `bashrc-sanitized.txt`（消毒過的 ~/.bashrc）

每次備份都會把 `/home/m6/.bashrc` 的**消毒版**放進同一個 zip，檔名 `bashrc-sanitized.txt`。

**消毒規則**：**第一個字是 `export` 的行整行刪掉**（那些行可能含金鑰），換成
`# [redacted export] <變數名>`。若被刪的行以 `\` 續行，續行也一起刪掉。

⚠️ **兩件要知道的事**

1. 這條規則**連無害的 `export` 也一起刪**（`PATH`、`LANG`、`EDITOR`、`DENO_INSTALL`…）。
   所以這份檔案是**參考用副本，不能直接拿來當 `.bashrc` 跑**。還原時要自己把 PATH 那幾行補回來
   （被刪的位置都留有 `# [redacted export] PATH` 標記，行號與原檔一致）。
2. 反過來說，**不是 `export` 開頭的機密不會被刪**（例如寫在 `alias` 裡的 token）。
   規則就是「以 export 開頭」，不是「偵測機密」。要更嚴，得再加規則。

**Fail-closed**：消毒後若還有任何 `export` 行殘留，該檔**不會**被放進 zip，且該次備份標記為
`bashrc=fail`（整個 service 以非零結束）。`~/.bashrc` 讀不到時 → `bashrc=skip` + WARN，不算失敗。

備份結果那行會顯示狀態：`backup OK: forgejo-....zip r3s=ok s3=ok bashrc=ok`。

## 兩個 shell 指令（`backupstatus` / `backupnow`）

正本在 `bashrc-block.sh`，內容貼在 `~/.bashrc` 末端。改完要重貼：

```sh
# 先刪掉舊區塊（# >>> forgejo-backup >>> ... # <<< forgejo-backup <<<），再：
cat bashrc-block.sh >> ~/.bashrc && . ~/.bashrc
```

- **`backupstatus`** —— 過去 7 天跑得順不順。會顯示：
  - 一行結論：`HEALTHY` / `CHECK` / `PROBLEM`
  - timer 狀態 + 下次執行時間
  - journal 裡的成功 / 失敗次數（**注意**：journal 只留幾天，起始日會標出來）
  - R3S 與 S3 上實際有幾份、多大、最新一份是哪個
  - **每日表**：每天實際落地幾份 / 應該幾份（以 R3S 上的檔案為準，這才是真憑據）
  - **失敗清單**：把失敗的那幾次單獨列出來（不會被最近幾筆蓋掉）
  - 需要 `sudo`（讀 R3S 要 root 金鑰）與 `aws` profile `willy`（列 S3）。任一不通只會顯示
    「unreachable」，不會中斷。
- **`backupnow`** —— 立刻跑一次（等同 `sudo systemctl start forgejo-backup.service`），
  跑完直接印出 `r3s=` / `s3=` / `backup OK|FAILED` 幾行。

> 每日表的「應該幾份」只從 **timer 檔案的安裝日**起算 6 份/天；更早的日子標成
> `before the every-4h change`，不會誤報成「漏了 5 份」。

## 重新部署

```sh
sudo install -m 750 -o root -g root forgejo-backup.sh /usr/local/bin/forgejo-backup.sh
sudo install -m 644 forgejo-backup.service /etc/systemd/system/forgejo-backup.service
sudo install -m 644 forgejo-backup.timer   /etc/systemd/system/forgejo-backup.timer
sudo systemctl daemon-reload
sudo systemctl enable --now forgejo-backup.timer
sudo systemctl start forgejo-backup.service   # 立刻跑一次
```

## 手動跑一次 / 看結果

```sh
sudo systemctl start forgejo-backup.service
journalctl -u forgejo-backup.service -n 20 --no-pager      # 應有 "backup OK"
systemctl list-timers forgejo-backup.timer --no-pager      # 看下次排程
```

## 還原（已驗證可行的路徑：用原始 forgejo.db，不需重放 SQL）

```sh
# 1) 取回某天的備份（R3S 最快；或從 S3 用 admin profile 下載）
sudo ssh -i /root/.ssh/id_forgejo_backup root@192.168.1.1 \
  'cat /backup/forgejo/forgejo-YYYYMMDD-HHMM.zip' > restore.zip

# 從 S3 下載（用 willy admin；forgejo-backup 專用金鑰不能讀）
aws s3 cp s3://forgejo-backup-020195185189-ap-east-2-an/forgejo-YYYYMMDD-HHMM.zip restore.zip --profile willy
# 2) 解開
unzip restore.zip -d restore/
#    restore/bashrc-sanitized.txt = 消毒過的 ~/.bashrc（參考用，export 行已被拿掉）
# 3) 停服務、還原資料目錄與設定
sudo systemctl stop forgejo
sudo cp -a restore/data/.            /var/lib/forgejo/data/      # 含 forgejo.db(+wal/shm)
sudo cp -a restore/repos/.           /var/lib/forgejo/data/forgejo-repositories/   # 視 dump 結構調整
sudo cp restore/app.ini              /etc/forgejo/app.ini
sudo chown -R git:git /var/lib/forgejo
sudo systemctl start forgejo
```

> ⚠️ **SQLite 還原請用 dump 內的原始 `data/forgejo.db`（已驗證 `integrity_check=ok`）。**
> dump 內的 `forgejo-db.sql` 用了 `unistr()` 函式；M6 系統的 `sqlite3`（3.46.1 patched）**不認得**它，
> 直接 `sqlite3 x.db < forgejo-db.sql` 會失敗。要用 SQL 路徑，得換一個支援 `unistr()` 的 sqlite3。

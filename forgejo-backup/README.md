# forgejo-backup — M6 每晚把 Forgejo 備份推到 R3S

本目錄是**已部署**的檔案的版本控制副本。實際部署位置：

| repo 檔案 | 部署到 M6 |
|---|---|
| `forgejo-backup.sh` | `/usr/local/bin/forgejo-backup.sh`（root:root 0750）|
| `forgejo-backup.service` | `/etc/systemd/system/forgejo-backup.service` |
| `forgejo-backup.timer` | `/etc/systemd/system/forgejo-backup.timer`（每晚 03:30，`Persistent=true`）|

設計與步驟見：`../plans/forgejo_backup_r3s.md`（設計）、`../plans/forgejo_backup_r3s_runbook.md`（runbook）。

## 它做什麼

每晚（03:30）以 root 觸發 → 以 `git` 身分 `forgejo dump`（產生**自包含** zip：repos + `forgejo-db.sql`
+ 原始 `data/forgejo.db` + `app.ini`）→ 透過 SSH（`/root/.ssh/id_forgejo_backup`）推到
`R3S:/backup/forgejo/forgejo-YYYYMMDD-HHMM.zip` → 在 R3S 刪掉 30 天前的舊檔 → 清掉 M6 暫存。

> 這是 3-2-1 的**第 2 份（本地副本）**。R3S 與 M6 同棟，**不抗火災/失竊**；真正的異地第 3 份（S3 + 加密）尚未做。

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
# 1) 取回某天的備份
sudo ssh -i /root/.ssh/id_forgejo_backup root@192.168.1.1 \
  'cat /backup/forgejo/forgejo-YYYYMMDD-HHMM.zip' > restore.zip
# 2) 解開
unzip restore.zip -d restore/
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

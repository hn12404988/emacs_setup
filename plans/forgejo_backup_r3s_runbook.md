# Forgejo 異地備份 Runbook（M6 → R3S）— 執行紀錄

> 紀錄日期：2026-06-20
> 設計來源：`forgejo_backup_r3s.md`
> **本次由 Claude 代為執行**（使用者授權「proceed until done」，覆蓋設計 §7.3 的「你本人執行」分工）。
> 狀態：**✅ 全部執行完成並驗證（2026-06-20）**。
> 部署檔正本在 `../forgejo-backup/`；與設計的差異見 `forgejo_backup_r3s.md` §11。

---

## 環境事實（2026-06-20 查證）

| 項目 | 值 |
|---|---|
| Forgejo | v15.0.3，service `active`，資料 `/var/lib/forgejo`（`git:git`），設定 `/etc/forgejo/app.ini`（`640 root:git`，git 可讀）|
| M6 LAN IP | `192.168.1.230`（→ R3S 來源 IP）|
| R3S | OpenWrt，dropbear **v2025.89**，`root@192.168.1.1`；**無 rsync、無 sftp-server**；有 scp/tar/gzip/find/cat/mv；busybox `find` **無 `-delete`** |
| 既有 SSH | `m6` → `root@R3S` 可用；`authorized_keys` 原 1 行 |
| `runuser` | 在 `/usr/sbin/runuser`（root PATH 內，m6 PATH 不含 sbin）|

**重要實測**：Forgejo v15 的 `forgejo dump` **已自包含** `app.ini`（與 live 檔 sha256 一致）＋`forgejo-db.sql`＋
原始 `data/forgejo.db`＋所有 repos → 不需另外打包，直接推 dump 的 `.zip` 即可。
**傳輸**：R3S 無 rsync/sftp → 用 `ssh 'cat > 檔'`（`.partial`+`mv` 原子寫入）。**輪替**用 `find ... -mtime +30 -exec rm`。

---

## 執行清單（全部完成 ✅）

### Phase 0 — 前置確認
- [x] `runuser` 存在於 `/usr/sbin/runuser`（script 用絕對路徑）。
- [x] `runuser -u git -- forgejo dump` 能產生有效 zip（10MB，含 app.ini + DB + repos）。
- [x] M6 → R3S 來源 IP = `192.168.1.230`。

### Phase 1 — root 專用金鑰 + R3S 備份目錄
- [x] 產生 `/root/.ssh/id_forgejo_backup`（ed25519，註解 `forgejo-backup@m6`）。
- [x] R3S host key 寫入 root 的 `known_hosts`（**比對指紋與 m6 已信任的相同**才寫，非盲信 TOFU）。
- [x] 備份 R3S `authorized_keys` → `authorized_keys.bak-20260620`，再 **append**（不覆蓋）root 公鑰。
- [x] ⚠️ **`from=` 無效**：dropbear v2025.89 不接受 `from="192.168.1.230"`（引號/無引號皆被拒，整把金鑰失效）。
  改用可用的 `no-pty,no-port-forwarding,no-agent-forwarding,no-X11-forwarding`。
- [x] 建 `/backup/forgejo/`。
- [x] 驗證 root 新金鑰可登入、可寫、`no-pty` 生效。

### Phase 2 — 備份腳本
- [x] 安裝 `/usr/local/bin/forgejo-backup.sh`（root:root 0750）。**正本：`../forgejo-backup/forgejo-backup.sh`**。
- [x] 手動跑通：dump → 推 R3S（大小相符 10,361,291 bytes）→ 本機暫存清空。

### Phase 3 — systemd service + timer
- [x] 安裝 `forgejo-backup.service`（oneshot，root）+ `forgejo-backup.timer`（`OnCalendar=*-*-* 03:30:00`，`Persistent=true`）。
  **正本：`../forgejo-backup/`**。
- [x] `daemon-reload` + `enable --now` timer；經 systemd `start` 跑一次 → `status=0/SUCCESS`（證明無 tty 也 OK）。
- [x] `list-timers` 顯示下次 **2026-06-21 03:30 CST**。

### Phase 4 — 還原演練
- [x] 從 R3S 取回最新 zip、解開。
- [x] zip 完整性 `unzip -t` = OK。
- [x] 用**原始 `data/forgejo.db`** 開啟：`integrity_check=ok`、1 admin（willy）、4 repos，與 live 一致。
- [x] ⚠️ `forgejo-db.sql` 用 `unistr()`，M6 `sqlite3`（3.46.1 patched）不認得 → SQL 重放會失敗。**還原走原始 DB 檔**（見下）。
- [x] dump 內 `app.ini` 對 live sha256 一致。

### Phase 5 — 回填文件
- [x] `R3S_hardening.md`：記錄備份金鑰（含 `from=` 不支援）、`/backup/forgejo/`、新增驗證狀態行。
- [x] `forgejo_backup_r3s.md`：狀態改「已實作」、§2 前提修正、§11 實作結果。
- [x] `../forgejo-backup/`：部署檔正本 + README。
- [x] commit。

---

## 部署檔正本

不在本 runbook 重貼程式碼，以免和正本漂移。請看：

- 腳本：`../forgejo-backup/forgejo-backup.sh`
- service / timer：`../forgejo-backup/forgejo-backup.service`、`../forgejo-backup/forgejo-backup.timer`
- 安裝 / 操作 / 還原速查：`../forgejo-backup/README.md`

---

## 還原（已驗證可行：用原始 DB 檔，不重放 SQL）

```sh
# 1) 取回某天備份
sudo ssh -i /root/.ssh/id_forgejo_backup root@192.168.1.1 \
  'cat /backup/forgejo/forgejo-YYYYMMDD-HHMM.zip' > restore.zip
# 2) 解開
unzip restore.zip -d restore/
# 3) 停服務 → 還原資料目錄與設定 → 重啟
sudo systemctl stop forgejo
sudo cp -a restore/data/.  /var/lib/forgejo/data/      # 含原始 forgejo.db(+wal/shm)；勿用 forgejo-db.sql 重放
sudo cp -a restore/repos/. /var/lib/forgejo/data/forgejo-repositories/   # 依 dump 結構調整
sudo cp restore/app.ini    /etc/forgejo/app.ini
sudo chown -R git:git /var/lib/forgejo
sudo systemctl start forgejo
# 4) 驗證：能登入、repo 在、可 clone
```

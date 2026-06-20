# Forgejo 異地備份 — 第一步：M6 → R3S 本地副本（設計）

> 紀錄日期：2026-06-20
> 狀態：**設計已批准，待寫成 runbook**（尚未動工）
> 設計來源：`forgejo_home_cloud.md` 第 4 節（備份）的細化
> 相關：`R3S_hardening.md`（R3S 會多一把金鑰，需回填紀錄）、`forgejo_install_m6.md`（Forgejo 已裝好）

---

## 1. 範圍與定位（先講清楚）

每晚由 **M6** 產生**一份自包含的 Forgejo 備份**，複製到 **R3S**。
**這一步不加密、不碰 S3。**

⚠️ **這是「本地副本」，不是真正的「異地」。** R3S 和 M6 在**同一棟樓**，所以
M6 → R3S 是 3-2-1 裡的**第 2 份（local copy）**，用途是「手殘刪了 / DB 壞了」的**快速還原**。
真正抗火災 / 失竊的**異地第 3 份是 S3**，留待後續步驟（那時一起把 `age` 加密補上）。

先做 M6 → R3S 的理由：它最簡單 —— 兩臺都在內網、SSH 金鑰已通、不用碰 AWS。
先把「dump → 打包 → 推送 → 輪替刪舊 → 還原」這條管線跑通，之後加 S3 只是「多一個目的地 + 加密」。

### 本次查證的現況（2026-06-20）

| 項目 | 狀態 |
|---|---|
| Forgejo | 已在 M6 跑起來（v15.0.3，service `active`）|
| 資料目錄 | `/var/lib/forgejo`（屬 `git:git`）；DB 在 `data/forgejo.db`，目前 ~2.3MB，整包資料僅數 MB |
| SQLite | **WAL 模式**（有 `forgejo.db-wal`）→ 不能用 `cp` 直接複製資料夾（會不一致）；`forgejo dump` 會正確處理 |
| 設定檔 | `/etc/forgejo/app.ini`，權限 `640 root:git` → **`git` 使用者可讀**（群組 git）|
| SSH 到 R3S | 目前唯一可用路徑是 **`m6` 使用者 → `root@192.168.1.1`**（用 m6 的金鑰）|
| root@M6 → R3S | **不通**（`/root/.ssh` 沒金鑰、也沒 R3S 的 known_hosts）→ 本設計會補一把 root 專用金鑰 |
| R3S 空間 | `/` 有 108GB 可用，尚無 `/backup` 目錄 |

---

## 2. 要備份什麼 —— 兩樣東西，包成一包

`forgejo dump` **不包含** `app.ini`。但 `app.ini` 裡有 `SECRET_KEY` / `INTERNAL_TOKEN` /
`JWT_SECRET` —— 少了它們，還原回來的 DB 無法正常運作（加密欄位、session 會壞）。
**所以備份一定要「dump + app.ini」兩樣都收。**

每晚把這兩樣**包成一個檔**：`forgejo-YYYYMMDD-HHMM.tar.gz`（內含 `dump.zip` + `app.ini`）。

> 一個檔的好處：**輪替只看一個 mtime、還原只抓一個檔**。

| 內容物 | 來源 | 怎麼產生 |
|---|---|---|
| `dump.zip` | repos + SQLite + 附件 / 頭像 / LFS + 設定 | `sudo -u git forgejo dump -c /etc/forgejo/app.ini`（以 git 身分跑）|
| `app.ini` | 三把密鑰 + 設定 | root 直接讀 `/etc/forgejo/app.ini` 收進包裡 |

---

## 3. 誰來跑、怎麼到 R3S（執行身分與金鑰）

| 決策 | 值 | 理由 |
|---|---|---|
| **觸發** | M6 上的 **systemd service + timer**（不是 cron）| 對齊現有 `forgejo.service` 風格；日誌走 `journalctl`；`Persistent=true` 讓「M6 當下沒開機而錯過的那晚」於下次開機補跑（M6 是 SBC，可能會關機）|
| **執行身分** | service 以 **root** 跑 | 系統級備份本來就該是 root 的工作；root 讀得到所有檔（dump 結果 + app.ini）|
| **dump 子步驟** | 以 **git** 跑（`sudo -u git`）| 不讓 Forgejo 以 root 執行、也不在資料目錄留下 root 擁有的檔；root 只「讀」結果再推送 |
| **推送金鑰** | root **專用新金鑰** `/root/.ssh/id_forgejo_backup`，公鑰加到 R3S | 讓備份**獨立於 `m6` 登入帳號**；同時讓 Forgejo 的 `git` 服務帳號**完全沒有**進路由器的金鑰（萬一 Forgejo 被打，攻擊者不會順手拿到 R3S 的鑰匙）|

> 為什麼不直接用 `m6` 的現成金鑰跑：那會把「人類登入帳號」綁進自動化備份，且要靠 `sudo` 在
> git／m6 之間搬檔案、喬擁有權。改用 root 專用金鑰，整條流程乾淨、不混身分。

---

## 4. 每晚資料流

```
M6 root systemd timer（每晚 03:30）:
  1. sudo -u git forgejo dump -c app.ini   →  /var/lib/forgejo/backups/dump.zip   （git 寫、git 擁有）
  2. 打包 dump.zip + app.ini               →  /var/lib/forgejo/backups/forgejo-DATE.tar.gz   （root 讀兩者）
  3. rsync over SSH（root 專用金鑰）        →  R3S:/backup/forgejo/
  4. 在 R3S 上輪替：刪掉 mtime > 30 天 的 forgejo-*.tar.gz
  5. 清掉 M6 上的暫存（M6 本身就是第 1 份，不留 dump）
  6. 基本驗證：推完後確認 R3S 上該檔存在且大小 > 0；script 用 set -euo pipefail，失敗寫進 journal
```

- **暫存目錄**：`/var/lib/forgejo/backups/`（git 可寫、root 可讀）。推送後清空。
- **M6 不留備份**：M6 是 live（第 1 份），留 dump 只是多餘。

---

## 5. 存放位置與保留策略

| 位置 | 路徑 | 保留 | 機制 |
|---|---|---|---|
| R3S（本地副本） | `/backup/forgejo/` | **30 天** | M6 的 script 透過 SSH 跑 `find /backup/forgejo -name 'forgejo-*.tar.gz' -mtime +30 -delete` |

> 為什麼 30 天：每份 dump 現在約 3MB，30 份 ≈ 90MB，對 108GB 完全無感。
> 留一個月 = 萬一較晚才發現資料毀損，仍有一個月的歷史可回溯。日後資料變大再調。

---

## 6. 還原流程（並做一次演練）

設定完成後**實際演練一次**，確認備份「真的能還原」，而不只是「檔有複製過去」。

1. 從 R3S 取回某天的 `forgejo-YYYYMMDD-HHMM.tar.gz`。
2. 解開 → 得到 `dump.zip` + `app.ini`。
3. 依 Forgejo 文件還原 dump（停服務 → 還原資料目錄與 SQLite → 放回 `app.ini` → 重啟）。
4. 驗證：能登入、repo 內容在、clone 得回來。

> 細步驟在 runbook 寫成可照做的指令。

---

## 7. 取捨與風險（攤開講，不藏）

1. **Push + prune 模型**：M6 負責推送**也**負責刪 R3S 上的舊檔。所以**M6 若被完全入侵，
   攻擊者也能把 R3S 上的備份一起刪掉**。對「本地副本」這層可接受 —— 真正抗勒索的是之後
   S3 那層（用只能 `PutObject`、不能 delete 的 IAM 金鑰）。
   - **可選的當下強化**：R3S 的 `authorized_keys` 那行前面加
     `from="192.168.1.230",no-pty,no-port-forwarding`，把這把備份金鑰鎖到只能從 M6、且只能跑指令。
2. **R3S 會多一把金鑰**：這動到「已硬化的路由器」。完成後會**回填 `R3S_hardening.md`**，
   讓該文件保持正確（repo 慣例：R3S 的每次更動都要記）。
3. **執行分工**：照本 repo 慣例（見 `forgejo_install_m6.md`）——**Claude 寫 runbook、你（Willy）本人執行**，
   Claude 只負責之後 debug。

---

## 8. 已定預設值（runbook 直接採用，要改在 review 時講）

| 項目 | 值 |
|---|---|
| 執行時間 | 每晚 **03:30**（`OnCalendar=*-*-* 03:30:00`，`Persistent=true`）|
| 保留 | R3S **30 天** |
| 打包格式 | `forgejo-YYYYMMDD-HHMM.tar.gz`（內含 `dump.zip` + `app.ini`）|
| 執行身分 | systemd timer 以 **root**；dump 子步驟以 **git** |
| 推送金鑰 | root 專用 `/root/.ssh/id_forgejo_backup`，公鑰加到 R3S |
| R3S 路徑 | `/backup/forgejo/` |
| M6 暫存 | `/var/lib/forgejo/backups/`（推送後清空）|

---

## 9. 不在本次範圍（後續）

- **S3 異地備份（第 3 份）+ `age` 加密** —— 真正抗火災/失竊的那份；屆時：dump → `age` 加密 →
  推 R3S **和** S3；S3 用只寫 IAM 金鑰 + bucket lifecycle 過期。加密私鑰**離線保管、不可只存 M6**。
- LFS（目前 `LFS_START_SERVER = false`，dump 自然不含 LFS 物件；之後開 LFS 再一併納入備份驗證）。

---

## 10. 後續

設計批准後 → 用 writing-plans 寫成 runbook（systemd unit + timer、備份 script、root 金鑰佈署到 R3S 的步驟、
保留 cron 邏輯、還原演練、回填 `R3S_hardening.md`）。

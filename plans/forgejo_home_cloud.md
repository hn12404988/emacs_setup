# Forgejo 個人 Home Cloud + 異地備份（設計）

> 紀錄日期：2026-06-19
> 狀態：**設計已批准，待寫成實作計畫**（尚未動工）
> 相關：`R3S_hardening.md`（網路/防火牆）、`magit_forge.md`（Emacs forge 整合）、`tailscale_derp.md`

---

## 1. 目標與最終決策

要在家裡架一台**只在內網/tailnet 使用**的 Git 伺服器（Forgejo），給個人開發用。
不對外暴露任何 port。重點不是「對全世界開放」，而是「自己從哪都連得到、且資料安全」。

經過分析後鎖定兩個決策：

1. **Forgejo 跑在 M6**（不是 R3S）。
   理由 = **乾淨的角色分離**：
   - **R3S = 網路層**（路由 / 防火牆 / DNS / adblock）。網路設備要無聊、可預測，不堆應用程式。
   - **M6 = 應用 / 運算層**（已在跑 `local-llm`、`pieces-daemon`，未來加 Forgejo）。這才是 home cloud 主機。
   - M6 幾乎 24/7 開著、本來就在 tailnet 裡，所以「看 UI / 遠端用」零額外網路架構。
   - 把應用塞進路由器（R3S）長期只會越來越亂；放 M6 維持架構乾淨。

2. **韌性靠「異地備份」，不是靠把服務複製到路由器。**
   M6 是單點 → 用備份讓單點安全：M6 壞掉 / 被偷 / 火災都還能還原。
   採 **3-2-1**：live(M6) + 本地副本(R3S) + 異地(S3)。

> 為什麼這兩件事相關：一旦決定「集中在 M6」，備份就是讓這個決定安全的那塊拼圖。

---

## 2. 機器與網路現況（本次查證）

| 機器 | LAN IP | Tailscale IP | 角色 / 規格 | 在 tailnet? |
|---|---|---|---|---|
| **R3S**（NanoPi R3S）| 192.168.1.1 | 無 | OpenWrt 25.12.4 路由器；2GB RAM；**108GB ext4 可用**；已硬化（WAN DROP、只金鑰登入）| ❌ 不裝 |
| **M6**（NanoPi M6, RK3588 級）| 192.168.1.230 | 100.70.67.97 | 常開的 SBC 主機；已跑 local-llm / pieces-daemon | ✅ 在線 |
| **willy**（筆電 / macOS）| 浮動 | 100.98.14.82 | 會到處跑的工作筆電 | ✅ 在線 |

關鍵事實：
- 裝 Forgejo **不需要**在 WAN 或 LAN 開任何對外 port。
- R3S 接受 M6 的 SSH 金鑰登入（`root@192.168.1.1`，已驗證），所以 M6 能把備份推到 R3S。
- R3S 的 108GB 是 rootfs (`/dev/root`)，放本地備份空間充足。

---

## 3. 方案 A：Forgejo 跑在 M6

| 項目 | 設計 | 理由 |
|---|---|---|
| **安裝方式** | 原生 binary（linux-arm64）+ systemd 服務，以專屬使用者執行 | 單一 Go 執行檔；對齊現有 `local-llm.service` 模式；資料是純檔案，最好備份。（Docker 為替代，多一層隔離也多一層複雜，個人用不划算）|
| **資料庫** | **SQLite** | 單人使用足夠；DB 就是資料目錄裡一個檔案，備份極簡。（Postgres 是多人/高併發才需要）|
| **資料目錄** | 收斂成**單一目錄**（repos + SQLite + 設定 + LFS 全在內），例：`/var/lib/forgejo` | 「要備份的東西」收斂成一個資料夾 |
| **網路曝露** | 只綁 `127.0.0.1`，再用 `tailscale serve` 反代成 HTTPS | LAN/WAN 上**完全不開 port**；只有 tailnet 連得到；白送 HTTPS（登入頁不被瀏覽器嫌）|
| **對外網址 / `ROOT_URL`** | `https://nanopi-m6.<你的-tailnet>.ts.net` | 讓 forge 連結、clone 網址正確（需在 Tailscale admin console 啟用 HTTPS / MagicDNS）|
| **存取路徑** | M6 上 magit-forge 走 `http://localhost:3000`；筆電在外走上面的 ts.net 網址 | M6 不開機才連不到——但它幾乎都開著，可接受 |
| **Git 操作** | 走 HTTPS + access token（個人單人最簡單）| forge 本來就要在 `~/.authinfo` 放 API token（見 `magit_forge.md`）|

**安全模型**：不打開任何對外大門。信任邊界 = 「我管得好的 tailnet」（帳號開 2FA、ACL 收緊）。
大樓鄰居在 172.16.x 上看到的依舊是一片漆黑（R3S 的隱身不受影響）。

---

## 4. 方案 B：異地備份

### 4.1 為什麼用「每日獨立 dump」而不是 restic（重要）

想用 **S3 lifecycle 自動過期舊備份**（省心、控成本）——這個直覺對，但它跟 `restic` **不相容**：

- restic 的 repo 是「內容定址的 pack 檔」，**新快照會共用舊 pack**。
- 用 S3 lifecycle 按「檔案年齡」刪舊物件，會刪到**還被最新快照引用的 pack** → repo 壞掉、還原失敗。
- 所以 restic 的過期**只能**用它自己的 `restic forget --prune` 管，不能交給 S3 lifecycle。

**結論**：採「**每天一個獨立、自包含的 dump 檔**」。每個檔案互不依賴，S3 lifecycle 按年齡刪**完全安全**。
代價是每天整包（無增量去重），但個人 git server 資料小，配 lifecycle 過期，成本可忽略。
（未來資料若變大，再評估改 restic + 自管 prune。）

### 4.2 每晚備份資料流

```
M6 (cron, 每晚):
  forgejo dump  →  age 加密  →  forgejo-YYYYMMDD.zip.age   （dump 預設 zip，型別可調）
                                  ├──→  R3S:/backup/forgejo/    （本地副本，快速還原）
                                  └──→  s3://<bucket>/forgejo/  （異地，抗火災/失竊）
```

- **dump**：`forgejo dump` 把 repos + SQLite + 設定 + LFS 打包成一個一致的壓縮檔。
- **加密**：用 `age`（或 gpg）。
  > ⚠️ **解密金鑰不能只存在 M6**——否則 M6 死了備份也救不回。金鑰離線保管（密碼管理器 / 紙本），與備份分開存。

### 4.3 保留策略（建議值，可調）

| 位置 | 保留 | 機制 |
|---|---|---|
| **R3S（本地）** | 最近 14 天 | cron `find /backup/forgejo -mtime +14 -delete`（108GB 很夠）|
| **S3（異地）** | 90 天過期 | **S3 bucket lifecycle rule**（可選：30 天後轉 Standard-IA 省錢）|

### 4.4 S3 端的抗勒索設計（重要）

- M6 上傳用的 IAM 憑證**只給 `s3:PutObject`**（不給 delete / get）。
- 這樣即使 M6 被入侵/勒索，攻擊者**也刪不掉異地備份**；過期完全交給 bucket lifecycle 在伺服器端做。
- **不要**用需要 MFA 的人類 profile 跑 cron（MFA session 會過期）；建一個只能寫這個 bucket 的專用 IAM key。
- 可選加強：bucket 開 versioning + Object Lock，進一步防竄改。

### 4.5 還原流程（簡述）

1. 取得某天的 `forgejo-YYYYMMDD.tar.gz.age`（從 R3S 拿最快；或從 S3 下載）。
2. 用離線保管的金鑰 `age -d` 解密。
3. 依 Forgejo 文件還原 dump（解壓 → 還原資料目錄與 SQLite → 重啟服務）。

### 4.6 3-2-1 對照

| 副本 | 在哪 | 角色 |
|---|---|---|
| 1 | M6（live） | 正在用的 |
| 2 | R3S（同棟） | **快速**還原（手殘刪、DB 壞）|
| 3 | S3（異地） | **災難**還原（M6 + R3S 一起沒）|

> 注意：R3S 與 M6 在**同一棟**，所以 R3S 是「本地副本」不是「異地」。真正抗火災/失竊的是 S3。

---

## 5. 待確認 / 開放問題

- [ ] S3 供應商：預設 **AWS S3**（你已在用 AWS）；若要換 B2/別家，lifecycle 設定相同概念。
- [ ] 你的 tailnet 完整網域名稱（決定 `ROOT_URL` 的 `*.ts.net`）。
- [ ] Forgejo 資料目錄最終路徑（暫定 `/var/lib/forgejo`）。
- [ ] 保留天數最終值（暫定 R3S 14 天 / S3 90 天）。
- [ ] 加密工具：`age`（建議）或 `gpg`。

---

## 6. 後續

確認此設計後 → 寫成詳細實作計畫（安裝步驟、systemd unit、`tailscale serve` 指令、備份 script + cron、IAM policy、還原演練）。

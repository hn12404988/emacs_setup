# Self-hosted Forgejo CI for `pieces` — status & rollback

> Date: 2026-06-22 (built 2026-06-20, migrated to single-repo 2026-06-22).
> All on your own infra (M6 + your Forgejo). GitHub setup unchanged.

---

## 1. The idea: one repo, two CI systems side by side

`emacs_setup` holds **both** CI definitions; each platform reads only its own folder:
- `.github/workflows/build-binaries.yml` → **GitHub Actions** (GitHub-hosted runners). Unchanged.
- `.forgejo/workflows/build-binaries.yml` → **Forgejo Actions** (your self-hosted runners).

GitHub Actions ignores `.forgejo/`. Forgejo ignores `.github/` **when `.forgejo/` exists**
(verified on this instance: a push touching both ran only the `.forgejo` workflow). So there is
**no duplication and no dual-run** — the crate has a single source of truth in `emacs_setup`.

> Earlier there was a throwaway `willy/pieces-daemon` repo (a detached copy of the crate). It and the
> `willy/runner-smoke` smoke-test repo were **deleted** in this migration. Everything now lives in
> `emacs_setup`, mirrored to Forgejo as `willy/emacs_setup`.

---

## 2. What works (verified)

| Part | State |
|---|---|
| Linux build | ✅ `linux-arm64` on M6 (`m6-host` runner) → `pieces-linux-arm64`. |
| macOS build | ✅ `darwin-arm64` + `darwin-x86_64` (cross) on the Mac (`mac-willy` runner). |

Forgejo repo `willy/emacs_setup`, release `v0.2.0` (tag from `pieces-daemon/Cargo.toml`):
`pieces-darwin-arm64` · `pieces-darwin-x86_64` · `pieces-linux-arm64`, all built in-house.

---

## 3. What was created (inventory / rollback)

On **M6**:
- systemd service `/etc/systemd/system/forgejo-runner-host.service` — a **host-mode** Forgejo runner
  running as user `m6` (so it has `cargo`). Runner `m6-host`, labels `linux-arm64`, `ubuntu-latest`,
  config dir `/home/m6/fj-runner-host/`. (Pre-existing docker-mode `forgejo-runner.service` left alone.)
- Installed packages: `sqlite3`, `zstd`.

On **Forgejo**:
- Repo `willy/emacs_setup` (private) = mirror of the GitHub repo + the `.forgejo` pipeline.
- Repo Actions secret `RELEASE_TOKEN` (= the access token below).
- Personal access token named **`pipeline-setup`** (scope: all) for user `willy`.
- Release `willy/emacs_setup v0.2.0`.

In the **`emacs_setup` repo** (committed):
- `.forgejo/workflows/build-binaries.yml` — the Forgejo pipeline (builds from `pieces-daemon/`).
- `pieces-daemon/ci/upload-asset.sh` — publishes assets via the gitea-compatible API (no `gh`).

Git remote `forgejo` → `willy/emacs_setup` on the M6 instance (GitHub `origin` unchanged).

> A **host-mode** runner is required because Forgejo listens only on `127.0.0.1:3000` (unchanged).
> A docker-mode job runs in a container that can't reach the host's `127.0.0.1`, so checkout/upload
> would fail. The host-mode runner runs directly on M6, which can reach Forgejo.

---

## 4. Decisions & limits

- **Linux = `linux-arm64` only**, built natively on M6. It is a **glibc dynamic** binary (the GitHub
  workflow makes a **musl static** one). Different, but fine for your own machines.
- **`linux-x86_64` is a gap** — building x86_64 on the arm64 M6 needs `cross` + docker + qemu
  (slow/fragile). Left out; it still builds on GitHub, or add later with `cross` (docker is on M6).
- **macOS needs rustc ≥ 1.94** (the `sqlx 0.9` dependency). Keep the Mac's Rust current
  (`rustup update stable`). The Mac was on 1.88 once and the macOS job failed until updated to 1.96.

---

## 5. How to operate

- **Re-run:** push to `main` (`git push forgejo main`) or `workflow_dispatch` from the Forgejo UI
  (repo → Actions). The release tag follows `pieces-daemon/Cargo.toml` (`v0.2.0` now).
- **Mac runner is a `nohup` process** — it stops on reboot/sleep. After restarting the Mac, run
  `./forgejo-mac-runner.sh up`. `down` stops it, `status` shows state.
- **Watch status (on M6):**
  ```sh
  sudo -u git sqlite3 -readonly /var/lib/forgejo/data/forgejo.db \
    "SELECT r.id, j.name, j.status FROM action_run r JOIN action_run_job j ON j.run_id=r.id \
     WHERE r.repo_id=(SELECT id FROM repository WHERE lower_name='emacs_setup') ORDER BY r.id DESC;"
  # status: 1=success 2=failure 3=cancelled 5=waiting 6=running
  ```
- **Runner service:** `systemctl status forgejo-runner-host` · `journalctl -u forgejo-runner-host`.

---

## 6. Security notes

- The host-mode runner runs **any** workflow code in `willy/emacs_setup` **as user `m6`**. Fine for a
  private repo only you push to — do not point this runner at untrusted repos.
- The `pipeline-setup` token is **full scope** and also stored as the `RELEASE_TOKEN` secret. Consider
  rotating it to a repo-write-only token later (Forgejo → Settings → Applications).
- Forgejo is still bound to `127.0.0.1` only — not weakened.

---

## 7. Full rollback

```sh
# M6 runner
sudo systemctl disable --now forgejo-runner-host.service
sudo rm -f /etc/systemd/system/forgejo-runner-host.service
sudo systemctl daemon-reload
rm -rf /home/m6/fj-runner-host
# local repo
git remote remove forgejo
# (optional) drop the CI files: rm -r .forgejo pieces-daemon/ci
# Forgejo UI/API: delete runner 'm6-host', delete repo willy/emacs_setup, revoke 'pipeline-setup' token.
```

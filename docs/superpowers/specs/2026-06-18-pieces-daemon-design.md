# Pieces Daemon — Design

- **Date:** 2026-06-18
- **Status:** Approved (brainstorming complete) → ready for implementation plan
- **Plugin:** `mental-model-builder` (this repo, `hn12404988/emacs_setup`, public)
- **Skill changed:** `skills/pieces/SKILL.md`

## 1. Context and motivation

The current `pieces` skill is a working PoC. When the agent has a wall of complex
output, it writes the **complete** answer to a `/tmp` file, then delivers it to the
user **one small piece at a time** in the terminal. The user says **"next"** to move
forward.

The PoC proves the idea but the architecture is weak:

- Every "next" is a new conversation turn. Each turn **resends the whole conversation
  as input** to the model. With N pieces, the input context is paid N times. This is
  the real token sink — not the `/tmp` file.
- It is slow and clumsy: the user must type "next" again and again.

This redesign **keeps the soul of the skill** (one idea per piece, simple English,
complete-but-paced, user controls the pace) and **changes only the delivery**: a small
local Rust daemon receives the pieces as one JSON POST and renders a web page the user
browses at their own pace. One turn replaces the N "next" turns.

## 2. Goals and non-goals

**Goals**

- Kill the "next" loop. The agent sends all pieces in **one** POST; the user browses
  for free (no model calls while reading).
- Pieces are the **only** representation. The agent does **not** also emit a separate
  full copy (that would waste tokens).
- Heavy information → the daemon. Everything else (including questions) stays in the
  terminal, exactly as today.
- Ship as a single small static binary, downloaded by the skill from a GitHub Release.

**Non-goals (phase 1)**

- No interactivity in the web page. It is **read-only**. All interaction stays in the
  terminal (TUI).
- No browser-side answering of questions or editing of pieces. (Possible phase 2.)

## 3. Architecture

```
Coding agent ──POST /messages (curl)──► Rust daemon (axum) ──► SQLite
                                              │
Human ◄──── browser (read-only) ◄── GET pages (maud + comrak)
```

Three actors:

- **Coding agent** — writes only. Builds a JSON payload of pieces and POSTs it.
- **Daemon** — the one shared store. Receives payloads, persists them, serves the web UI.
- **Human (browser)** — reads only, at their own pace.

Questions never touch the daemon. They stay as normal terminal output.

## 4. Data model

Hierarchy: **Thread → Response → Pieces**.

- A **thread** groups all responses for one working context (one project / session).
- A **response** is one agent answer = one POST.
- A **piece** is one small idea inside a response.

### SQLite tables

```sql
CREATE TABLE thread (
  id          TEXT PRIMARY KEY,   -- readable slug, from the PIECES-THREAD header
  created_at  TEXT NOT NULL
);

CREATE TABLE response (
  id          TEXT PRIMARY KEY,   -- server-generated (uuid)
  thread_id   TEXT NOT NULL REFERENCES thread(id),
  title       TEXT NOT NULL,
  thumbnail   TEXT NOT NULL,      -- one-line preview, shown in lists
  created_at  TEXT NOT NULL
);

CREATE TABLE piece (
  response_id TEXT NOT NULL REFERENCES response(id),
  idx         INTEGER NOT NULL,   -- 1-based, used for referencing ("piece 3")
  heading     TEXT,               -- optional short title
  body_md     TEXT NOT NULL,      -- stored as Markdown, rendered to HTML at view time
  PRIMARY KEY (response_id, idx)
);
```

### POST payload schema

The thread comes from the header, so the body is small. The server stamps `id` and
`created_at`.

```jsonc
// POST /messages   header: PIECES-THREAD: <thread-id>
{
  "title": "How the reaper thread frees idle models",
  "thumbnail": "Lazy load + idle TTL, one lock for all",
  "pieces": [
    { "index": 1, "heading": "The problem", "body": "markdown… code blocks ok" },
    { "index": 2, "heading": "The lock",    "body": "…" }
  ]
}
```

A second POST with the same `PIECES-THREAD` **appends** a new response to the thread
(it does not replace earlier responses).

## 5. Daemon

### Endpoints

| Method | Path | Purpose |
| --- | --- | --- |
| `POST` | `/messages` | Header `PIECES-THREAD: <id>`. Create thread if new, insert response + pieces, return `{ "url": "…" }`. |
| `GET` | `/` | Thread list, most-recently-active first. |
| `GET` | `/t/{thread}` | That thread's responses, **DESC by time**. |
| `GET` | `/t/{thread}/r/{response}` | The piece reader. |
| `GET` | `/health` | Liveness (returns the daemon version). |

### Runtime

- Binds `127.0.0.1:8723` (default; overridable via `--port` / env). Localhost only.
- DB at `~/.local/share/pieces/pieces.db` (path via the `dirs` crate).
- Single static binary: maud templates and CSS are compiled in; nothing to ship beside it.
- Validates the JSON payload and returns a clear error on malformed input, so the agent
  can retry. (Code snippets inside pieces are full of quotes / backslashes / newlines —
  hand-written JSON will sometimes be invalid.)

### Stack (mirrors `atdd-cli`)

- `axum` — HTTP server.
- `comrak` — Markdown → GitHub-flavored HTML, `render.unsafe = false`.
- `maud` — compile-time HTML templates (embedded in the binary).
- `sqlx` (sqlite, runtime-tokio) — persistence + migrations.
- `tokio`, `serde` / `serde_json`, `chrono`, `dirs`, `uuid`, `anyhow` / `thiserror`,
  `tracing`.

## 6. Web UI (read-only)

Flow: **home (thread list)** → click → **response list** (newest first) → click →
**piece reader**.

The piece reader shows **one piece at a time**: its `heading` + the rendered `body`,
with `← prev / next →`, keyboard arrows, and a `piece N of M` marker. Flipping is
**client-side** (a few lines of JS, no page reload) so reading feels smooth.

Markdown `body` is rendered to HTML server-side by comrak and injected into the maud
template as `PreEscaped` — code blocks, inline code, and lists all render. Visual shell
mirrors atdd's existing dashboard look.

## 7. Thread identity

A plain file `<PROJECT_ROOT>/.claude/pieces-thread` holds the thread id.

- The agent reads it; if missing, it creates one as `<folder-name>-<6 hex>` (readable in
  the home list, still unique).
- The curl sends `PIECES-THREAD: $(cat .claude/pieces-thread)`.

Why a file and not a `settings.json` env var: editing `settings.json` mid-session does
**not** update the running session's environment (it is read at startup), so a freshly
written env var would be empty on the very first POST. A file works on the first POST,
survives across sessions, and avoids the risk of clobbering other keys when merging JSON.

## 8. Agent ↔ daemon protocol (the skill loop)

When pieces mode is on and the content is **heavy**:

1. **Ensure the daemon is up.** `curl -s localhost:8723/health`. If down: ensure the
   binary exists at `~/.local/share/pieces/bin/pieces-<version>` (download the
   version-pinned URL if missing, `chmod +x`), spawn it in the background
   (`nohup … --port 8723 >> ~/.local/share/pieces/daemon.log 2>&1 &`), poll `/health`.
2. **Ensure the thread id.** Read / create `.claude/pieces-thread`.
3. **Send.** Write the payload JSON to a temp file, then
   `curl --data @tmpfile -H "PIECES-THREAD: $T" -H "Content-Type: application/json"
   localhost:8723/messages`. (Temp file avoids shell-escaping a big JSON full of code.)
4. **Hand off.** Print the returned URL in one line.

Questions and short answers stay as normal terminal output. Pieces are numbered, so the
user refers to them in the terminal ("explain piece 3").

For **short, simple** answers, the agent answers directly in the terminal — it does not
push tiny answers through the daemon. Mode persists for the session until the user says
stop, same as today.

## 9. Distribution

The Rust source lives in this repo (e.g., `pieces-daemon/`, a Cargo project). The repo is
public, so Release binaries are downloadable by anyone. Two GitHub workflows in
`.github/workflows/`:

- **`release.yml` — Release.** On tag push `v*` → create the GitHub Release.
- **`build-binaries.yml` — Binaries.** On tag push `v*` **or** manual `workflow_dispatch`
  (required `version` input, so binaries can be re-attached / overwritten) → build 4
  targets → checksums → `gh release upload "v$version" … --clobber`.

Build matrix (mirrors `atdd-cli/.github/workflows/build-binaries.yml`):

| Target | Asset name |
| --- | --- |
| `x86_64-unknown-linux-musl` (via `cross`) | `pieces-linux-x86_64` |
| `aarch64-unknown-linux-musl` (via `cross`) | `pieces-linux-arm64` |
| `aarch64-apple-darwin` | `pieces-darwin-arm64` |
| `x86_64-apple-darwin` | `pieces-darwin-x86_64` |

Linux is built **static via musl + `cross`** so the binary runs on the RK3588 without
glibc trouble. A `SHA256SUMS` file is uploaded with the assets.

### Version contract

**Skill version == git tag == binary version.** `SKILL.md` hardcodes the download URL:

```
https://github.com/hn12404988/emacs_setup/releases/download/v<VERSION>/pieces-<os>-<arch>
```

The skill picks `<os>-<arch>` from `uname -s` / `uname -m`. Bumping `<VERSION>` in
`SKILL.md` (and `plugin.json`) is part of cutting a tag — they move together. Because the
URL is version-pinned (not "latest"), a binary update never breaks an older installed
skill.

## 10. The new SKILL.md

**Keeps** the soul: one idea per piece; simple English (reader is not a native speaker;
keep identifiers verbatim); complete-but-paced; mode persists for the session; only turns
on when the user asks; short answers stay direct.

**Changes** only delivery: POST the pieces to the daemon instead of writing `/tmp` and
waiting for repeated "next". This removes the token-heavy loop that motivated the rewrite.

## 11. Reference patterns to copy from `atdd-cli`

- Markdown render: `atdd-core/src/web/markdown.rs` — comrak `Options` + `markdown_to_html`
  + `maud::PreEscaped`, `render.unsafe = false`.
- maud templates / shared layout shell: `atdd-core/src/web/views.rs`.
- axum server, bind `127.0.0.1`, `/health`, background serve task:
  `atdd-core/src/daemon.rs` (≈ lines 352–395).
- sqlx pool setup: `atdd-core/src/store.rs` (≈ 10, 193, 244) and
  `atdd-core/src/master.rs` (≈ 12, 54, 92).
- Migrations: `atdd-core/src/migrate.rs`.
- Release/binary workflow: `atdd-cli/.github/workflows/build-binaries.yml` (manual
  `workflow_dispatch` + required `version`, version guard against `Cargo.toml`, 4 targets,
  `SHA256SUMS`, `gh release upload --clobber`).

## 12. Deferred / phase 2

- Browser interactivity (answer questions, mark pieces read) instead of read-only.
- Live update of an already-open page when a new response arrives (poll / SSE).
- Daemon-version vs payload-schema skew handling (phase 1 treats `/health` as liveness
  only; a running daemon of any version is reused).
- Security beyond localhost binding (e.g., on a shared host). Out of scope for phase 1.

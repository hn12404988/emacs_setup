# Pieces Daemon Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the `pieces` skill's `/tmp` file + repeated "next" loop with a small local Rust daemon that receives all pieces in one JSON POST and serves a read-only web page the user browses at their own pace.

**Architecture:** A single static Rust binary (`pieces`) runs an `axum` server on `127.0.0.1:8723`. The coding agent POSTs a payload of pieces (one POST per answer) with a `PIECES-THREAD` header; the daemon stores it in SQLite (`sqlx`) and serves three read-only pages (thread list → response list → piece reader) rendered with `maud`, with each piece's Markdown body rendered to HTML by `comrak`. The skill (`skills/pieces/SKILL.md`) is rewritten to drive this loop; two GitHub workflows build and attach version-pinned release binaries.

**Tech Stack:** Rust (edition 2021), `axum` 0.8, `sqlx` 0.9 (sqlite, runtime-tokio), `comrak` 0.52, `maud` 0.27, `tokio`, `serde`/`serde_json`, `chrono`, `dirs`, `uuid`, `clap`. Reference implementation patterns: `../atdd-cli/atdd-core/src/{web/markdown.rs,web/views.rs,daemon.rs,store.rs,migrate.rs}` and `../atdd-cli/.github/workflows/build-binaries.yml`.

## Global Constraints

- **Bind localhost only:** `127.0.0.1:8723` (default port; overridable via `--port`). Never `0.0.0.0`.
- **Single static binary:** maud templates and CSS compile into the binary. No separate asset files to ship.
- **Pieces are the only representation:** the agent does NOT also emit a separate full copy.
- **Read-only web UI (phase 1):** no interactivity in the browser. All interaction stays in the terminal.
- **Append semantics:** a second POST with the same `PIECES-THREAD` adds a new response; it never replaces earlier responses.
- **Version contract:** skill version == git tag == binary version. `SKILL.md` hardcodes a version-pinned URL: `https://github.com/hn12404988/emacs_setup/releases/download/v<VERSION>/pieces-<os>-<arch>`. First release version: `0.2.0`.
- **Build matrix:** `pieces-linux-x86_64`, `pieces-linux-arm64` (both static via musl + `cross`), `pieces-darwin-arm64`, `pieces-darwin-x86_64`.
- **Crate location:** `pieces-daemon/` at repo root; package + binary name `pieces`.
- **SQLite runtime queries** (no compile-time `DATABASE_URL`): use `sqlx::query` / `sqlx::query_as` with `.bind(...)`.

---

### Task 1: Scaffold the `pieces` crate with a `/health` endpoint

**Files:**
- Create: `pieces-daemon/Cargo.toml`
- Create: `pieces-daemon/src/main.rs`
- Create: `pieces-daemon/src/web/mod.rs`
- Modify: `.gitignore` (append `pieces-daemon/target/`)
- Test: in `pieces-daemon/src/web/mod.rs` (`#[cfg(test)]` module)

**Interfaces:**
- Produces: `web::router(state: web::AppState) -> axum::Router`; `web::AppState { store: crate::store::Store }` (added in Task 7 — for now `AppState` is an empty placeholder); `web::health` handler returning JSON `{"version": <pkg version>}`.

- [ ] **Step 1: Create `pieces-daemon/Cargo.toml`**

```toml
[package]
name = "pieces"
version = "0.2.0"
edition = "2021"
license = "UNLICENSED"

[[bin]]
name = "pieces"
path = "src/main.rs"

[dependencies]
anyhow = "1"
axum = "0.8"
chrono = { version = "0.4", default-features = false, features = ["clock"] }
clap = { version = "4", features = ["derive"] }
comrak = { version = "0.52", default-features = false }
dirs = "6"
maud = "0.27"
serde = { version = "1", features = ["derive"] }
serde_json = "1"
sqlx = { version = "0.9", default-features = false, features = ["runtime-tokio", "sqlite"] }
tokio = { version = "1", features = ["rt-multi-thread", "macros", "net", "signal"] }
uuid = { version = "1", features = ["v4"] }

[dev-dependencies]
tower = { version = "0.5", features = ["util"] }

[profile.release]
opt-level = "z"
lto = true
strip = true
```

- [ ] **Step 2: Append the target dir to `.gitignore`**

Add this line to `.gitignore`:

```
pieces-daemon/target/
```

- [ ] **Step 3: Write the failing test for `/health`**

Create `pieces-daemon/src/web/mod.rs`:

```rust
//! axum router + HTTP handlers for the pieces daemon.

use axum::{Json, Router, routing::get};

#[derive(Clone)]
pub struct AppState {}

pub fn router(state: AppState) -> Router {
    Router::new().route("/health", get(health)).with_state(state)
}

async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({ "version": env!("CARGO_PKG_VERSION") }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::Body;
    use axum::http::{Request, StatusCode};
    use tower::ServiceExt; // for `oneshot`

    #[tokio::test]
    async fn health_returns_version() {
        let app = router(AppState {});
        let resp = app
            .oneshot(Request::builder().uri("/health").body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(resp.status(), StatusCode::OK);
        let bytes = axum::body::to_bytes(resp.into_body(), usize::MAX).await.unwrap();
        let v: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
        assert_eq!(v["version"], env!("CARGO_PKG_VERSION"));
    }
}
```

- [ ] **Step 4: Write `main.rs` so the crate compiles**

Create `pieces-daemon/src/main.rs`:

```rust
mod web;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let app = web::router(web::AppState {});
    let listener = tokio::net::TcpListener::bind((std::net::Ipv4Addr::LOCALHOST, 8723)).await?;
    println!("pieces daemon on http://127.0.0.1:8723/");
    axum::serve(listener, app).await?;
    Ok(())
}
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `cd pieces-daemon && cargo test web::tests::health_returns_version`
Expected: PASS (`test result: ok. 1 passed`).

- [ ] **Step 6: Commit**

```bash
git add pieces-daemon/Cargo.toml pieces-daemon/Cargo.lock pieces-daemon/src .gitignore
git commit -m "feat(pieces-daemon): scaffold crate with /health endpoint"
```

---

### Task 2: Markdown rendering module

**Files:**
- Create: `pieces-daemon/src/web/markdown.rs`
- Modify: `pieces-daemon/src/web/mod.rs` (add `pub mod markdown;`)
- Test: in `pieces-daemon/src/web/markdown.rs`

**Interfaces:**
- Produces: `web::markdown::render(src: &str) -> maud::Markup` — GitHub-flavored Markdown → HTML, with raw embedded HTML neutralized (`render.unsafe = false`).

- [ ] **Step 1: Add the module declaration**

In `pieces-daemon/src/web/mod.rs`, add near the top (after the doc comment):

```rust
pub mod markdown;
```

- [ ] **Step 2: Write the failing test**

Create `pieces-daemon/src/web/markdown.rs`:

```rust
//! Markdown → HTML rendering for piece bodies. GitHub-flavored, server-side,
//! via comrak. Raw embedded HTML is neutralized (comrak's safe default).

use comrak::{Options, markdown_to_html};
use maud::{Markup, PreEscaped};

/// Render GFM `src` to HTML markup, safe to inject into a maud template.
pub fn render(src: &str) -> Markup {
    let mut options = Options::default();
    options.extension.table = true;
    options.extension.strikethrough = true;
    options.extension.tasklist = true;
    options.extension.autolink = true;
    options.render.r#unsafe = false;
    PreEscaped(markdown_to_html(src, &options))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn renders_heading_and_code() {
        let html = render("# Title\n\nUse `useState` here.").into_string();
        assert!(html.contains("<h1>"));
        assert!(html.contains("<code>useState</code>"));
    }

    #[test]
    fn raw_html_is_not_passed_through_as_live_tag() {
        let html = render("<script>alert(1)</script>").into_string();
        assert!(!html.contains("<script>"));
    }
}
```

- [ ] **Step 3: Run the tests to verify they pass**

Run: `cd pieces-daemon && cargo test web::markdown`
Expected: PASS (2 passed).

- [ ] **Step 4: Commit**

```bash
git add pieces-daemon/src/web
git commit -m "feat(pieces-daemon): comrak markdown rendering"
```

---

### Task 3: Store — schema + open

**Files:**
- Create: `pieces-daemon/src/store.rs`
- Create: `pieces-daemon/src/models.rs`
- Modify: `pieces-daemon/src/main.rs` (add `mod store;` and `mod models;`)
- Test: in `pieces-daemon/src/store.rs`

**Interfaces:**
- Produces: `store::Store` (Clone), `store::Store::open(db: &std::path::Path) -> anyhow::Result<Store>`, `store::Store::open_memory() -> anyhow::Result<Store>`. The three tables `thread`, `response`, `piece` exist after open.

- [ ] **Step 1: Add module declarations**

In `pieces-daemon/src/main.rs`, add below `mod web;`:

```rust
mod models;
mod store;
```

- [ ] **Step 2: Create the (empty for now) models module**

Create `pieces-daemon/src/models.rs`:

```rust
//! Data transfer objects (POST payload) and DB row structs.
```

- [ ] **Step 3: Write the failing test for `open_memory`**

Create `pieces-daemon/src/store.rs`:

```rust
//! SQLite-backed store. Runtime queries (no compile-time DATABASE_URL).
//! Pool with WAL + busy_timeout + foreign_keys, mirroring atdd-cli's store.

use anyhow::{Context, Result};
use sqlx::sqlite::{SqliteConnectOptions, SqliteJournalMode, SqlitePool, SqlitePoolOptions};
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;

const SCHEMA: &str = r#"
CREATE TABLE IF NOT EXISTS thread (
  id          TEXT PRIMARY KEY,
  created_at  TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS response (
  id          TEXT PRIMARY KEY,
  thread_id   TEXT NOT NULL REFERENCES thread(id),
  title       TEXT NOT NULL,
  thumbnail   TEXT NOT NULL,
  created_at  TEXT NOT NULL
);
CREATE TABLE IF NOT EXISTS piece (
  response_id TEXT NOT NULL REFERENCES response(id) ON DELETE CASCADE,
  idx         INTEGER NOT NULL,
  heading     TEXT,
  body_md     TEXT NOT NULL,
  PRIMARY KEY (response_id, idx)
);
"#;

#[derive(Clone)]
pub struct Store {
    pool: SqlitePool,
}

impl Store {
    pub async fn open(db: &Path) -> Result<Store> {
        if let Some(parent) = db.parent() {
            std::fs::create_dir_all(parent).ok();
        }
        let opts = SqliteConnectOptions::new()
            .filename(db)
            .create_if_missing(true)
            .journal_mode(SqliteJournalMode::Wal)
            .busy_timeout(Duration::from_secs(5))
            .foreign_keys(true);
        let pool = SqlitePoolOptions::new()
            .max_connections(8)
            .connect_with(opts)
            .await
            .context("opening sqlite store")?;
        sqlx::raw_sql(SCHEMA).execute(&pool).await.context("applying schema")?;
        Ok(Store { pool })
    }

    /// In-memory store for tests (single connection so it persists across calls).
    pub async fn open_memory() -> Result<Store> {
        let opts = SqliteConnectOptions::from_str("sqlite::memory:")?.foreign_keys(true);
        let pool = SqlitePoolOptions::new().max_connections(1).connect_with(opts).await?;
        sqlx::raw_sql(SCHEMA).execute(&pool).await?;
        Ok(Store { pool })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn open_memory_creates_three_tables() {
        let store = Store::open_memory().await.unwrap();
        let names: Vec<String> = sqlx::query_scalar(
            "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name",
        )
        .fetch_all(&store.pool)
        .await
        .unwrap();
        assert!(names.contains(&"thread".to_string()));
        assert!(names.contains(&"response".to_string()));
        assert!(names.contains(&"piece".to_string()));
    }
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `cd pieces-daemon && cargo test store::tests::open_memory_creates_three_tables`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add pieces-daemon/src
git commit -m "feat(pieces-daemon): sqlite store schema + open"
```

---

### Task 4: Store — insert a response (append) + list queries

**Files:**
- Modify: `pieces-daemon/src/models.rs` (add DTO + row structs)
- Modify: `pieces-daemon/src/store.rs` (add `insert_response`, `list_threads`, `list_responses`)
- Test: in `pieces-daemon/src/store.rs`

**Interfaces:**
- Consumes: `Store` from Task 3.
- Produces:
  - `models::PostBody { title: String, thumbnail: String, pieces: Vec<models::PieceIn> }` (Deserialize)
  - `models::PieceIn { index: i64, heading: Option<String>, body: String }` (Deserialize)
  - `models::ThreadRow { id: String, response_count: i64, last_activity: String }` (FromRow)
  - `models::ResponseRow { id: String, title: String, thumbnail: String, created_at: String }` (FromRow)
  - `Store::insert_response(&self, thread_id: &str, body: &PostBody) -> Result<String>` (returns the new response id)
  - `Store::list_threads(&self) -> Result<Vec<ThreadRow>>` (ordered by last activity DESC)
  - `Store::list_responses(&self, thread_id: &str) -> Result<Vec<ResponseRow>>` (DESC by time)

- [ ] **Step 1: Add the structs to `models.rs`**

Replace the contents of `pieces-daemon/src/models.rs` with:

```rust
//! Data transfer objects (POST payload) and DB row structs.

use serde::Deserialize;

/// The POST /messages body. Thread comes from the header; id + time are
/// server-stamped, so they are not in the payload.
#[derive(Debug, Deserialize)]
pub struct PostBody {
    pub title: String,
    pub thumbnail: String,
    pub pieces: Vec<PieceIn>,
}

#[derive(Debug, Deserialize)]
pub struct PieceIn {
    pub index: i64,
    #[serde(default)]
    pub heading: Option<String>,
    pub body: String,
}

#[derive(Debug, sqlx::FromRow)]
pub struct ThreadRow {
    pub id: String,
    pub response_count: i64,
    pub last_activity: String,
}

#[derive(Debug, sqlx::FromRow)]
pub struct ResponseRow {
    pub id: String,
    pub title: String,
    pub thumbnail: String,
    pub created_at: String,
}
```

- [ ] **Step 2: Write the failing test in `store.rs`**

Add to the `tests` module in `pieces-daemon/src/store.rs`:

```rust
    use crate::models::{PieceIn, PostBody};

    fn sample(title: &str) -> PostBody {
        PostBody {
            title: title.into(),
            thumbnail: "one-line preview".into(),
            pieces: vec![
                PieceIn { index: 1, heading: Some("A".into()), body: "alpha".into() },
                PieceIn { index: 2, heading: None, body: "beta".into() },
            ],
        }
    }

    #[tokio::test]
    async fn insert_then_list() {
        let store = Store::open_memory().await.unwrap();
        store.insert_response("proj-abc", &sample("first")).await.unwrap();
        store.insert_response("proj-abc", &sample("second")).await.unwrap();

        let threads = store.list_threads().await.unwrap();
        assert_eq!(threads.len(), 1);
        assert_eq!(threads[0].id, "proj-abc");
        assert_eq!(threads[0].response_count, 2);

        let responses = store.list_responses("proj-abc").await.unwrap();
        assert_eq!(responses.len(), 2);
        // DESC by time (ties broken by id) — both titles are present.
        let titles: Vec<&str> = responses.iter().map(|r| r.title.as_str()).collect();
        assert!(titles.contains(&"first") && titles.contains(&"second"));
    }
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `cd pieces-daemon && cargo test store::tests::insert_then_list`
Expected: FAIL (no method `insert_response` / `list_threads` / `list_responses`).

- [ ] **Step 4: Implement the methods**

Add to the `impl Store` block in `pieces-daemon/src/store.rs` (and add the imports shown):

```rust
// add near the top of store.rs:
use crate::models::{PostBody, ResponseRow, ThreadRow};
use chrono::Utc;
use uuid::Uuid;
```

```rust
    /// Append one response (with its pieces) to a thread, creating the thread
    /// row on first use. Returns the new response id.
    pub async fn insert_response(&self, thread_id: &str, body: &PostBody) -> Result<String> {
        let now = Utc::now().to_rfc3339();
        let rid = Uuid::new_v4().to_string();
        let mut tx = self.pool.begin().await?;
        sqlx::query("INSERT OR IGNORE INTO thread (id, created_at) VALUES (?, ?)")
            .bind(thread_id)
            .bind(&now)
            .execute(&mut *tx)
            .await?;
        sqlx::query(
            "INSERT INTO response (id, thread_id, title, thumbnail, created_at) VALUES (?, ?, ?, ?, ?)",
        )
        .bind(&rid)
        .bind(thread_id)
        .bind(&body.title)
        .bind(&body.thumbnail)
        .bind(&now)
        .execute(&mut *tx)
        .await?;
        for p in &body.pieces {
            sqlx::query("INSERT INTO piece (response_id, idx, heading, body_md) VALUES (?, ?, ?, ?)")
                .bind(&rid)
                .bind(p.index)
                .bind(&p.heading)
                .bind(&p.body)
                .execute(&mut *tx)
                .await?;
        }
        tx.commit().await?;
        Ok(rid)
    }

    pub async fn list_threads(&self) -> Result<Vec<ThreadRow>> {
        let rows = sqlx::query_as::<_, ThreadRow>(
            "SELECT t.id AS id, COUNT(r.id) AS response_count, \
             COALESCE(MAX(r.created_at), t.created_at) AS last_activity \
             FROM thread t LEFT JOIN response r ON r.thread_id = t.id \
             GROUP BY t.id ORDER BY last_activity DESC",
        )
        .fetch_all(&self.pool)
        .await?;
        Ok(rows)
    }

    pub async fn list_responses(&self, thread_id: &str) -> Result<Vec<ResponseRow>> {
        let rows = sqlx::query_as::<_, ResponseRow>(
            "SELECT id, title, thumbnail, created_at FROM response \
             WHERE thread_id = ? ORDER BY created_at DESC, id DESC",
        )
        .bind(thread_id)
        .fetch_all(&self.pool)
        .await?;
        Ok(rows)
    }
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `cd pieces-daemon && cargo test store::tests::insert_then_list`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add pieces-daemon/src
git commit -m "feat(pieces-daemon): insert_response (append) + thread/response list queries"
```

---

### Task 5: Store — read one response with its pieces

**Files:**
- Modify: `pieces-daemon/src/models.rs` (add `PieceRow`)
- Modify: `pieces-daemon/src/store.rs` (add `get_response`)
- Test: in `pieces-daemon/src/store.rs`

**Interfaces:**
- Consumes: `Store::insert_response`, `ResponseRow` from Task 4.
- Produces:
  - `models::PieceRow { idx: i64, heading: Option<String>, body_md: String }` (FromRow)
  - `Store::get_response(&self, thread_id: &str, response_id: &str) -> Result<Option<(ResponseRow, Vec<PieceRow>)>>` — pieces ordered by `idx` ascending.

- [ ] **Step 1: Add `PieceRow` to `models.rs`**

Append to `pieces-daemon/src/models.rs`:

```rust
#[derive(Debug, sqlx::FromRow)]
pub struct PieceRow {
    pub idx: i64,
    pub heading: Option<String>,
    pub body_md: String,
}
```

- [ ] **Step 2: Write the failing test in `store.rs`**

Add to the `tests` module in `pieces-daemon/src/store.rs`:

```rust
    #[tokio::test]
    async fn get_response_returns_ordered_pieces() {
        let store = Store::open_memory().await.unwrap();
        let rid = store.insert_response("proj-abc", &sample("only")).await.unwrap();

        let got = store.get_response("proj-abc", &rid).await.unwrap();
        let (resp, pieces) = got.expect("response should exist");
        assert_eq!(resp.title, "only");
        assert_eq!(pieces.len(), 2);
        assert_eq!(pieces[0].idx, 1);
        assert_eq!(pieces[0].heading.as_deref(), Some("A"));
        assert_eq!(pieces[1].idx, 2);
        assert_eq!(pieces[1].heading, None);

        let missing = store.get_response("proj-abc", "nope").await.unwrap();
        assert!(missing.is_none());
    }
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `cd pieces-daemon && cargo test store::tests::get_response_returns_ordered_pieces`
Expected: FAIL (no method `get_response`).

- [ ] **Step 4: Implement `get_response`**

Add `PieceRow` to the `use crate::models::...` import line in `store.rs`, then add to `impl Store`:

```rust
    pub async fn get_response(
        &self,
        thread_id: &str,
        response_id: &str,
    ) -> Result<Option<(ResponseRow, Vec<PieceRow>)>> {
        let resp = sqlx::query_as::<_, ResponseRow>(
            "SELECT id, title, thumbnail, created_at FROM response \
             WHERE id = ? AND thread_id = ?",
        )
        .bind(response_id)
        .bind(thread_id)
        .fetch_optional(&self.pool)
        .await?;
        let Some(resp) = resp else { return Ok(None) };
        let pieces = sqlx::query_as::<_, PieceRow>(
            "SELECT idx, heading, body_md FROM piece WHERE response_id = ? ORDER BY idx ASC",
        )
        .bind(response_id)
        .fetch_all(&self.pool)
        .await?;
        Ok(Some((resp, pieces)))
    }
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `cd pieces-daemon && cargo test store::tests::get_response_returns_ordered_pieces`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add pieces-daemon/src
git commit -m "feat(pieces-daemon): get_response with ordered pieces"
```

---

### Task 6: Web views (maud) — layout + three pages

**Files:**
- Create: `pieces-daemon/src/web/views.rs`
- Modify: `pieces-daemon/src/web/mod.rs` (add `pub mod views;`)
- Test: in `pieces-daemon/src/web/views.rs`

**Interfaces:**
- Consumes: `models::{ThreadRow, ResponseRow, PieceRow}`, `web::markdown::render`.
- Produces:
  - `web::views::thread_list_page(threads: &[ThreadRow]) -> maud::Markup`
  - `web::views::response_list_page(thread_id: &str, responses: &[ResponseRow]) -> maud::Markup`
  - `web::views::reader_page(thread_id: &str, response: &ResponseRow, pieces: &[PieceRow]) -> maud::Markup`

- [ ] **Step 1: Add the module declaration**

In `pieces-daemon/src/web/mod.rs`, add near the top:

```rust
pub mod views;
```

- [ ] **Step 2: Write the failing test**

Create `pieces-daemon/src/web/views.rs`:

```rust
//! maud templates: a shared `layout` shell wraps three read-only pages —
//! thread list → response list → piece reader (client-side prev/next).

use crate::models::{PieceRow, ResponseRow, ThreadRow};
use crate::web::markdown;
use maud::{DOCTYPE, Markup, PreEscaped, html};

const APP_CSS: &str = "\
body{font-family:system-ui,sans-serif;max-width:46rem;margin:2rem auto;padding:0 1rem;line-height:1.6;color:#222}\
a{color:#06c;text-decoration:none}a:hover{text-decoration:underline}\
header.top a.brand{font-weight:600;font-size:1.1rem}\
nav.crumbs{color:#888;margin:.5rem 0 1.5rem;font-size:.9rem}\
ul.list{list-style:none;padding:0}ul.list li{padding:.6rem 0;border-bottom:1px solid #eee}\
.thumb{color:#666;font-size:.9rem}\
.piece{display:none}.piece h2{margin-top:0}\
.markdown-body pre{background:#f6f8fa;padding:.8rem;border-radius:6px;overflow:auto}\
.markdown-body code{background:#f6f8fa;padding:.1rem .3rem;border-radius:4px}\
.markdown-body pre code{background:none;padding:0}\
.nav{margin-top:1.5rem;display:flex;gap:1rem}.nav button{font-size:1rem;padding:.4rem .9rem;cursor:pointer}\
.marker{color:#666;font-size:.9rem;margin-bottom:1rem}";

const READER_JS: &str = "(function(){\
var pieces=document.querySelectorAll('.piece');var total=pieces.length;var cur=0;\
var marker=document.getElementById('marker');\
function show(i){if(i<0||i>=total)return;pieces[cur].style.display='none';cur=i;\
pieces[cur].style.display='block';marker.textContent='piece '+(cur+1)+' of '+total;}\
var p=document.getElementById('prev');var n=document.getElementById('next');\
if(p)p.addEventListener('click',function(){show(cur-1);});\
if(n)n.addEventListener('click',function(){show(cur+1);});\
document.addEventListener('keydown',function(e){\
if(e.key==='ArrowLeft')show(cur-1);if(e.key==='ArrowRight')show(cur+1);});\
if(total>0)show(0);})();";

fn layout(page_title: &str, body: Markup) -> Markup {
    html! {
        (DOCTYPE)
        html lang="en" {
            head {
                meta charset="utf-8";
                meta name="viewport" content="width=device-width, initial-scale=1";
                title { (page_title) " — pieces" }
                style { (PreEscaped(APP_CSS)) }
            }
            body {
                header.top { a.brand href="/" { "pieces" } }
                main { (body) }
            }
        }
    }
}

pub fn thread_list_page(threads: &[ThreadRow]) -> Markup {
    let body = html! {
        h1 { "Threads" }
        @if threads.is_empty() {
            p.thumb { "No threads yet." }
        } @else {
            ul.list {
                @for t in threads {
                    li {
                        a href=(format!("/t/{}", t.id)) { (t.id) }
                        span.thumb { " — " (t.response_count) " responses" }
                    }
                }
            }
        }
    };
    layout("Threads", body)
}

pub fn response_list_page(thread_id: &str, responses: &[ResponseRow]) -> Markup {
    let body = html! {
        nav.crumbs { a href="/" { "all" } " › " (thread_id) }
        h1 { (thread_id) }
        ul.list {
            @for r in responses {
                li {
                    a href=(format!("/t/{}/r/{}", thread_id, r.id)) { (r.title) }
                    div.thumb { (r.thumbnail) }
                }
            }
        }
    };
    layout(thread_id, body)
}

pub fn reader_page(thread_id: &str, response: &ResponseRow, pieces: &[PieceRow]) -> Markup {
    let total = pieces.len();
    let body = html! {
        nav.crumbs {
            a href="/" { "all" } " › "
            a href=(format!("/t/{}", thread_id)) { (thread_id) } " › "
            (response.title)
        }
        div.marker id="marker" { "piece 1 of " (total) }
        @for p in pieces {
            div.piece {
                @if let Some(h) = &p.heading { h2 { (h) } }
                div.markdown-body { (markdown::render(&p.body_md)) }
            }
        }
        div.nav {
            button id="prev" { "← prev" }
            button id="next" { "next →" }
        }
        script { (PreEscaped(READER_JS)) }
    };
    layout(&response.title, body)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn resp() -> ResponseRow {
        ResponseRow {
            id: "r1".into(),
            title: "How it works".into(),
            thumbnail: "preview".into(),
            created_at: "2026-06-18T00:00:00Z".into(),
        }
    }

    #[test]
    fn thread_list_shows_ids_and_links() {
        let threads = vec![ThreadRow {
            id: "proj-abc".into(),
            response_count: 3,
            last_activity: "2026-06-18T00:00:00Z".into(),
        }];
        let html = thread_list_page(&threads).into_string();
        assert!(html.contains("proj-abc"));
        assert!(html.contains("href=\"/t/proj-abc\""));
        assert!(html.contains("3 responses"));
    }

    #[test]
    fn reader_renders_markdown_and_marker_and_nav() {
        let pieces = vec![
            PieceRow { idx: 1, heading: Some("The problem".into()), body_md: "# Big\n\ntext".into() },
            PieceRow { idx: 2, heading: None, body_md: "use `x`".into() },
        ];
        let html = reader_page("proj-abc", &resp(), &pieces).into_string();
        assert!(html.contains("The problem"));
        assert!(html.contains("<h1>Big</h1>"));
        assert!(html.contains("<code>x</code>"));
        assert!(html.contains("piece 1 of 2"));
        assert!(html.contains("id=\"prev\""));
        assert!(html.contains("id=\"next\""));
    }
}
```

- [ ] **Step 3: Run the tests to verify they pass**

Run: `cd pieces-daemon && cargo test web::views`
Expected: PASS (2 passed).

- [ ] **Step 4: Commit**

```bash
git add pieces-daemon/src/web
git commit -m "feat(pieces-daemon): maud views — thread list, response list, piece reader"
```

---

### Task 7: Web handlers + router wiring + POST /messages

**Files:**
- Modify: `pieces-daemon/src/web/mod.rs` (real `AppState`, handlers, full router)
- Test: in `pieces-daemon/src/web/mod.rs`

**Interfaces:**
- Consumes: `store::Store`, `models::PostBody`, `views::*`.
- Produces:
  - `web::AppState { store: crate::store::Store }` (Clone)
  - `web::router(state: AppState) -> Router` wiring: `POST /messages`, `GET /`, `GET /t/{thread}`, `GET /t/{thread}/r/{response}`, `GET /health`.
  - POST returns JSON `{"url": "/t/<thread>/r/<id>"}`. Missing `PIECES-THREAD` header → `400`.

- [ ] **Step 1: Replace `web/mod.rs` with the full router + handlers**

Replace the contents of `pieces-daemon/src/web/mod.rs` (keep the `pub mod markdown;` / `pub mod views;` lines) with:

```rust
//! axum router + HTTP handlers for the pieces daemon.

pub mod markdown;
pub mod views;

use crate::models::PostBody;
use crate::store::Store;
use axum::extract::{Path, State};
use axum::http::{HeaderMap, StatusCode};
use axum::response::Html;
use axum::routing::{get, post};
use axum::{Json, Router};

#[derive(Clone)]
pub struct AppState {
    pub store: Store,
}

pub fn router(state: AppState) -> Router {
    Router::new()
        .route("/", get(index))
        .route("/messages", post(post_messages))
        .route("/t/{thread}", get(thread_page))
        .route("/t/{thread}/r/{response}", get(reader))
        .route("/health", get(health))
        .with_state(state)
}

async fn health() -> Json<serde_json::Value> {
    Json(serde_json::json!({ "version": env!("CARGO_PKG_VERSION") }))
}

async fn post_messages(
    State(st): State<AppState>,
    headers: HeaderMap,
    Json(body): Json<PostBody>,
) -> Result<Json<serde_json::Value>, (StatusCode, String)> {
    let thread = headers
        .get("PIECES-THREAD")
        .and_then(|v| v.to_str().ok())
        .map(|s| s.to_string())
        .filter(|s| !s.is_empty())
        .ok_or((StatusCode::BAD_REQUEST, "missing PIECES-THREAD header".to_string()))?;
    let rid = st
        .store
        .insert_response(&thread, &body)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;
    let url = format!("/t/{}/r/{}", thread, rid);
    Ok(Json(serde_json::json!({ "url": url })))
}

async fn index(State(st): State<AppState>) -> Result<Html<String>, StatusCode> {
    let threads = st.store.list_threads().await.map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    Ok(Html(views::thread_list_page(&threads).into_string()))
}

async fn thread_page(
    State(st): State<AppState>,
    Path(thread): Path<String>,
) -> Result<Html<String>, StatusCode> {
    let responses = st
        .store
        .list_responses(&thread)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    Ok(Html(views::response_list_page(&thread, &responses).into_string()))
}

async fn reader(
    State(st): State<AppState>,
    Path((thread, response)): Path<(String, String)>,
) -> Result<Html<String>, StatusCode> {
    match st
        .store
        .get_response(&thread, &response)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?
    {
        Some((resp, pieces)) => Ok(Html(views::reader_page(&thread, &resp, &pieces).into_string())),
        None => Err(StatusCode::NOT_FOUND),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::Body;
    use axum::http::Request;
    use tower::ServiceExt;

    async fn body_string(resp: axum::response::Response) -> String {
        let bytes = axum::body::to_bytes(resp.into_body(), usize::MAX).await.unwrap();
        String::from_utf8(bytes.to_vec()).unwrap()
    }

    async fn test_app() -> Router {
        let store = Store::open_memory().await.unwrap();
        router(AppState { store })
    }

    #[tokio::test]
    async fn post_then_browse_roundtrip() {
        let app = test_app().await;
        let payload = r#"{"title":"T","thumbnail":"th","pieces":[{"index":1,"heading":"H","body":"# Big"}]}"#;

        let resp = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/messages")
                    .header("PIECES-THREAD", "proj-x")
                    .header("content-type", "application/json")
                    .body(Body::from(payload))
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(resp.status(), StatusCode::OK);
        let v: serde_json::Value = serde_json::from_str(&body_string(resp).await).unwrap();
        let url = v["url"].as_str().unwrap().to_string();
        assert!(url.starts_with("/t/proj-x/r/"));

        // home page lists the thread
        let home = app
            .clone()
            .oneshot(Request::builder().uri("/").body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert!(body_string(home).await.contains("proj-x"));

        // reader renders the piece's markdown
        let read = app
            .clone()
            .oneshot(Request::builder().uri(&url).body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(read.status(), StatusCode::OK);
        assert!(body_string(read).await.contains("<h1>Big</h1>"));
    }

    #[tokio::test]
    async fn post_without_header_is_400() {
        let app = test_app().await;
        let payload = r#"{"title":"T","thumbnail":"th","pieces":[]}"#;
        let resp = app
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/messages")
                    .header("content-type", "application/json")
                    .body(Body::from(payload))
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
    }
}
```

- [ ] **Step 2: Run the tests to verify they pass**

Run: `cd pieces-daemon && cargo test web::tests`
Expected: PASS (`post_then_browse_roundtrip`, `post_without_header_is_400`, `health_returns_version`).

- [ ] **Step 3: Commit**

```bash
git add pieces-daemon/src/web
git commit -m "feat(pieces-daemon): web handlers + POST /messages + router wiring"
```

---

### Task 8: `main.rs` wiring (CLI, data dir) + end-to-end smoke test

**Files:**
- Modify: `pieces-daemon/src/main.rs`
- Test: manual smoke test (documented commands + expected output)

**Interfaces:**
- Consumes: `web::router`, `web::AppState`, `store::Store`.
- Produces: a runnable binary `pieces` that accepts `--port <u16>` (default `8723`), opens the DB at `<data_dir>/pieces/pieces.db`, binds `127.0.0.1`, and serves.

- [ ] **Step 1: Replace `main.rs`**

Replace the contents of `pieces-daemon/src/main.rs` with:

```rust
mod models;
mod store;
mod web;

use clap::Parser;

#[derive(Parser)]
#[command(name = "pieces", version)]
struct Args {
    /// Port to bind on 127.0.0.1.
    #[arg(long, default_value_t = 8723)]
    port: u16,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let db = dirs::data_dir()
        .ok_or_else(|| anyhow::anyhow!("cannot resolve data dir"))?
        .join("pieces")
        .join("pieces.db");
    let store = store::Store::open(&db).await?;

    let app = web::router(web::AppState { store });
    let listener =
        tokio::net::TcpListener::bind((std::net::Ipv4Addr::LOCALHOST, args.port)).await?;
    let bound = listener.local_addr()?.port();
    println!("pieces daemon on http://127.0.0.1:{bound}/  (db: {})", db.display());
    axum::serve(listener, app).await?;
    Ok(())
}
```

- [ ] **Step 2: Build the release binary**

Run: `cd pieces-daemon && cargo build --release`
Expected: `Finished \`release\` profile` with no errors.

- [ ] **Step 3: Smoke test — start, health, POST, read**

Run (from `pieces-daemon/`):

```bash
./target/release/pieces --port 8799 &
PID=$!
sleep 1
curl -s http://127.0.0.1:8799/health
echo
curl -s -X POST http://127.0.0.1:8799/messages \
  -H "PIECES-THREAD: smoke-test" -H "Content-Type: application/json" \
  --data '{"title":"Smoke","thumbnail":"hi","pieces":[{"index":1,"heading":"One","body":"# Hello\n\n`code`"}]}'
echo
curl -s http://127.0.0.1:8799/ | grep -o "smoke-test"
kill $PID
```

Expected output (roughly):
- health: `{"version":"0.2.0"}`
- POST: `{"url":"/t/smoke-test/r/<uuid>"}`
- grep: `smoke-test`

- [ ] **Step 4: Commit**

```bash
git add pieces-daemon/src/main.rs
git commit -m "feat(pieces-daemon): main wiring — CLI port + data-dir db path"
```

---

### Task 9: Rewrite `skills/pieces/SKILL.md`

**Files:**
- Modify (full rewrite): `skills/pieces/SKILL.md`
- Verify: grep checks (no automated test — it's an instruction file)

**Interfaces:**
- Consumes: the daemon's HTTP contract from Tasks 7–8 (`/health`, `POST /messages` with `PIECES-THREAD` header, the `{url}` response) and the release URL from the Global Constraints.

- [ ] **Step 1: Replace `skills/pieces/SKILL.md` with the new content**

```markdown
---
name: pieces
description: Turn on "pieces mode". Instead of dumping a wall of complex information into the chat, send it to the local pieces daemon as a JSON payload of small pieces, then point the user at a localhost web page they browse at their own pace. Questions and short answers stay in the terminal. The mode stays on for the session until the user says stop. Do not turn this on yourself — only when the user asks.
disable-model-invocation: true
user-invocable: true
argument-hint: [optional: a file path, a topic, or nothing for the current context]
---

# /pieces — One Small Piece at a Time

## Purpose

When the user is buried under a wall of complex information — a long answer, a
list of questions, a dense file — this skill slows everything down. The complete
answer is broken into small **pieces**, each one idea, and shown on a local web
page the user flips through at their own pace.

This turns on a **mode**. It stays on for the rest of the session (until the user
says stop). While it is on, the agent does not dump walls of text into the chat.
Heavy information goes to the daemon; the chat stays light.

## When to use

- Right after the agent gives (or is about to give) a wall of complex context.
- The user turns it on themselves. **Never turn this on by yourself.**

## What it works on (the argument — anything)

- **No argument** → the most recent complex content already in the session.
- **A file path** → read the file; break its content into pieces.
- **A topic or term** → that topic, grounded in the current session and project.

## What goes where

- **Heavy information → the daemon** (as pieces). The user reads it on the web page.
- **Everything else → the terminal, unchanged.** Questions, short answers, and
  decisions stay in the terminal the normal way. If a question needs a lot of
  background, the background becomes pieces; only the short question stays in chat.

## Step 1 — Make sure the daemon is running

Run this in the shell. It checks health, downloads the version-pinned binary if
needed, and starts it. The version below is bound to this skill's release.

```sh
PIECES_VERSION=0.2.0
PIECES_PORT=8723
PIECES_DIR="$HOME/.local/share/pieces"
BIN="$PIECES_DIR/bin/pieces-$PIECES_VERSION"

if ! curl -fsS "http://127.0.0.1:$PIECES_PORT/health" >/dev/null 2>&1; then
  if [ ! -x "$BIN" ]; then
    os=$(uname -s); arch=$(uname -m)
    case "$os" in Linux) os=linux;; Darwin) os=darwin;; esac
    case "$arch" in x86_64|amd64) arch=x86_64;; aarch64|arm64) arch=arm64;; esac
    mkdir -p "$PIECES_DIR/bin"
    url="https://github.com/hn12404988/emacs_setup/releases/download/v$PIECES_VERSION/pieces-$os-$arch"
    curl -fsSL "$url" -o "$BIN" && chmod +x "$BIN"
  fi
  nohup "$BIN" --port "$PIECES_PORT" >> "$PIECES_DIR/daemon.log" 2>&1 &
  for i in $(seq 1 50); do
    curl -fsS "http://127.0.0.1:$PIECES_PORT/health" >/dev/null 2>&1 && break
    sleep 0.1
  done
fi
```

## Step 2 — Make sure this project has a thread id

```sh
THREAD_FILE=".claude/pieces-thread"
if [ ! -f "$THREAD_FILE" ]; then
  mkdir -p .claude
  slug=$(basename "$PWD" | tr -c 'a-zA-Z0-9' '-' | sed 's/-\{1,\}/-/g; s/^-//; s/-$//')
  rand=$(head -c3 /dev/urandom | od -An -tx1 | tr -d ' \n')
  echo "${slug}-${rand}" > "$THREAD_FILE"
fi
THREAD=$(cat "$THREAD_FILE")
```

## Step 3 — Build the payload and send it

Write the payload to a temp file (a big JSON full of code does not belong on the
command line), then POST it with the thread header.

The payload schema:

```jsonc
{
  "title": "short title of this whole answer",
  "thumbnail": "one-line preview, shown in lists",
  "pieces": [
    { "index": 1, "heading": "optional short heading", "body": "markdown — code blocks ok" },
    { "index": 2, "heading": "...", "body": "..." }
  ]
}
```

Send it:

```sh
# PAYLOAD_FILE is the temp file you just wrote the JSON to.
curl -fsS -X POST "http://127.0.0.1:$PIECES_PORT/messages" \
  -H "PIECES-THREAD: $THREAD" -H "Content-Type: application/json" \
  --data @"$PAYLOAD_FILE"
```

The response is `{"url":"/t/<thread>/r/<id>"}`. Tell the user the full URL in one
line, e.g. `http://127.0.0.1:8723/t/<thread>/r/<id>`.

If the POST fails with a validation error, the JSON was malformed (often
unescaped quotes or newlines inside a code body). Fix the JSON and resend.

## The piece-by-piece principle (unchanged)

- One piece = one idea, small enough to read in a few seconds. Short sentences,
  common words.
- Pieces are **numbered**. The user refers to them in the terminal ("explain
  piece 3").
- Pieces are the **only** version. Do **not** also write a separate full copy —
  that wastes tokens. The pieces carry every idea, spread out and told plainly.

## Short answers stay direct

For a yes/no, a one-liner, or a quick confirmation, answer directly in the
terminal. Do not push tiny answers through the daemon.

## The persisting mode

Once on, pieces mode stays on for the rest of the session. For any heavy answer,
send it to the daemon as pieces. The mode ends when the user says **"stop"**,
**"exit pieces mode"**, or similar.

## Language rule

**Output is always in simple English**, regardless of the source language. The
reader is not a native English speaker — short sentences, common words. Keep
proper nouns and identifiers verbatim: `useState`, `git rebase`, file paths,
function names, flags, numbers. Simple words — but do not drop ideas.

Note: this `SKILL.md` is in English because it is instructions for the assistant.
The rule above is about the output shown to the user.
```

- [ ] **Step 2: Verify the required pieces are present**

Run:

```bash
grep -c "127.0.0.1:\$PIECES_PORT/messages\|PIECES-THREAD\|releases/download/v\$PIECES_VERSION\|.claude/pieces-thread\|disable-model-invocation: true\|user-invocable: true" skills/pieces/SKILL.md
```

Expected: a count of `6` (every required element present).

- [ ] **Step 3: Commit**

```bash
git add skills/pieces/SKILL.md
git commit -m "feat(pieces): rewrite skill to drive the pieces daemon"
```

---

### Task 10: GitHub workflows — release + binaries

**Files:**
- Create: `.github/workflows/release.yml`
- Create: `.github/workflows/build-binaries.yml`
- Verify: `actionlint` if available, else documented manual review

**Interfaces:**
- Consumes: the crate at `pieces-daemon/` (binary `pieces`, version in `pieces-daemon/Cargo.toml`).
- Produces: a GitHub Release per `v*` tag, with 4 binaries + `SHA256SUMS` attached, downloadable at the URL the skill hardcodes.

- [ ] **Step 1: Create `.github/workflows/release.yml`**

```yaml
name: Release

on:
  push:
    tags: ['v*']

permissions:
  contents: write

jobs:
  create-release:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Create the release (idempotent)
        env:
          GH_TOKEN: ${{ github.token }}
        run: |
          tag="${GITHUB_REF#refs/tags/}"
          if gh release view "$tag" --repo "$GITHUB_REPOSITORY" >/dev/null 2>&1; then
            echo "release $tag already exists"
          else
            gh release create "$tag" --repo "$GITHUB_REPOSITORY" --title "$tag" --generate-notes
          fi
```

- [ ] **Step 2: Create `.github/workflows/build-binaries.yml`**

```yaml
name: Build & upload pieces binaries

# Triggered by a v* tag push (alongside the Release workflow) OR manually with a
# version (so binaries can be rebuilt/overwritten on an existing release).
on:
  push:
    tags: ['v*']
  workflow_dispatch:
    inputs:
      version:
        description: "Version to attach binaries to (no leading v, e.g. 0.2.0)"
        required: true
        type: string

permissions:
  contents: write

jobs:
  vars:
    runs-on: ubuntu-latest
    outputs:
      version: ${{ steps.v.outputs.version }}
    steps:
      - id: v
        run: |
          if [ "${{ github.event_name }}" = "workflow_dispatch" ]; then
            echo "version=${{ inputs.version }}" >> "$GITHUB_OUTPUT"
          else
            echo "version=${GITHUB_REF#refs/tags/v}" >> "$GITHUB_OUTPUT"
          fi

  build-linux:
    needs: vars
    runs-on: ubuntu-latest
    strategy:
      fail-fast: false
      matrix:
        include:
          - target: x86_64-unknown-linux-musl
            asset: pieces-linux-x86_64
          - target: aarch64-unknown-linux-musl
            asset: pieces-linux-arm64
    steps:
      - uses: actions/checkout@v4
      - name: Guard — Cargo version must equal the resolved version
        run: |
          cv="$(grep -m1 '^version' pieces-daemon/Cargo.toml | sed -E 's/.*"(.*)".*/\1/')"
          echo "Cargo: $cv ; resolved: ${{ needs.vars.outputs.version }}"
          [ "$cv" = "${{ needs.vars.outputs.version }}" ] || { echo "::error::pieces-daemon/Cargo.toml version ($cv) != ${{ needs.vars.outputs.version }} — bump Cargo first"; exit 1; }
      - uses: dtolnay/rust-toolchain@stable
      - uses: taiki-e/install-action@v2
        with:
          tool: cross
      - name: Build (musl static, via cross)
        working-directory: pieces-daemon
        run: cross build --release --bin pieces --target ${{ matrix.target }}
      - name: Stage asset
        run: |
          mkdir -p dist
          cp "pieces-daemon/target/${{ matrix.target }}/release/pieces" "dist/${{ matrix.asset }}"
      - uses: actions/upload-artifact@v4
        with:
          name: ${{ matrix.asset }}
          path: dist/${{ matrix.asset }}
          if-no-files-found: error

  build-macos:
    needs: vars
    runs-on: macos-14
    strategy:
      fail-fast: false
      matrix:
        include:
          - target: aarch64-apple-darwin
            asset: pieces-darwin-arm64
          - target: x86_64-apple-darwin
            asset: pieces-darwin-x86_64
    steps:
      - uses: actions/checkout@v4
      - uses: dtolnay/rust-toolchain@stable
        with:
          targets: ${{ matrix.target }}
      - name: Build
        working-directory: pieces-daemon
        run: cargo build --release --bin pieces --target ${{ matrix.target }}
      - name: Stage asset
        run: |
          mkdir -p dist
          cp "pieces-daemon/target/${{ matrix.target }}/release/pieces" "dist/${{ matrix.asset }}"
      - uses: actions/upload-artifact@v4
        with:
          name: ${{ matrix.asset }}
          path: dist/${{ matrix.asset }}
          if-no-files-found: error

  upload:
    needs: [vars, build-linux, build-macos]
    runs-on: ubuntu-latest
    steps:
      - uses: actions/download-artifact@v4
        with:
          path: dist
          merge-multiple: true
      - name: Checksums
        run: |
          cd dist
          sha256sum pieces-* > SHA256SUMS
          cat SHA256SUMS
      - name: Ensure release exists, then upload
        env:
          GH_TOKEN: ${{ github.token }}
        run: |
          tag="v${{ needs.vars.outputs.version }}"
          gh release view "$tag" --repo "$GITHUB_REPOSITORY" >/dev/null 2>&1 \
            || gh release create "$tag" --repo "$GITHUB_REPOSITORY" --title "$tag" --generate-notes
          cd dist
          gh release upload "$tag" \
            pieces-linux-x86_64 pieces-linux-arm64 pieces-darwin-arm64 pieces-darwin-x86_64 SHA256SUMS \
            --repo "$GITHUB_REPOSITORY" --clobber
          echo "Uploaded 4 binaries + SHA256SUMS to $GITHUB_REPOSITORY release $tag"
```

- [ ] **Step 3: Lint the workflows (if `actionlint` is available)**

Run: `actionlint .github/workflows/release.yml .github/workflows/build-binaries.yml`
Expected: no output (clean). If `actionlint` is not installed, manually confirm: both files are valid YAML, `permissions: contents: write` is set, the matrix has the 4 expected targets, and the upload step references all 4 asset names + `SHA256SUMS`.

- [ ] **Step 4: Commit**

```bash
git add .github/workflows
git commit -m "ci(pieces): release + binary build/upload workflows"
```

---

## Release procedure (after all tasks land)

This is the human step that ties version == tag == binary together. Not a code task.

1. Ensure `pieces-daemon/Cargo.toml` version, `SKILL.md`'s `PIECES_VERSION`, and the
   intended tag all read the same value (e.g. `0.2.0`).
2. (Optional) bump `.claude-plugin/plugin.json` version to match.
3. `git tag v0.2.0 && git push origin v0.2.0`. The Release workflow creates the
   release; the binaries workflow builds + attaches the 4 binaries.
4. Verify the asset URLs resolve, e.g.
   `https://github.com/hn12404988/emacs_setup/releases/download/v0.2.0/pieces-linux-arm64`.

---

## Self-Review

**Spec coverage** — every spec section maps to a task:
- §3 Architecture (axum daemon, SQLite, read-only browser) → Tasks 1, 3–8.
- §4 Data model (3 tables, POST payload schema, append) → Tasks 3 (schema), 4 (insert/append), 5 (read), DTO in 4.
- §5 Daemon endpoints + runtime (port 8723, localhost, data-dir db, validation) → Tasks 7 (endpoints + header validation; `Json` extractor auto-rejects malformed bodies with 422), 8 (port/data-dir).
- §6 Web UI (thread list → response list → reader, comrak, client-side prev/next) → Tasks 2, 6.
- §7 Thread identity (`.claude/pieces-thread` file) → Task 9 Step 2.
- §8 Agent ↔ daemon protocol (health/download/spawn, temp-file curl, URL handoff, TUI rules) → Task 9.
- §9 Distribution (two workflows, version pin, 4-target musl matrix, SHA256SUMS) → Task 10 + Release procedure.
- §10 New SKILL.md (soul preserved, delivery changed) → Task 9.

**Placeholder scan:** no `TBD`/`TODO`/"handle edge cases"; every code step shows full code; `<VERSION>` / `<os>` / `<thread>` appear only as documented runtime template values, not unfilled plan blanks.

**Type consistency:** `Store`, `AppState { store }`, `PostBody { title, thumbnail, pieces }`, `PieceIn { index, heading, body }`, `ThreadRow { id, response_count, last_activity }`, `ResponseRow { id, title, thumbnail, created_at }`, `PieceRow { idx, heading, body_md }`, `views::{thread_list_page, response_list_page, reader_page}`, `markdown::render`, `web::router` — names and signatures are used identically across Tasks 1–9.

**Deferred (spec §12, intentionally not in this plan):** browser interactivity, live page update, daemon-version/schema-skew handling, security beyond localhost.

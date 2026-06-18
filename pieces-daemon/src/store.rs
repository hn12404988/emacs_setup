//! SQLite-backed store. Runtime queries (no compile-time DATABASE_URL).
//! Pool with WAL + busy_timeout + foreign_keys, mirroring atdd-cli's store.

use anyhow::{Context, Result};
use crate::models::{PieceRow, PostBody, ResponseRow, ThreadRow};
use chrono::Utc;
use sqlx::sqlite::{SqliteConnectOptions, SqliteJournalMode, SqlitePool, SqlitePoolOptions};
use std::path::Path;
#[cfg(test)]
use std::str::FromStr;
use std::time::Duration;
use uuid::Uuid;

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
    #[cfg(test)]
    pub async fn open_memory() -> Result<Store> {
        let opts = SqliteConnectOptions::from_str("sqlite::memory:")?.foreign_keys(true);
        let pool = SqlitePoolOptions::new().max_connections(1).connect_with(opts).await?;
        sqlx::raw_sql(SCHEMA).execute(&pool).await?;
        Ok(Store { pool })
    }

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

    /// All threads, most-recently-active first.
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

    /// Fetch one response (scoped by both id and thread_id) and its pieces ordered by idx.
    /// Returns None if the response does not exist in that thread.
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

    /// All responses in a thread, newest first.
    pub async fn list_responses(&self, thread_id: &str) -> Result<Vec<ResponseRow>> {
        let rows = sqlx::query_as::<_, ResponseRow>(
            "SELECT id, title, thumbnail, created_at FROM response \
             WHERE thread_id = ? ORDER BY created_at DESC, ROWID DESC",
        )
        .bind(thread_id)
        .fetch_all(&self.pool)
        .await?;
        Ok(rows)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        // Newest first: "second" was inserted last, so it sorts ahead of "first".
        assert_eq!(responses[0].title, "second");
        assert_eq!(responses[1].title, "first");
    }

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

        // A valid response id under the WRONG thread must not leak.
        let wrong_thread = store.get_response("other-thread", &rid).await.unwrap();
        assert!(wrong_thread.is_none(), "should not leak across threads");
    }

    #[tokio::test]
    async fn get_response_orders_pieces_by_idx_not_insert_order() {
        let store = Store::open_memory().await.unwrap();
        // pieces vec is in REVERSE idx order on purpose.
        let body = PostBody {
            title: "ord".into(),
            thumbnail: "t".into(),
            pieces: vec![
                PieceIn { index: 2, heading: None, body: "two".into() },
                PieceIn { index: 1, heading: None, body: "one".into() },
            ],
        };
        let rid = store.insert_response("th", &body).await.unwrap();
        let (_resp, pieces) = store.get_response("th", &rid).await.unwrap().unwrap();
        assert_eq!(pieces.len(), 2);
        assert_eq!(pieces[0].idx, 1, "must be ordered by idx ASC, not insert order");
        assert_eq!(pieces[0].body_md, "one");
        assert_eq!(pieces[1].idx, 2);
    }

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

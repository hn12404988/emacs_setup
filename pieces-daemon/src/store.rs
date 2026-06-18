//! SQLite-backed store. Runtime queries (no compile-time DATABASE_URL).
//! Pool with WAL + busy_timeout + foreign_keys, mirroring atdd-cli's store.

use anyhow::{Context, Result};
use crate::models::{PostBody, ResponseRow, ThreadRow};
use chrono::Utc;
use sqlx::sqlite::{SqliteConnectOptions, SqliteJournalMode, SqlitePool, SqlitePoolOptions};
use std::path::Path;
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
        // DESC by time (ties broken by id) — both titles are present.
        let titles: Vec<&str> = responses.iter().map(|r| r.title.as_str()).collect();
        assert!(titles.contains(&"first") && titles.contains(&"second"));
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

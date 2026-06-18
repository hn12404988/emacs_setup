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

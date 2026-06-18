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

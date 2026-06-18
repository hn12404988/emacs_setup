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
    use axum::http::{Request, StatusCode};
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
    async fn health_returns_version() {
        let app = test_app().await;
        let resp = app
            .oneshot(Request::builder().uri("/health").body(Body::empty()).unwrap())
            .await
            .unwrap();
        assert_eq!(resp.status(), StatusCode::OK);
        let bytes = axum::body::to_bytes(resp.into_body(), usize::MAX).await.unwrap();
        let v: serde_json::Value = serde_json::from_slice(&bytes).unwrap();
        assert_eq!(v["version"], env!("CARGO_PKG_VERSION"));
    }

    #[tokio::test]
    async fn post_then_browse_roundtrip() {
        let app = test_app().await;
        let payload = r##"{"title":"T","thumbnail":"th","pieces":[{"index":1,"heading":"H","body":"# Big"}]}"##;

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

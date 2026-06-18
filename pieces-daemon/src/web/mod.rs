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

mod models;
mod store;
mod web;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let store = store::Store::open(std::path::Path::new("pieces.db")).await?;
    let app = web::router(web::AppState { store });
    let listener = tokio::net::TcpListener::bind((std::net::Ipv4Addr::LOCALHOST, 8723)).await?;
    println!("pieces daemon on http://127.0.0.1:8723/");
    axum::serve(listener, app).await?;
    Ok(())
}

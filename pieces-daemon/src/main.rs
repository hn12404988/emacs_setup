mod models;
mod store;
mod web;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let app = web::router(web::AppState {});
    let listener = tokio::net::TcpListener::bind((std::net::Ipv4Addr::LOCALHOST, 8723)).await?;
    println!("pieces daemon on http://127.0.0.1:8723/");
    axum::serve(listener, app).await?;
    Ok(())
}

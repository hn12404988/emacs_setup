mod models;
mod store;
mod web;

use clap::Parser;

#[derive(Parser)]
#[command(name = "pieces", version)]
struct Args {
    /// Port to bind on 0.0.0.0 (all interfaces).
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
        tokio::net::TcpListener::bind((std::net::Ipv4Addr::UNSPECIFIED, args.port)).await?;
    let bound = listener.local_addr()?.port();
    println!("pieces daemon on 0.0.0.0:{bound} (open http://127.0.0.1:{bound}/)  (db: {})", db.display());
    axum::serve(listener, app).await?;
    Ok(())
}

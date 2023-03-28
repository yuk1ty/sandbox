use std::net::SocketAddr;
use std::sync::Arc;

use axum::{routing::get, Extension, Router, Server};
use rust_sdk::providers::traits::FeatureProvider;
use rust_sdk::traits::Client;
use rust_sdk::{providers::NoopProvider, OpenFeatureClient};

const DEFAULT_MESSAGE: &'static str = "Hello!";
const NEW_WELCOME_MESSAGE: &'static str = "Hello, welcome to this OpenFeature-enabled website!";

#[tokio::main]
async fn main() {
    let client = Arc::new(OpenFeatureClient::new(
        "feature-flag".to_string(),
        NoopProvider::new(),
    ));
    let app = Router::new()
        .route("/hello", get(hello))
        .layer(Extension(client));
    let addr = SocketAddr::from(([127, 0, 0, 1], 8080));
    Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn hello(
    // The SDK only provides "NoopProvider" for now. A provider for flagd will be supplied in the future.
    // This Noop client can't get any value from flagd.
    Extension(feature_flag): Extension<Arc<OpenFeatureClient<NoopProvider>>>,
) -> &'static str {
    if dbg!(feature_flag
        .value(
            "welcome-message".to_string(),
            false,
            feature_flag.evaluation_context(),
        )
        .unwrap())
    {
        NEW_WELCOME_MESSAGE
    } else {
        DEFAULT_MESSAGE
    }
}

use poem_mcpserver::{McpServer, Tools, stdio::stdio, tool::Text};
use uuid::Uuid;

/// "UUIDv4 Generator"
struct UuidGenerator;

#[Tools]
impl UuidGenerator {
    /// Generate a new UUIDv4
    async fn generate_v4(&self) -> Text<String> {
        Text(Uuid::new_v4().to_string())
    }
}

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    stdio(McpServer::new().tools(UuidGenerator)).await
}

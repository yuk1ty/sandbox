use poem_mcpserver::{McpServer, Tools, stdio::stdio, tool::Text};
use uuid::Uuid;

struct UuidGenerator;

#[Tools]
impl UuidGenerator {
    async fn generate(&self) -> Text<String> {
        Text(Uuid::new_v4().to_string())
    }
}

#[tokio::main]
async fn main() -> Result<(), std::io::Error> {
    stdio(McpServer::new().tools(UuidGenerator)).await
}

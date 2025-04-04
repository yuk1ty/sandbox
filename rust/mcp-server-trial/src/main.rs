use poem_mcpserver::{McpServer, Tools, tool::Text};
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
    poem_mcpserver::stdio::stdio(McpServer::new().tools(UuidGenerator)).await
}

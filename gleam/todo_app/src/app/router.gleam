import app/handlers/health
import app/handlers/todos
import wisp.{type Request, type Response}

pub fn handle_request(req: Request) -> Response {
  case wisp.path_segments(req) {
    [] -> health.health_check(req)
    ["todos"] -> todos.get_todos(req)
    _ -> wisp.not_found()
  }
}

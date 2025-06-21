import app/handlers/health
import wisp.{type Request, type Response}

pub fn handle_request(req: Request) -> Response {
  case wisp.path_segments(req) {
    [] -> health.health_check(req)
    _ -> wisp.not_found()
  }
}

import gleam/http
import wisp.{type Request, type Response}

pub fn health_check(req: Request) -> Response {
  use <- wisp.require_method(req, http.Get)
  wisp.no_content()
}

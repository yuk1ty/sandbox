import wisp.{type Request, type Response}

pub fn health_check(_req: Request) -> Response {
  wisp.no_content()
}

import app/models/todos
import app/serde/todo_encoder
import gleam/http
import gleam/json
import gleam/option
import wisp.{type Request, type Response}

pub fn get_todos(req: Request) -> Response {
  use <- wisp.require_method(req, http.Get)
  let todos = [
    todos.TodoResponse(
      id: 1,
      title: "Buy groceries",
      description: option.Some("Don't forget milk"),
      done: False,
    ),
  ]
  todos
  |> todo_encoder.encode
  |> json.to_string_tree
  |> wisp.json_response(200)
}

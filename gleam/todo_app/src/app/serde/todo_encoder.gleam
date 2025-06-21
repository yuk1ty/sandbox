import app/models/todos
import gleam/json

pub fn encode(todos: List(todos.TodoResponse)) -> json.Json {
  todos
  |> json.array(fn(elem) {
    json.object([
      #("id", json.int(elem.id)),
      #("title", json.string(elem.title)),
      #("description", json.nullable(elem.description, of: json.string)),
      #("done", json.bool(elem.done)),
    ])
  })
}

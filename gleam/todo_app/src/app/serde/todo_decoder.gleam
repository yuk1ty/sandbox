import app/models/todos
import gleam/dynamic/decode

pub fn decode() -> decode.Decoder(todos.CreateTodoReq) {
  use title <- decode.field("title", decode.string)
  use description <- decode.field("description", decode.optional(decode.string))
  decode.success(todos.CreateTodoReq(title:, description:))
}

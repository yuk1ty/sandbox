import gleam/option

pub type CreateTodoReq {
  CreateTodoReq(title: String, description: option.Option(String))
}

pub type TodoResponse {
  TodoResponse(
    id: Int,
    title: String,
    description: option.Option(String),
    done: Bool,
  )
}

data class ListName(val name: String)

data class TodoList(val listName: ListName, val items: List<TodoItem>)

data class TodoItem(val description: String)
import org.http4k.core.HttpHandler
import org.http4k.server.Jetty
import org.http4k.server.asServer

fun main() {
    val items = listOf("write chapter", "insert code", "draw diagrams")
    val todoList = TodoList(ListName("book"), items.map(::TodoItem))
    val lists = mapOf(User("uberto") to listOf(todoList))

    val app: HttpHandler = Zettai(lists)
    app.asServer(Jetty(8080)).start()
}
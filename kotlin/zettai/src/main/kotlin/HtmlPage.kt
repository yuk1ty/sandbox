data class HtmlPage(val raw: String)

fun renderHtml(todoList: TodoList): HtmlPage =
    HtmlPage(
        """
    <html>
        <body>
            <h1>Zettai</h1>
            <h2>${todoList.listName.name}</h2>
            <table>
                <tbody>${renderItems(todoList.items)}</tbody>
            </table>
        </body>
    </html>
    """.trimIndent()
    )

private fun renderItems(items: List<TodoItem>) =
    items.map {
        """<tr><td>${it.description}</td></tr>""".trimIndent()
    }.joinToString("")
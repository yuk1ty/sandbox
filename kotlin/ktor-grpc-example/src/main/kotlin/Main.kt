package com.github.yuk1ty

import io.ktor.server.netty.*
import io.ktor.server.routing.*
import io.ktor.server.application.*
import io.ktor.http.*
import io.ktor.server.response.*
import io.ktor.server.engine.*

fun main(args: Array<String>) {
    embeddedServer(Netty, 8080) {
        routing {
            get("/") {
                call.respondText("Hello, world!", ContentType.Text.Html)
            }

            get("/tasks") {
                call.respond(listOf(Task("shopping", "Go shopping", false)))
            }
        }
    }.start(wait = true)
}
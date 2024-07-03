package com.github.yuk1ty

import kotlinx.serialization.Serializable

@Serializable
data class Task(
    val name: String,
    val description: String,
    val done: Boolean,
)

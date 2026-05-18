package com.github.yuk1ty.typeClassQuicklyExplained.step1

import arrow.core.EitherNel
import arrow.core.raise.either

// Validatableというインタフェースを実装する方式の場合、OOPでいう単一責任原則に反する可能性がある。
// DTOはvalidateの情報を持っている必要がないが、この実装の仕方だと持つことになるため、DTOの責務が増える。
// さらにいうと、ProtobufやOpenAPIをはじめとするスキーマ自動生成型のDTOの場合、拡張ができない。
// これを解決していくのが型クラスの主題の中心である。

data class CreatePortfolioDTO(val userId: String, val amount: Double) : Validatable<CreatePortfolioDTO> {
    override fun validate(): EitherNel<ValidationError, CreatePortfolioDTO> {
        TODO("Not yet implemented")
    }
}

data class ChangePortfolioDTO(val stock: String, val quantity: Int) : Validatable<ChangePortfolioDTO> {
    override fun validate(): EitherNel<ValidationError, ChangePortfolioDTO> {
        TODO("Not yet implemented")
    }
}

interface ValidationError

interface Validatable<T> {
    fun validate(): EitherNel<ValidationError, T>
}

fun <T: Validatable<T>> process(validatable: T) = either {
    val validated = validatable.validate().bind()
    TODO()
}

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
fun main() {

}
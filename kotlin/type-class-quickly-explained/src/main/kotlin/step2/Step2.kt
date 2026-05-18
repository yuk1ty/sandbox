package com.github.yuk1ty.typeClassQuicklyExplained.step2

import arrow.core.EitherNel
import arrow.core.raise.context.bind
import arrow.core.raise.context.either
import com.github.yuk1ty.typeClassQuicklyExplained.step1.ValidationError

// 一旦まずはValidatableのようにインタフェースで直接DTOに差し込む方式はやめ、validatorとvalidation対象を分離した。
// これを使うとProtobufやOpenAPIなどの自動生成なものに対してもvalidationを差し込めるようにはなる。
// が、今度はprocess関数に引数を一つ足す必要が出てくる。
// あとは、DTOから見ると結果的にまだvalidate関数がDTOに生えてるように見える（と記事に書かれているが、そうは思わないな）

data class CreatePortfolioDTO(val userId: String, val amount: Double)

data class ChangePortfolioDTO(val stock: String, val quantity: Int)

interface Validator<T> {
    fun validate(toValidate: T): EitherNel<ValidationError, T>
}

val createPortfolioDTOValidator =
    object : Validator<CreatePortfolioDTO> {
        override fun validate(toValidate: CreatePortfolioDTO): EitherNel<ValidationError, CreatePortfolioDTO> {
            TODO("Not yet implemented")
        }
    }

fun <T> process(
    toValidate: T,
    validator: Validator<T>
) = either {
    val validated = validator.validate(toValidate).bind()
    TODO()
}

fun main() {
    process(CreatePortfolioDTO("userId", 0.0), createPortfolioDTOValidator)
}
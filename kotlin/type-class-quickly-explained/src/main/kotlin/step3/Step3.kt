package com.github.yuk1ty.typeClassQuicklyExplained.step3

import arrow.core.EitherNel
import arrow.core.raise.context.bind
import arrow.core.raise.context.either
import com.github.yuk1ty.typeClassQuicklyExplained.step1.ValidationError

// ValidatorScopeというコンテキストを作り、そのコンテキストを読み込ませることで完全分離を果たした。
// Context Parametersを使うと引数にscopeをたらい回しにしなくても自動で伝播させてくれるようになる。
// withを使ってコンテキストを切って、その中であればprocess関数を呼び出せるという静的解析もつくようになる。

data class CreatePortfolioDTO(val userId: String, val amount: Double)

data class ChangePortfolioDTO(val stock: String, val quantity: Int)

interface ValidatorScope<T> {
    fun T.validate(): EitherNel<ValidationError, T>
}

val createPortfolioDTOValidatorScope =
    object : ValidatorScope<CreatePortfolioDTO> {
        override fun CreatePortfolioDTO.validate(): EitherNel<ValidationError, CreatePortfolioDTO> {
            TODO("Not yet implemented")
        }
    }

context(scope: ValidatorScope<T>)
fun <T> process(toValidate: T) = either {
    val validated: T = with(scope) { toValidate.validate() }.bind()
}

fun main() {
    with(createPortfolioDTOValidatorScope) {
        process(CreatePortfolioDTO("userId", 0.0))
    }
}
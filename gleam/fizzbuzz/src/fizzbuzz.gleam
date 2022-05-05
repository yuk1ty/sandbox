import gleam/io
import gleam/list
import gleam/int

pub fn do_fizzbuzz(n: Int) -> String {
  case n % 5, n % 3 {
    0, 0 -> "FizzBuzz"
    0, _ -> "Fizz"
    _, 0 -> "Buzz"
    _, _ -> int.to_string(n)
  }
}

pub fn main() {
  list.range(1, 101)
  |> list.map(do_fizzbuzz)
  |> io.debug
}
// Side note: the following one will be compile error.
// Gleam doesn't support wider case of guards (especially including arithmetic operations)?
// error: Syntax error
//    ┌─ src/fizzbuzz.gleam:32:14
//    │
// 32 │       n if n % 5 == 0 && n % 3 == 0 -> "FizzBuzz"
//    │              ^ I was not expecting this.
// Expected one of: "->"
// pub fn main() {
//   let fizzbuzz = list.range(1, 101) |> list.map(fn(n) {
//     case n {
//       n if n % 5 == 0 && n % 3 == 0 -> "FizzBuzz"
//       n if n % 5 -> "Fizz"
//       n if n % 3 -> "Buzz"
//       _ -> int.to_string(n)
//     }
//   })
//   io.debug(fizzbuzz)
// }

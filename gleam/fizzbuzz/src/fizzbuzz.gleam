import gleam/io
import gleam/list
import gleam/int

pub fn main() {
  let fizzbuzz = list.range(1, 101) |> list.map(fn(n) {
    case n % 5, n % 3 {
      0, 0 -> "FizzBuzz"
      0, _ -> "Fizz"
      _, 0 -> "Buzz"
      _, _ -> int.to_string(n)
    }
  })
  io.debug(fizzbuzz)
}

// Side note: the following one will be compile error.
// Gleam doesn't support wider case of guards (especially including arithmetic operations)?
// error: Syntax error
//    ┌─ src/fizzbuzz.gleam:22:14
//    │
// 22 │       n if n % 5 && n % 3 -> "FizzBuzz"
//    │              ^ I was not expecting this.
// 
// Expected one of: "->"
// pub fn main() {
//   let fizzbuzz = list.range(1, 101) |> list.map(fn(n) {
//     case n {
//       n if n % 5 && n % 3 -> "FizzBuzz"
//       n if n % 5 -> "Fizz"
//       n if n % 3 -> "Buzz"
//       _ -> int.to_string(n)
//     }
//   })
//   io.debug(fizzbuzz)
// }
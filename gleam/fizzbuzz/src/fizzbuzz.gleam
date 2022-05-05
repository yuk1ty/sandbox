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

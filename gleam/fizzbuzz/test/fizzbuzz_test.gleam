import gleeunit
import gleeunit/should
import fizzbuzz

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn fizzbuzz_test() {
  fizzbuzz.do_fizzbuzz(15)
  |> should.equal("FizzBuzz")
}

pub fn fizz_test() {
  fizzbuzz.do_fizzbuzz(5)
  |> should.equal("Fizz")
}

pub fn buzz_test() {
  fizzbuzz.do_fizzbuzz(3)
  |> should.equal("Buzz")
}

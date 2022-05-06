import gleam/io

pub type MyDatabaseError {
  InvalidQuery
  NetworkTimeout
}

fn insert(db_row) {
  // Ok or Error is included in prelude?
  Error(NetworkTimeout)
}

pub fn main() {
  try a = parse_int(1)
  try b = parse_int(2)
  try c = parse_int(3)
  io.println("Hello from error_handling!")
}

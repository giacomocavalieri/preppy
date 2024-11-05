import gleam/string

pub fn trim(string: String) -> String {
  trim_left(trim_right(string))
}

fn trim_left(string: String) -> String {
  case string {
    "\u{20}" <> rest | "\u{A0}" <> rest -> trim_left(rest)
    _ -> string.trim_left(string)
  }
}

fn trim_right(string: String) -> String {
  trim_left(string.reverse(string))
  |> string.reverse
}

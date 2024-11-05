import gleam/float
import gleam/int
import gleam/result
import gleam/string
import preppy/string_extra

pub fn lenient_parse(string: String) -> Result(Float, Nil) {
  let string = string_extra.trim(string) |> string.replace(each: ",", with: ".")
  case string.starts_with(string, ".") {
    True -> float.parse("0" <> string)
    False ->
      case string.ends_with(string, ".") {
        True -> float.parse(string <> "0")
        False ->
          case float.parse(string) {
            Ok(float) -> Ok(float)
            Error(_) -> int.parse(string) |> result.map(int.to_float)
          }
      }
  }
}

pub type PrettyOptions {
  HideDecimalPartIfZero
  KeepDecimalPart
}

pub fn to_pretty_string(float: Float, options: PrettyOptions) -> String {
  let integer_part = float.truncate(float)
  let decimal_digits = case integer_part {
    0 -> 2
    _ -> 1
  }

  let float_string = float.to_string(float)
  case string.split(float_string, on: ".") {
    [integer_part, decimals] ->
      case string.slice(decimals, 0, decimal_digits), options {
        "", _ -> integer_part

        "0", HideDecimalPartIfZero | "00", HideDecimalPartIfZero -> integer_part

        decimals, HideDecimalPartIfZero | decimals, KeepDecimalPart ->
          integer_part <> "." <> decimals
      }
    _ -> float_string
  }
}
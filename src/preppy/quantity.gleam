import frac.{type Fraction}
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string
import preppy/string_extra

pub type Quantity {
  FloatQuantity(Float)
  FractionQuantity(Fraction)
}

pub fn parse(string: String) -> Result(Quantity, Nil) {
  let string =
    string_extra.trim(string)
    |> string.replace(each: ",", with: ".")

  case float_lenient_parse(string) {
    Ok(float) -> Ok(FloatQuantity(float))
    Error(_) ->
      case fraction_parse(string) {
        Ok(fraction) -> Ok(FractionQuantity(fraction))
        Error(_) -> Error(Nil)
      }
  }
}

fn float_lenient_parse(string: String) -> Result(Float, Nil) {
  case string.starts_with(string, ".") {
    True -> float.parse("0" <> string)
    False ->
      case string.ends_with(string, ".") {
        True -> float.parse(string <> "0")
        False -> {
          use <- result.lazy_or(float.parse(string))
          int.parse(string) |> result.map(int.to_float)
        }
      }
  }
}

fn fraction_parse(string: String) -> Result(Fraction, Nil) {
  // Please forgive me for I have sinned.
  let string =
    string.replace(in: string, each: "⁄", with: "/")
    |> string.replace(each: "⁰", with: "0")
    |> string.replace(each: "¹", with: "1")
    |> string.replace(each: "²", with: "2")
    |> string.replace(each: "³", with: "3")
    |> string.replace(each: "⁴", with: "4")
    |> string.replace(each: "⁵", with: "5")
    |> string.replace(each: "⁶", with: "6")
    |> string.replace(each: "⁷", with: "7")
    |> string.replace(each: "⁸", with: "8")
    |> string.replace(each: "⁹", with: "9")
    |> string.replace(each: "₀", with: "0")
    |> string.replace(each: "₁", with: "1")
    |> string.replace(each: "₂", with: "2")
    |> string.replace(each: "₃", with: "3")
    |> string.replace(each: "₄", with: "4")
    |> string.replace(each: "₅", with: "5")
    |> string.replace(each: "₆", with: "6")
    |> string.replace(each: "₇", with: "7")
    |> string.replace(each: "₈", with: "8")
    |> string.replace(each: "₉", with: "9")

  case string.split(string, on: "/") {
    [numerator, denominator] -> {
      case string.split(numerator, on: " ") {
        [whole_part, numerator] -> {
          use whole_part <- result.try(int.parse(whole_part))
          use numerator <- result.try(int.parse(numerator))
          use denominator <- result.map(int.parse(denominator))
          frac.from_int(whole_part)
          |> frac.add(frac.new(numerator, denominator))
        }
        _ -> {
          use numerator <- result.try(int.parse(numerator))
          use denominator <- result.map(int.parse(denominator))
          frac.new(numerator, denominator)
        }
      }
    }
    _ -> Error(Nil)
  }
}

pub type PrettyOptions {
  HideDecimalPartIfZero
  KeepDecimalPart
}

pub fn multiply(one: Quantity, by other: Quantity) -> Quantity {
  case one, other {
    FloatQuantity(one), FloatQuantity(other) -> FloatQuantity(one *. other)
    FractionQuantity(one), FractionQuantity(other) ->
      FractionQuantity(frac.multiply(one, other))
    FloatQuantity(one), FractionQuantity(other) ->
      FloatQuantity(one *. frac.to_float(other))
    FractionQuantity(one), FloatQuantity(other) -> {
      let other = frac.approximate(other, 1000)
      FractionQuantity(frac.multiply(one, other))
    }
  }
}

pub fn to_float(quantity: Quantity) -> Float {
  case quantity {
    FractionQuantity(fraction) -> frac.to_float(fraction)
    FloatQuantity(float) -> float
  }
}

pub fn to_pretty_string(quantity: Quantity, options: PrettyOptions) -> String {
  case quantity {
    FloatQuantity(float) -> pretty_float(float, options)
    FractionQuantity(fraction) -> pretty_fraction(fraction, options)
  }
}

fn pretty_float(float: Float, options: PrettyOptions) -> String {
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

fn pretty_fraction(fraction: Fraction, options: PrettyOptions) -> String {
  let #(integer_part, remaining_fraction) = frac.to_mixed_numbers(fraction)
  let numerator = frac.numerator(remaining_fraction)
  let denominator = frac.denominator(remaining_fraction)

  let pretty_fraction = case denominator > 10 {
    True -> frac.to_float(remaining_fraction) |> pretty_float(options)
    False -> {
      let numerator = frac.numerator(remaining_fraction)
      int_to_sup(numerator) <> "⁄" <> int_to_sub(denominator)
    }
  }

  case integer_part, numerator {
    0, _ -> pretty_fraction
    _, 0 -> int.to_string(integer_part)
    _, _ -> int.to_string(integer_part) <> " " <> pretty_fraction
  }
}

fn int_to_sup(int: Int) -> String {
  let assert Ok(digits) = int.digits(int, 10)
  digits |> list.map(digit_to_sup) |> string.join(with: "")
}

fn digit_to_sup(digit: Int) -> String {
  case digit {
    0 -> "⁰"
    1 -> "¹"
    2 -> "²"
    3 -> "³"
    4 -> "⁴"
    5 -> "⁵"
    6 -> "⁶"
    7 -> "⁷"
    8 -> "⁸"
    9 -> "⁹"
    _ -> panic as "not a digit"
  }
}

fn int_to_sub(int: Int) -> String {
  let assert Ok(digits) = int.digits(int, 10)
  digits |> list.map(digit_to_sub) |> string.join(with: "")
}

fn digit_to_sub(digit: Int) -> String {
  case digit {
    0 -> "₀"
    1 -> "₁"
    2 -> "₂"
    3 -> "₃"
    4 -> "₄"
    5 -> "₅"
    6 -> "₆"
    7 -> "₇"
    8 -> "₈"
    9 -> "₉"
    _ -> panic as "not a digit"
  }
}

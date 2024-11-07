import decode/zero
import gleam/bool
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import preppy/array.{type Array}
import preppy/float_extra.{HideDecimalPartIfZero}
import preppy/string_extra

pub type Recipe {
  Recipe(name: String, ingredients: Array(Ingredient))
}

pub type Ingredient {
  Ingredient(name: String, quantity: Input(Float), converted: Input(Float))
}

pub type Input(a) {
  Empty
  Valid(raw: String, parsed: a)
  Invalid(raw: String)
  Computed(value: a)
}

// --- RECIPE TRANSFORMATIONS --------------------------------------------------

pub fn add_empty_ingredient(recipe: Recipe) -> Recipe {
  let ingredients =
    array.append(recipe.ingredients, Ingredient("", Empty, Empty))

  Recipe(..recipe, ingredients:)
}

pub fn empty_all_converted(recipe: Recipe) -> Recipe {
  map_ingredients(recipe, ingredient_set_converted(_, Empty))
}

pub fn set_converted(
  recipe: Recipe,
  index: Int,
  converted: Input(Float),
) -> Recipe {
  map_ingredient(recipe, index, ingredient_set_converted(_, converted))
}

pub fn empty_converted(recipe: Recipe, index: Int) -> Recipe {
  map_ingredient(recipe, index, ingredient_set_converted(_, Empty))
}

pub fn convert_all_ingredients(
  recipe: Recipe,
  using conversion_rate: Float,
) -> Recipe {
  map_ingredients(recipe, apply_conversion(_, conversion_rate))
}

pub fn convert_ingredient(
  recipe: Recipe,
  index: Int,
  using conversion_rate: Float,
) -> Recipe {
  map_ingredient(recipe, index, apply_conversion(_, conversion_rate))
}

pub fn map_ingredients(
  in recipe: Recipe,
  with fun: fn(Ingredient) -> Ingredient,
) -> Recipe {
  Recipe(..recipe, ingredients: array.map(recipe.ingredients, fun))
}

pub fn map_ingredient(
  in recipe: Recipe,
  at index: Int,
  with fun: fn(Ingredient) -> Ingredient,
) -> Recipe {
  let ingredients = array.update(recipe.ingredients, index, fun)
  Recipe(..recipe, ingredients:)
}

// --- INGREDIENT TRANSFORMATIONS ----------------------------------------------

fn ingredient_set_converted(
  ingredient: Ingredient,
  converted: Input(Float),
) -> Ingredient {
  Ingredient(..ingredient, converted:)
}

fn apply_conversion(
  ingredient: Ingredient,
  conversion_rate: Float,
) -> Ingredient {
  case ingredient.quantity {
    Empty | Invalid(_) -> ingredient
    Computed(value:) | Valid(raw: _, parsed: value) ->
      Ingredient(..ingredient, converted: Computed(value *. conversion_rate))
  }
}

// --- ENCODING/DECODING -------------------------------------------------------

pub fn to_string(recipe: Recipe) -> String {
  let Recipe(name:, ingredients:) = recipe

  let ingredients =
    array.to_list(ingredients)
    |> list.filter_map(fn(ingredient) {
      let Ingredient(name:, quantity:, converted: _) = ingredient

      // If the ingredient has an empty name we do not add it to the list
      // of ingredients to save.
      use <- bool.guard(when: string_extra.trim(name) == "", return: Error(Nil))

      // We accept the quantity might be missing, in that case we just don't
      // add it to the end of the line.
      let quantity = case quantity {
        Empty | Invalid(_) -> ""
        Computed(value:) | Valid(raw: _, parsed: value) ->
          ", " <> float_extra.to_pretty_string(value, HideDecimalPartIfZero)
      }
      Ok("- " <> name <> quantity)
    })
    |> string.join(with: "\n")

  name <> "\n\n# Ingredients\n" <> ingredients
}

pub fn from_string(string: String) -> Result(Recipe, Nil) {
  use <- result.lazy_or(from_markdown(string))
  use <- result.lazy_or(from_cooklang(string))
  from_json_ld(string)
}

// --- MARKDOWN PARSING --------------------------------------------------------

fn from_markdown(string: String) -> Result(Recipe, Nil) {
  case string.split(string, on: "\n\n# Ingredients\n") {
    [] | [_] | [_, _, _, ..] -> Error(Nil)
    [name, ingredients] -> {
      use ingredients <- result.try(
        string.split(ingredients, on: "\n")
        |> list.try_map(parse_markdown_ingredient),
      )
      Ok(Recipe(name:, ingredients: array.from_list(ingredients)))
    }
  }
}

fn parse_markdown_ingredient(string: String) -> Result(Ingredient, Nil) {
  let string = case string_extra.trim(string) {
    "-" <> string | string -> string_extra.trim(string)
  }

  case string.split(string, on: ",") {
    [] -> Error(Nil)
    [name] -> Ok(Ingredient(name:, quantity: Empty, converted: Empty))
    [name, quantity] -> {
      use quantity <- result.try(float_extra.lenient_parse(quantity))
      Ok(Ingredient(name:, quantity: Computed(quantity), converted: Empty))
    }
    _ -> Error(Nil)
  }
}

// --- COOKLANG PARSING --------------------------------------------------------
// Since we're just interested in the ingredients we don't have to support the
// full spec, we can just look for ingredients and parse those.
//

fn from_cooklang(string: String) {
  let ingredients = parse_cooklang_ingredients(string, string, 0, [])
  case ingredients {
    [] -> Error(Nil)
    _ -> Ok(Recipe(name: "Recipe", ingredients: array.from_list(ingredients)))
  }
}

fn parse_cooklang_ingredients(
  original: String,
  string: String,
  position: Int,
  acc: List(Ingredient),
) -> List(Ingredient) {
  case string {
    // We reached the end of the string, so we can return all the ingredients
    // we munched!
    "" -> list.reverse(acc)

    // A line that starts with `--` is a comment and we just ignore it.
    "--" <> rest -> {
      let #(rest, position) = consume_line(rest, position + 2)
      parse_cooklang_ingredients(original, rest, position, acc)
    }

    // We found the start of an ingredient! This is where things get tricky.
    "@" <> rest -> {
      let position = position + 1
      let #(rest, position, ingredient) =
        parse_cooklang_ingredient(original, rest, position, position, None)

      let acc = case ingredient {
        Some(ingredient) -> [ingredient, ..acc]
        None -> acc
      }

      parse_cooklang_ingredients(original, rest, position, acc)
    }

    // Otherwise we just keep munching the string until the end
    _ -> {
      let rest = drop_bytes(string, 1)
      parse_cooklang_ingredients(original, rest, position + 1, acc)
    }
  }
}

fn parse_cooklang_ingredient(
  original: String,
  string: String,
  position: Int,
  start: Int,
  size: Option(Int),
) {
  case string {
    "{" <> rest -> {
      let size = position - start
      case string.trim(slice_bytes(original, start, size)) {
        "" -> #(rest, position + 1, None)
        name -> {
          let #(rest, position, quantity, unit) =
            parse_quantity_unit(original, rest, position + 1, position + 1)

          let name = case unit {
            Some(unit) -> name <> " (" <> unit <> ")"
            None -> name
          }

          let ingredient = Ingredient(name:, quantity:, converted: Empty)
          #(rest, position, Some(ingredient))
        }
      }
    }

    // The ingredient is a single word one.
    "" | "@" <> _ | "#" <> _ | "~" <> _ -> {
      let size = case size {
        None -> position - start
        Some(size) -> size
      }
      let ingredient = case string.trim(slice_bytes(original, start, size)) {
        "" -> None
        name -> Some(Ingredient(name:, quantity: Empty, converted: Empty))
      }
      #(string, position, ingredient)
    }

    _ -> {
      // If the ingredient name is not over we just advance, but we cannot just
      // drop a byte and call it a day. We also need to check if the first word
      // after `@` is over (that is there's a whitespace or punctuation char)
      // and keep track of its size.
      //
      // We must do this since we won't know if the ingredient is a multiword or
      // single word one until we run into a new modifier or the open curly
      // bracket (then we know it's a multiword ingredient).
      //
      let byte = first_byte(string)
      let rest = drop_bytes(string, 1)
      let is_word_end = is_punctuation(byte) || is_whitespace(byte)
      let size = case is_word_end, size {
        False, _ -> size
        True, Some(_) -> size
        True, None -> Some(position - start)
      }
      parse_cooklang_ingredient(original, rest, position + 1, start, size)
    }
  }
}

fn parse_quantity_unit(
  original: String,
  string: String,
  start: Int,
  position: Int,
) -> #(String, Int, Input(Float), Option(String)) {
  case string {
    // We found the closing curly bracket, we take a slice of the quantity and
    // try to parse it.
    "}" <> rest -> {
      let quantity = slice_bytes(original, start, position - start)
      #(rest, position + 1, parse_to_input(quantity), None)
    }

    // Unclosed quantity, in that case we just return an empty one.
    "" -> #(string, position, Empty, None)

    // We found a `%` so we switch to unit parsing
    "%" <> rest -> {
      let quantity = slice_bytes(original, start, position - start)
      let #(rest, position, unit) =
        parse_unit(original, rest, position + 1, position + 1)
      #(rest, position, parse_to_input(quantity), unit)
    }

    _ -> {
      let rest = drop_bytes(string, 1)
      parse_quantity_unit(original, rest, start, position + 1)
    }
  }
}

fn parse_to_input(raw: String) -> Input(Float) {
  case string_extra.trim(raw) {
    "" -> Empty
    _ ->
      case float_extra.lenient_parse(raw) {
        Ok(parsed) -> Valid(raw:, parsed:)
        Error(_) -> Invalid(raw:)
      }
  }
}

fn parse_unit(original: String, string: String, start: Int, position: Int) {
  case string {
    "}" <> rest -> {
      let unit = string.trim(slice_bytes(original, start, position - start))
      let unit = case unit {
        "" -> None
        unit -> Some(unit)
      }
      #(rest, position + 1, unit)
    }

    _ -> {
      let rest = drop_bytes(string, 1)
      parse_unit(original, rest, start, position + 1)
    }
  }
}

fn consume_line(string: String, acc: Int) -> #(String, Int) {
  case string {
    "\n" <> rest -> #(rest, acc + 1)
    "" -> #(string, acc)
    _ -> consume_line(drop_bytes(string, 1), acc + 1)
  }
}

fn is_punctuation(string: String) -> Bool {
  string == ","
}

fn is_whitespace(string: String) -> Bool {
  string == " "
}

// --- JSON-LD PARSING ---------------------------------------------------------

/// This parses the [recipe structured data](https://developers.google.com/search/docs/appearance/structured-data/recipe)
/// from a json object.
///
pub fn from_json_ld(json: String) -> Result(Recipe, Nil) {
  let ingredients_decoder = zero.list(json_ingredient_decoder())
  let recipe_decoder = {
    use name <- zero.field("name", zero.string)
    use ingredients <- zero.field("recipeIngredient", ingredients_decoder)
    zero.success(Recipe(name:, ingredients: array.from_list(ingredients)))
  }

  json.decode(json, zero.run(_, recipe_decoder))
  |> result.nil_error
}

fn json_ingredient_decoder() -> zero.Decoder(Ingredient) {
  use ingredient <- zero.then(zero.string)
  zero.success(Ingredient(name: ingredient, quantity: Empty, converted: Empty))
}

// --- FFI HELPERS -------------------------------------------------------------

@external(javascript, "../preppy.ffi.mjs", "drop_bytes")
fn drop_bytes(from string: String, bytes n: Int) -> String

@external(javascript, "../preppy.ffi.mjs", "first_byte")
fn first_byte(string: String) -> String

@external(javascript, "../preppy.ffi.mjs", "slice_bytes")
fn slice_bytes(string: String, from: Int, size: Int) -> String

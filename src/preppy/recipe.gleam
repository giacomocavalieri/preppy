import decode/zero
import frac
import gleam/bool
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/regex
import gleam/result
import gleam/string
import preppy/array.{type Array}
import preppy/quantity.{
  type Quantity, FloatQuantity, FractionQuantity, HideDecimalPartIfZero,
}
import preppy/string_extra

pub type Recipe {
  Recipe(name: String, ingredients: Array(Ingredient))
}

pub type Ingredient {
  Ingredient(
    name: String,
    quantity: Input(Quantity),
    converted: Input(Quantity),
  )
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
  converted: Input(Quantity),
) -> Recipe {
  map_ingredient(recipe, index, ingredient_set_converted(_, converted))
}

pub fn empty_converted(recipe: Recipe, index: Int) -> Recipe {
  map_ingredient(recipe, index, ingredient_set_converted(_, Empty))
}

pub fn convert_all_ingredients(
  recipe: Recipe,
  using conversion_rate: Quantity,
) -> Recipe {
  map_ingredients(recipe, apply_conversion(_, conversion_rate))
}

pub fn convert_ingredient(
  recipe: Recipe,
  index: Int,
  using conversion_rate: Quantity,
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
  converted: Input(Quantity),
) -> Ingredient {
  Ingredient(..ingredient, converted:)
}

fn apply_conversion(
  ingredient: Ingredient,
  conversion_rate: Quantity,
) -> Ingredient {
  case ingredient.quantity {
    Empty | Invalid(_) -> ingredient
    Computed(value:) | Valid(raw: _, parsed: value) -> {
      let converted = quantity.multiply(value, by: conversion_rate)
      Ingredient(..ingredient, converted: Computed(converted))
    }
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
          ", " <> quantity.to_pretty_string(value, HideDecimalPartIfZero)
      }
      Ok("- " <> name <> quantity)
    })
    |> string.join(with: "\n")

  name <> "\n\n# Ingredients\n" <> ingredients
}

pub fn from_string(string: String) -> Result(Recipe, Nil) {
  use <- result.lazy_or(from_markdown(string))
  from_cooklang(string)
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
      use quantity <- result.try(quantity.parse(quantity))
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
) -> #(String, Int, Input(Quantity), Option(String)) {
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

fn parse_to_input(raw: String) -> Input(Quantity) {
  case string_extra.trim(raw) {
    "" -> Empty
    _ ->
      case quantity.parse(raw) {
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
  case json.decode(json, zero.run(_, json_ld_decoder())) {
    Error(_) -> Error(Nil)
    Ok(SimpleRecipe(OtherNode)) -> Error(Nil)
    Ok(SimpleRecipe(RecipeNode(recipe))) -> Ok(recipe)
    Ok(GraphRecipe(nodes)) ->
      list.find_map(nodes, fn(node) {
        case node {
          RecipeNode(recipe) -> Ok(recipe)
          OtherNode -> Error(Nil)
        }
      })
  }
}

type JsonLdRecipe {
  SimpleRecipe(node: GraphNode)
  GraphRecipe(nodes: List(GraphNode))
}

type GraphNode {
  RecipeNode(Recipe)
  OtherNode
}

fn json_ld_decoder() -> zero.Decoder(JsonLdRecipe) {
  zero.one_of(
    zero.at(["@graph"], graph_decoder())
      |> zero.map(GraphRecipe),
    [
      recipe_node_decoder()
      |> zero.map(SimpleRecipe),
    ],
  )
}

fn graph_decoder() -> zero.Decoder(List(GraphNode)) {
  zero.list(zero.one_of(recipe_node_decoder(), [zero.success(OtherNode)]))
}

fn recipe_node_decoder() -> zero.Decoder(GraphNode) {
  use type_ <- zero.field("@type", zero.string)
  case type_ {
    "Recipe" -> zero.map(recipe_decoder(), RecipeNode)
    _ -> zero.failure(OtherNode, "recipe node")
  }
}

fn recipe_decoder() -> zero.Decoder(Recipe) {
  let ingredients = zero.list(ingredient_decoder())

  use name <- zero.field("name", zero.string)
  use ingredients <- zero.field("recipeIngredient", ingredients)
  let ingredients = array.from_list(ingredients)
  zero.success(Recipe(name:, ingredients:))
}

fn ingredient_decoder() -> zero.Decoder(Ingredient) {
  use ingredient <- zero.then(zero.string)
  zero.success(parse_ingredient(ingredient))
}

fn parse_ingredient(ingredient: String) -> Ingredient {
  let quantity =
    ingredient
    |> split_words
    |> extract_quantity

  case quantity {
    Error(Nil) ->
      Ingredient(name: ingredient, quantity: Empty, converted: Empty)

    Ok(#(quantity, None, name)) ->
      Ingredient(name:, quantity: Computed(quantity), converted: Empty)

    Ok(#(quantity, Some(unit), name)) -> {
      let name = name <> " (" <> unit <> ")"
      Ingredient(name:, quantity: Computed(quantity), converted: Empty)
    }
  }
}

type ThisOrThat(a, b) {
  This(a)
  That(b)
}

fn split_words(string: String) -> List(ThisOrThat(Quantity, String)) {
  let fraction_regex = "[0-9]+/[0-9]+"
  let float_regex = "(([0-9]*[\\.,])?[0-9]+)"
  let number_regex = fraction_regex <> "|" <> float_regex
  let assert Ok(regex) = regex.from_string(number_regex <> "|\\w+")

  use match <- list.map(regex.scan(regex, string))
  case quantity.parse(match.content) {
    Ok(quantity) -> This(quantity)
    Error(_) -> That(match.content)
  }
}

fn extract_quantity(words: List(ThisOrThat(Quantity, String))) {
  let is_word = fn(word) {
    case word {
      This(_) -> False
      That(_) -> True
    }
  }

  let to_string = fn(word) {
    case word {
      That(string) -> string
      This(_) -> ""
    }
  }

  // We start by dropping all words preceding a quantity
  let #(preceding_quantity, words) = list.split_while(words, is_word)
  let ingredient =
    preceding_quantity
    |> list.map(to_string)
    |> string.join(with: " ")

  case words {
    [] -> Error(Nil)
    [This(quantity)] -> Ok(#(quantity, None, ingredient))
    [This(quantity), That(maybe_unit)] ->
      Ok(#(quantity, Some(maybe_unit), ingredient))

    [This(FloatQuantity(whole)), This(FractionQuantity(rest))] -> {
      let frac =
        frac.approximate(whole, 1000)
        |> frac.add(rest)
        |> FractionQuantity

      Ok(#(frac, None, ingredient))
    }

    [This(FloatQuantity(whole)), This(FractionQuantity(rest)), That(maybe_unit)] -> {
      let frac =
        frac.approximate(whole, 1000)
        |> frac.add(rest)
        |> FractionQuantity

      Ok(#(frac, Some(maybe_unit), ingredient))
    }

    _ -> Error(Nil)
  }
}

// --- FFI HELPERS -------------------------------------------------------------

@external(javascript, "../preppy.ffi.mjs", "drop_bytes")
fn drop_bytes(from string: String, bytes n: Int) -> String

@external(javascript, "../preppy.ffi.mjs", "first_byte")
fn first_byte(string: String) -> String

@external(javascript, "../preppy.ffi.mjs", "slice_bytes")
fn slice_bytes(string: String, from: Int, size: Int) -> String

import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import preppy/array.{type Array}
import preppy/float_extra.{HideDecimalPartIfZero}
import preppy/string_extra

pub type Recipe {
  Recipe(title: String, ingredients: Array(Ingredient))
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

// --- RECIPE STRING ENCODING/DECODING -----------------------------------------

pub fn to_string(recipe: Recipe) -> String {
  let Recipe(title:, ingredients:) = recipe

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

  title <> "\n\n# Ingredients\n" <> ingredients
}

pub fn from_string(string: String) -> Result(Recipe, Nil) {
  case string.split(string, on: "\n\n# Ingredients\n") {
    [] | [_] | [_, _, _, ..] -> Error(Nil)
    [title, ingredients] -> {
      use ingredients <- result.try(
        string.split(ingredients, on: "\n")
        |> list.try_map(ingredient_from_string),
      )
      Ok(Recipe(title:, ingredients: array.from_list(ingredients)))
    }
  }
}

fn ingredient_from_string(string: String) -> Result(Ingredient, Nil) {
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

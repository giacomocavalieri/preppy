import decode/zero
import gleam/dynamic.{type Dynamic}
import gleam/fetch
import gleam/http/request.{type Request}
import gleam/javascript/promise
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import lustre
import lustre/attribute.{type Attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/event
import preppy/array
import preppy/icon
import preppy/quantity.{
  type PrettyOptions, type Quantity, FloatQuantity, HideDecimalPartIfZero,
  KeepDecimalPart,
}
import preppy/recipe.{
  type Ingredient, type Input, type Recipe, Computed, Empty, Ingredient, Invalid,
  Recipe, Valid,
}
import preppy/string_extra

const proxy_server = "https://preppy-server.fly.dev"

const recipe_localstore_key = "recipe"

pub fn main() {
  let app = lustre.application(init, update, view)

  let clipboard_capabilities = case is_ios() {
    False -> CanUseClipboard
    True -> CannotUseClipboard
  }

  // Before starting the application we try and read any recipe that might have
  // been saved in the local store, otherwise we just use a placeholder recipe.
  let recipe =
    get_item(recipe_localstore_key)
    |> recipe.from_string
    |> result.lazy_unwrap(placeholder_recipe)

  let initial_state = #(recipe, clipboard_capabilities)

  let assert Ok(_) = lustre.start(app, "#app", initial_state)
  Nil
}

// --- MODEL -------------------------------------------------------------------

type Model {
  Model(
    recipe: Recipe,
    conversion_rate: Input(Quantity),
    clipboard_capabilities: ClipboardCapabilities,
    paste_outcome: Option(PasteOutcome),
    copy_outcome: Option(Outcome),
    load_outcome: Option(LoadOutcome),
  )
}

type ClipboardCapabilities {
  CannotUseClipboard
  CanUseClipboard
}

pub type Outcome {
  Success
  Waiting
  Failure
}

pub type LoadOutcome {
  LoadSuccess
  LoadWaiting
  CouldntLoad
  LoadNotARecipe
}

pub type PasteOutcome {
  PasteSuccess
  PasteWaiting
  PasteFetchingPage
  CouldntPaste
  PasteNotARecipe
}

fn placeholder_recipe() -> Recipe {
  Recipe(
    name: "🍰 Your recipe's name",
    ingredients: array.from_list([
      Ingredient(name: "An ingredient", quantity: Empty, converted: Empty),
    ]),
  )
}

fn init(
  initial_state: #(Recipe, ClipboardCapabilities),
) -> #(Model, Effect(msg)) {
  let #(recipe, clipboard_capabilities) = initial_state
  let model =
    Model(
      recipe:,
      clipboard_capabilities:,
      conversion_rate: Empty,
      copy_outcome: None,
      paste_outcome: None,
      load_outcome: None,
    )
  #(model, effect.none())
}

// --- UPDATE ------------------------------------------------------------------

type Msg {
  UserClickedAddIngredient
  UserClickedSaveRecipe
  UserClickedClear

  UserClickedPasteRecipe
  ClipboardPastedRecipe(result: Result(String, Nil))
  ServerSentFetchedJsonLd(result: Result(String, Nil))
  PasteRecipeOutcomeExpired

  UserClickedCopyRecipe
  ClipboardPerformedCopyRecipe(outcome: Outcome)
  CopyRecipeOutcomeExpired

  UserChoseRecipeToLoad(file: JsFile)
  FileReaderReadRecipe(result: Result(String, Nil))
  LoadRecipeOutcomeExpired

  UserChangedRecipeName(name: String)
  UserChangedConversionRate(rate: String)
  UserChangedIngredientName(index: Int, name: String)
  UserChangedIngredientOriginalQuantity(index: Int, quantity: String)
  UserChangedIngredientConversion(index: Int, quantity: String)
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserClickedClear -> {
      let #(model, _) =
        init(#(placeholder_recipe(), model.clipboard_capabilities))
      #(model, save_recipe_to_localstore(model.recipe))
    }

    UserClickedAddIngredient -> {
      let recipe = recipe.add_empty_ingredient(model.recipe)
      #(Model(..model, recipe:), focus_last_ingredient())
    }

    UserChoseRecipeToLoad(file) -> #(
      Model(..model, load_outcome: Some(LoadWaiting)),
      read_file(file, FileReaderReadRecipe),
    )

    FileReaderReadRecipe(result: Error(_)) -> {
      let model = Model(..model, load_outcome: Some(CouldntLoad))
      #(model, after_seconds(1, LoadRecipeOutcomeExpired))
    }

    FileReaderReadRecipe(result: Ok(recipe)) ->
      case recipe.from_string(recipe) {
        Ok(recipe) -> {
          let conversion_rate = Empty
          let load_outcome = Some(LoadSuccess)
          let model = Model(..model, recipe:, load_outcome:, conversion_rate:)
          let effects = [
            save_recipe_to_localstore(recipe),
            after_seconds(1, LoadRecipeOutcomeExpired),
          ]
          #(model, effect.batch(effects))
        }

        Error(_) -> {
          let model = Model(..model, load_outcome: Some(LoadNotARecipe))
          #(model, after_seconds(1, LoadRecipeOutcomeExpired))
        }
      }

    LoadRecipeOutcomeExpired -> #(
      Model(..model, load_outcome: None),
      effect.none(),
    )

    UserClickedSaveRecipe -> #(
      model,
      download(model.recipe.name, recipe.to_string(model.recipe)),
    )

    UserClickedCopyRecipe -> #(
      Model(..model, copy_outcome: Some(Waiting)),
      write_clipboard(recipe.to_string(model.recipe)),
    )

    ClipboardPerformedCopyRecipe(outcome) -> #(
      Model(..model, copy_outcome: Some(outcome)),
      after_seconds(1, CopyRecipeOutcomeExpired),
    )

    CopyRecipeOutcomeExpired -> #(
      Model(..model, copy_outcome: None),
      effect.none(),
    )

    UserClickedPasteRecipe -> #(
      Model(..model, paste_outcome: Some(PasteWaiting)),
      paste_from_clipboard(ClipboardPastedRecipe),
    )

    ClipboardPastedRecipe(result: Error(_))
    | ServerSentFetchedJsonLd(result: Error(_)) -> {
      let model = Model(..model, paste_outcome: Some(CouldntPaste))
      #(model, after_seconds(1, PasteRecipeOutcomeExpired))
    }

    ClipboardPastedRecipe(result: Ok(recipe)) ->
      case uri.parse(recipe) |> result.then(request.from_uri) {
        Error(_) -> parse_recipe(model, recipe, recipe.from_string)
        // If the thing being pasted is a valid url then we want to fetch the
        // content of the page and parse the json ld once it's loaded.
        Ok(_request) -> {
          let model = Model(..model, paste_outcome: Some(PasteFetchingPage))
          #(model, fetch_recipe(recipe))
        }
      }

    ServerSentFetchedJsonLd(result: Ok(json_ld)) ->
      parse_recipe(model, json_ld, recipe.from_json_ld)

    PasteRecipeOutcomeExpired -> #(
      Model(..model, paste_outcome: None),
      effect.none(),
    )

    UserChangedConversionRate(rate: raw) ->
      case quantity.parse(raw) {
        Error(_) -> {
          let recipe = recipe.empty_all_converted(model.recipe)
          let model = Model(..model, recipe:, conversion_rate: Invalid(raw:))
          #(model, effect.none())
        }

        Ok(value) -> {
          let recipe =
            recipe.convert_all_ingredients(model.recipe, using: value)
          let conversion_rate = Valid(raw:, parsed: value)
          #(Model(..model, recipe:, conversion_rate:), effect.none())
        }
      }

    UserChangedRecipeName(name:) -> {
      let recipe = Recipe(..model.recipe, name:)
      #(Model(..model, recipe:), save_recipe_to_localstore(recipe))
    }

    UserChangedIngredientConversion(index:, quantity:) -> {
      let assert Ok(ingredient) = array.get(model.recipe.ingredients, index)
      case ingredient.quantity {
        // If the original quantity is not present or invalid we should not be
        // able to change the conversion quantity, se we just empty the
        // conversion field.
        Invalid(_) | Empty -> {
          let recipe = recipe.empty_converted(model.recipe, index)
          #(Model(..model, recipe:), effect.none())
        }

        // If there's an original value we need to come up with the new
        // conversion rate and apply it to all other fields.
        Computed(value: original_value) | Valid(_, parsed: original_value) -> {
          case parse_quantity_field(quantity) {
            Empty -> {
              let recipe = recipe.empty_all_converted(model.recipe)
              #(Model(..model, recipe:, conversion_rate: Empty), effect.none())
            }

            Computed(_) as converted | Invalid(_) as converted -> {
              let recipe = recipe.set_converted(model.recipe, index, converted)

              #(Model(..model, recipe:), effect.none())
            }

            Valid(raw: _, parsed: final_value) as converted -> {
              let conversion_rate =
                FloatQuantity(
                  quantity.to_float(final_value)
                  /. quantity.to_float(original_value),
                )

              let recipe =
                model.recipe
                |> recipe.convert_all_ingredients(conversion_rate)
                |> recipe.set_converted(index, converted)

              let conversion_rate = Computed(conversion_rate)
              #(Model(..model, recipe:, conversion_rate:), effect.none())
            }
          }
        }
      }
    }

    UserChangedIngredientName(index:, name:) -> {
      let recipe =
        recipe.map_ingredient(model.recipe, index, fn(ingredient) {
          let name = string.replace(in: name, each: "\n", with: "")
          Ingredient(..ingredient, name:)
        })
      let model = Model(..model, recipe:)
      #(model, save_recipe_to_localstore(model.recipe))
    }

    UserChangedIngredientOriginalQuantity(index:, quantity:) -> {
      let quantity = parse_quantity_field(quantity)
      let recipe =
        recipe.map_ingredient(model.recipe, index, fn(ingredient) {
          let converted = case quantity, model.conversion_rate {
            Valid(raw: _, parsed: original), Computed(conversion_rate)
            | Valid(raw: _, parsed: original),
              Valid(raw: _, parsed: conversion_rate)
            -> Computed(quantity.multiply(original, by: conversion_rate))

            _, _ -> Empty
          }

          Ingredient(..ingredient, converted:, quantity:)
        })
      let model = Model(..model, recipe:)
      #(model, save_recipe_to_localstore(recipe))
    }
  }
}

fn parse_recipe(
  model: Model,
  recipe: String,
  with parser: fn(String) -> Result(Recipe, Nil),
) -> #(Model, Effect(Msg)) {
  case parser(recipe) {
    Ok(recipe) -> {
      let conversion_rate = Empty
      let paste_outcome = Some(PasteSuccess)
      let model = Model(..model, recipe:, conversion_rate:, paste_outcome:)
      let effects = [
        save_recipe_to_localstore(recipe),
        after_seconds(1, PasteRecipeOutcomeExpired),
      ]
      #(model, effect.batch(effects))
    }

    Error(_) -> #(
      Model(..model, paste_outcome: Some(PasteNotARecipe)),
      after_seconds(1, PasteRecipeOutcomeExpired),
    )
  }
}

fn parse_quantity_field(raw: String) -> Input(Quantity) {
  case quantity.parse(raw) {
    Ok(parsed) -> Valid(parsed:, raw:)
    Error(_) ->
      case string_extra.trim(raw) {
        "" -> Empty
        _ -> Invalid(raw)
      }
  }
}

// --- EFFECTS -----------------------------------------------------------------

fn write_clipboard(value: String) -> Effect(Msg) {
  use dispatch <- effect.from
  use outcome <- do_write_clipboard(value)
  dispatch(ClipboardPerformedCopyRecipe(outcome))
}

fn paste_from_clipboard(to_msg: fn(Result(String, Nil)) -> msg) -> Effect(msg) {
  use dispatch <- effect.from
  use content <- do_read_clipboard
  dispatch(to_msg(content))
}

fn download(file_name: String, value: String) -> Effect(msg) {
  use _dispatch <- effect.from
  do_download(file_name, value)
}

fn read_file(
  file: JsFile,
  to_msg: fn(Result(String, Nil)) -> msg,
) -> Effect(msg) {
  use dispatch <- effect.from
  use file_content <- do_read_file(file)
  dispatch(to_msg(file_content))
}

fn save_recipe_to_localstore(recipe: Recipe) -> Effect(msg) {
  use _dispatch <- effect.from
  set_item(recipe_localstore_key, recipe.to_string(recipe))
}

fn focus_last_ingredient() -> Effect(msg) {
  use _dispatch <- effect.from
  use <- after_render
  focus(".recipe-table tr:last-child th")
}

fn after_seconds(seconds: Int, msg: msg) -> Effect(msg) {
  use dispatch <- effect.from
  use <- do_after_seconds(seconds)
  dispatch(msg)
}

fn fetch_recipe(url: String) -> Effect(Msg) {
  let encoded_url = uri.percent_encode(url)
  // We send the request to our proxy server hosted on fly!
  let assert Ok(request) = uri.parse(proxy_server <> "/fetch/" <> encoded_url)
  let assert Ok(request) = request.from_uri(request)
  http_get(request, ServerSentFetchedJsonLd)
}

fn http_get(
  request: Request(String),
  to_msg: fn(Result(String, Nil)) -> msg,
) -> Effect(msg) {
  use dispatch <- effect.from

  fetch.send(request)
  |> promise.try_await(fetch.read_text_body)
  |> promise.tap(fn(response) {
    case response {
      Ok(response) -> Ok(response.body)
      Error(_) -> Error(Nil)
    }
    |> to_msg
    |> dispatch
  })

  Nil
}

// --- VIEW --------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div([attribute.class("outer-container")], [
    html.main([attribute.class("main-container")], [
      editable_cell(
        Enabled,
        html.h1,
        [attribute.class("recipe-title"), on_cell_input(UserChangedRecipeName)],
        [html.text(model.recipe.name)],
      ),
      recipe_table_view(model),
      controls_view(model),
    ]),
    html.footer([], [
      html.span([], [html.text("This website is written in Gleam")]),
      icon.star([]),
      html.a([attribute.href("https://github.com/giacomocavalieri/preppy")], [
        html.text("View the source code"),
      ]),
    ]),
  ])
}

fn controls_view(model: Model) -> Element(Msg) {
  let copy_paste_group = case model.clipboard_capabilities {
    CannotUseClipboard -> element.fragment([])
    CanUseClipboard ->
      html.div([attribute.class("controls-group")], [
        copy_button(model.copy_outcome),
        paste_button(model.paste_outcome),
      ])
  }

  html.div(
    [attribute.class("controls-container"), attribute.class("no-print")],
    [
      html.button([event.on_click(UserClickedAddIngredient)], [
        icon.plus([]),
        html.text("add ingredient"),
      ]),
      html.button([event.on_click(UserClickedClear)], [
        icon.eraser([]),
        html.text("clear"),
      ]),
      copy_paste_group,
      html.div([attribute.class("controls-group")], [
        html.button([event.on_click(UserClickedSaveRecipe)], [
          icon.download([]),
          html.text("save recipe"),
        ]),
        load_button(model.load_outcome),
      ]),
    ],
  )
}

fn copy_button(outcome: Option(Outcome)) -> Element(Msg) {
  let text = case outcome {
    None -> "copy recipe"
    Some(Waiting) -> "copying..."
    Some(Failure) -> "couldn't copy"
    Some(Success) -> "copied!"
  }

  let class = case outcome {
    None -> attribute.none()
    Some(Waiting) -> attribute.class("waiting")
    Some(Success) -> attribute.class("success")
    Some(Failure) -> attribute.class("failure")
  }

  let icon = case outcome {
    None -> icon.clipboard_copy([])
    Some(Waiting) -> icon.gear([])
    Some(Failure) -> icon.cross([])
    Some(Success) -> icon.check([])
  }

  let on_click = case outcome {
    None -> event.on_click(UserClickedCopyRecipe)
    Some(_) -> attribute.none()
  }

  html.button([on_click, class], [icon, html.text(text)])
}

fn paste_button(outcome: Option(PasteOutcome)) -> Element(Msg) {
  let text = case outcome {
    None -> "paste recipe"
    Some(PasteWaiting) -> "pasting..."
    Some(PasteFetchingPage) -> "getting recipe from link..."
    Some(CouldntPaste) -> "couldn't paste"
    Some(PasteNotARecipe) -> "not a recipe"
    Some(PasteSuccess) -> "pasted!"
  }

  let class = case outcome {
    None -> attribute.none()
    Some(PasteWaiting) | Some(PasteFetchingPage) -> attribute.class("waiting")
    Some(PasteSuccess) -> attribute.class("success")
    Some(PasteNotARecipe) | Some(CouldntPaste) -> attribute.class("failure")
  }

  let icon = case outcome {
    None -> icon.clipboard([])
    Some(PasteWaiting) | Some(PasteFetchingPage) -> icon.gear([])
    Some(PasteSuccess) -> icon.check([])
    Some(PasteNotARecipe) | Some(CouldntPaste) -> icon.cross([])
  }

  let on_click = case outcome {
    None -> event.on_click(UserClickedPasteRecipe)
    Some(_) -> attribute.none()
  }

  html.button([on_click, class], [icon, html.text(text)])
}

fn load_button(outcome: Option(LoadOutcome)) -> Element(Msg) {
  let text = case outcome {
    None -> "load recipe"
    Some(LoadWaiting) -> "loading..."
    Some(CouldntLoad) -> "couldn't load"
    Some(LoadNotARecipe) -> "not a recipe"
    Some(LoadSuccess) -> "loaded!"
  }

  let class = case outcome {
    None -> attribute.none()
    Some(LoadWaiting) -> attribute.class("waiting")
    Some(CouldntLoad) | Some(LoadNotARecipe) -> attribute.class("failure")
    Some(LoadSuccess) -> attribute.class("success")
  }

  let icon = case outcome {
    None -> icon.upload([])
    Some(LoadWaiting) -> icon.gear([])
    Some(CouldntLoad) | Some(LoadNotARecipe) -> icon.cross([])
    Some(LoadSuccess) -> icon.check([])
  }

  let on_file_upload = case outcome {
    None -> on_file_upload(UserChoseRecipeToLoad)
    Some(_) -> attribute.none()
  }

  html.div([], [
    html.input([
      attribute.type_("file"),
      attribute.accept(["text/plain", ".cook", ".txt", ".md"]),
      attribute.id("recipe-upload-input"),
      attribute.attribute("hidden", "true"),
      on_file_upload,
    ]),
    html.label(
      [
        attribute.role("button"),
        attribute.id("load-recipe-label"),
        attribute.for("recipe-upload-input"),
        class,
      ],
      [icon, html.text(text)],
    ),
  ])
}

fn recipe_table_view(model: Model) -> Element(Msg) {
  let rows =
    list.reverse({
      use rows, ingredient, i <- array.index_fold(model.recipe.ingredients, [])
      let row = ingredient_row_view(ingredient, i)
      [row, ..rows]
    })

  let conversion_rate_text = case model.conversion_rate {
    Invalid(raw:) | Valid(raw:, parsed: _) -> raw
    Computed(value:) -> quantity.to_pretty_string(value, HideDecimalPartIfZero)
    Empty -> "1"
  }

  let conversion_rate_class = case model.conversion_rate {
    Computed(_) | Empty | Valid(_, _) -> attribute.none()
    Invalid(_) -> attribute.class("invalid")
  }

  let conversion_rate =
    html.span([attribute.class("conversion-rate")], [
      html.text(" x"),
      editable_cell(
        Enabled,
        html.span,
        [conversion_rate_class, on_cell_input(UserChangedConversionRate)],
        [html.text(conversion_rate_text)],
      ),
    ])

  html.table([attribute.class("recipe-table")], [
    html.thead([], [
      html.th([], [html.text("Ingredient")]),
      html.th([], [html.text("Quantity")]),
      html.th([], [html.text("Convert to"), conversion_rate]),
    ]),
    html.tbody([], rows),
  ])
}

fn ingredient_row_view(ingredient: Ingredient, index: Int) -> Element(Msg) {
  // The cell with the ingredient's name
  //
  let ingredient_cell =
    editable_cell(
      Enabled,
      html.th,
      [
        on_cell_input(UserChangedIngredientName(index, _)),
        attribute.attribute("inputmode", "text"),
      ],
      [html.text(ingredient.name)],
    )

  // Now we make the cell with the ingredient's original quantity.
  // We want to mark it as invalid if the data is not right.
  //
  let quantity_class = case ingredient.quantity {
    Computed(_) | Empty | Valid(_, _) -> attribute.none()
    Invalid(_) -> attribute.class("invalid")
  }

  let quantity_cell =
    editable_cell(
      Enabled,
      html.td,
      [
        on_cell_input(UserChangedIngredientOriginalQuantity(index, _)),
        attribute.attribute("inputmode", "decimal"),
        quantity_class,
      ],
      [html.text(pretty_quantity(ingredient.quantity, HideDecimalPartIfZero))],
    )

  // Finally we make the cell of the new computed quantity.
  // We want it to be disabled if the original quantity is invalid or empty.
  // And if it contains data that is not valid we mark it as invalid.
  //
  let converted_disabled = case ingredient.quantity {
    Computed(_) | Valid(_, _) -> Enabled
    Empty | Invalid(_) -> Disabled
  }

  let converted_class = case ingredient.converted {
    Computed(_) | Empty | Valid(_, _) -> attribute.none()
    Invalid(_) -> attribute.class("invalid")
  }

  // If someone wrote a valid whole number as the quantity we don't want to
  // show a decimal part if it's zero. However, if someone explicitly writes
  // `.0` then we'll keep it as it looks like it's intentional.
  //
  let pretty_options = case ingredient.quantity {
    Computed(_) | Empty | Invalid(_) -> HideDecimalPartIfZero
    Valid(raw:, parsed: _) ->
      case string.contains(raw, ".") || string.contains(raw, ",") {
        False -> HideDecimalPartIfZero
        True -> KeepDecimalPart
      }
  }

  let converted_cell =
    editable_cell(
      converted_disabled,
      html.td,
      [
        on_cell_input(UserChangedIngredientConversion(index, _)),
        attribute.attribute("inputmode", "decimal"),
        converted_class,
      ],
      [html.text(pretty_quantity(ingredient.converted, pretty_options))],
    )

  // Let's put everything together.
  //
  html.tr([], [ingredient_cell, quantity_cell, converted_cell])
}

type Editable {
  Enabled
  Disabled
}

fn editable_cell(
  status: Editable,
  of: fn(List(Attribute(msg)), List(Element(msg))) -> Element(msg),
  attributes: List(Attribute(msg)),
  elements: List(Element(msg)),
) -> Element(msg) {
  let disabled_class = case status {
    Disabled -> attribute.class("disabled")
    Enabled -> attribute.none()
  }

  let content_editable = case status {
    Disabled -> "false"
    Enabled -> "true"
  }

  of(
    [
      attribute.class("input-cell"),
      attribute.attribute("contenteditable", content_editable),
      disabled_class,
      ..attributes
    ],
    elements,
  )
}

fn pretty_quantity(quantity: Input(Quantity), options: PrettyOptions) -> String {
  case quantity {
    Invalid(raw:) | Valid(raw:, parsed: _) -> raw
    Computed(value:) -> quantity.to_pretty_string(value, options)
    Empty -> ""
  }
}

// --- UTILS -------------------------------------------------------------------

fn on_cell_input(emit: fn(String) -> msg) -> Attribute(msg) {
  event.on("input", fn(event) {
    zero.at(["target", "innerText"], zero.string)
    |> zero.run(event, _)
    |> result.map(emit)
  })
}

type JsFile

fn on_file_upload(emit: fn(JsFile) -> msg) -> Attribute(msg) {
  event.on("change", fn(event) {
    zero.at(["target", "files"], zero.at([0], zero.dynamic))
    |> zero.run(event, _)
    |> result.map(fn(file) {
      file |> unsafe_super_dangerous_coerce_dont_use_me |> emit
    })
  })
}

// --- FFI ---------------------------------------------------------------------

@external(javascript, "./preppy.ffi.mjs", "unsafe_super_dangerous_coerce_dont_use_me")
fn unsafe_super_dangerous_coerce_dont_use_me(from: Dynamic) -> a

@external(javascript, "./preppy.ffi.mjs", "do_download")
fn do_download(file_name: String, value: String) -> Nil

@external(javascript, "./preppy.ffi.mjs", "do_read_file")
fn do_read_file(file: JsFile, k: fn(Result(String, Nil)) -> Nil) -> Nil

@external(javascript, "./preppy.ffi.mjs", "focus")
fn focus(selector: String) -> Nil

@external(javascript, "./preppy.ffi.mjs", "set_item")
fn set_item(key: String, value: String) -> Nil

@external(javascript, "./preppy.ffi.mjs", "get_item")
fn get_item(key: String) -> String

@external(javascript, "./preppy.ffi.mjs", "do_after_seconds")
fn do_after_seconds(seconds: Int, do: fn() -> Nil) -> Nil

@external(javascript, "./preppy.ffi.mjs", "after_render")
fn after_render(do: fn() -> a) -> Nil

@external(javascript, "./preppy.ffi.mjs", "do_write_clipboard")
fn do_write_clipboard(value: String, do: fn(Outcome) -> a) -> a

@external(javascript, "./preppy.ffi.mjs", "do_read_clipboard")
fn do_read_clipboard(do: fn(Result(String, Nil)) -> Nil) -> Nil

@external(javascript, "./preppy.ffi.mjs", "is_ios")
fn is_ios() -> Bool

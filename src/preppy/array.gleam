import gleam/dict.{type Dict}
import gleam/list

pub opaque type Array(a) {
  Array(items: Dict(Int, a), size: Int)
}

pub fn get(array: Array(a), index: Int) -> Result(a, Nil) {
  dict.get(array.items, index)
}

pub fn from_list(list: List(a)) -> Array(a) {
  let #(items, size) = {
    use acc, item, i <- list.index_fold(over: list, from: #(dict.new(), 0))
    let #(items, size) = acc
    #(dict.insert(items, i, item), size + 1)
  }

  Array(items:, size:)
}

pub fn to_list(array: Array(a)) -> List(a) {
  use acc, index <- fold_desc_range(from: array.size - 1, to: 0, acc: [])
  let assert Ok(item) = dict.get(array.items, index)
  [item, ..acc]
}

pub fn update(array: Array(a), at index: Int, with fun: fn(a) -> a) -> Array(a) {
  case dict.get(array.items, index) {
    Error(_) -> array
    Ok(item) -> {
      let items = dict.insert(array.items, index, fun(item))
      Array(..array, items:)
    }
  }
}

pub fn index_map(array: Array(a), with fun: fn(a, Int) -> b) -> Array(b) {
  let Array(items:, size:) = array
  let items = dict.map_values(items, fn(i, item) { fun(item, i) })
  Array(size:, items:)
}

pub fn map(array: Array(a), with fun: fn(a) -> b) -> Array(b) {
  index_map(array, fn(a, _) { fun(a) })
}

pub fn append(array: Array(a), item: a) -> Array(a) {
  let Array(items:, size:) = array
  let items = dict.insert(items, size, item)
  Array(items:, size: size + 1)
}

pub fn index_fold(
  over array: Array(a),
  from acc: acc,
  with fun: fn(acc, a, Int) -> acc,
) -> acc {
  let Array(items:, size:) = array
  use acc, i <- fold_asc_range(0, size - 1, acc)
  let assert Ok(item) = dict.get(items, i)
  fun(acc, item, i)
}

fn fold_asc_range(
  from start: Int,
  to end: Int,
  acc acc: acc,
  with fun: fn(acc, Int) -> acc,
) -> acc {
  case start > end {
    False -> fold_asc_range(start + 1, end, fun(acc, start), fun)
    True -> acc
  }
}

fn fold_desc_range(
  from start: Int,
  to end: Int,
  acc acc: acc,
  with fun: fn(acc, Int) -> acc,
) -> acc {
  case start < end {
    False -> fold_desc_range(start - 1, end, fun(acc, start), fun)
    True -> acc
  }
}

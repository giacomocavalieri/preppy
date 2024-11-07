import * as $preppy from "./preppy.mjs";
import * as $gleam from "./gleam.mjs";

export function do_download(filename, value) {
  var element = document.createElement("a");
  element.setAttribute("href", "data:text/plain;charset=utf-8," + encodeURIComponent(value));
  element.setAttribute("download", filename);

  element.style.display = "none";
  document.body.appendChild(element);

  element.click();

  document.body.removeChild(element);
  return;
}

export function do_read_file(file, k) {
  let reader = new FileReader();
  reader.readAsText(file);
  reader.onload = () => k(new $gleam.Ok(reader.result));
  reader.onerror = () => k(new $gleam.Error());
  reader.onabort = () => k(new $gleam.Error());
}

export function unsafe_super_dangerous_coerce_dont_use_me(value) {
  return value;
}

export function set_item(key, value) {
  localStorage.setItem(key, value);
}

export function get_item(key) {
  return localStorage.getItem(key) ?? "";
}

export function do_after_seconds(seconds, k) {
  setTimeout(k, seconds * 1000);
}

export function focus(selector) {
  document?.querySelector(selector)?.focus();
}

export function after_render(k) {
  return requestAnimationFrame(k);
}

export function do_write_clipboard(value, k) {
  try {
    navigator.clipboard
      .writeText(value)
      .then(() => k(new $preppy.Success()))
      .catch((e) => {
        console.log(e);
        k(new $preppy.Failure());
      });
  } catch {
    k(new $preppy.Failure());
  }
}

export function do_read_clipboard(k) {
  try {
    navigator.clipboard
      .readText()
      .then((value) => k(new $gleam.Ok(value)))
      .catch((e) => {
        console.log(e);
        k(new $gleam.Error());
      });
  } catch {
    k(new $gleam.Error());
  }
}

export function is_ios() {
  return !!navigator?.userAgent?.match(/iPad|iPhone/i);
}

export function drop_bytes(string, bytes) {
  return string.slice(bytes);
}

export function first_byte(string) {
  return string.slice(0, 1);
}

export function slice_bytes(string, from, size) {
  return string.slice(from, from + size);
}

export function get_json_ld_script_content(string) {
  console.log(new DOMParser().parseFromString(string, "text/html"));
  return new $gleam.Error();
}

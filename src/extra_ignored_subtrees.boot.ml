open Stdune

let ignore path =
  match Path.to_string path with
  | "test" | "example" -> true
  | _ -> false

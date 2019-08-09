open Stdune

let bootstrapping = true

let data_only_path p =
  match Path.Source.to_string p with
  | "test" | "example" -> true
  | _ -> false

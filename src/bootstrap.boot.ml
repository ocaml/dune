open Stdune

let data_only_path p =
  match Path.to_string p with
  | "test" | "example" -> true
  | _ -> false

open Import

type t = string

let to_string t = t
let of_string s = s
let conv = Conv.string

module Table = String.Table

let gen =
  let module Id = Stdune.Id.Make () in
  fun () -> Id.gen () |> Id.to_int |> Int.to_string
;;

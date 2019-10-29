open Stdune
module Re = Dune_re

type t =
  { re : Re.re
  ; repr : string
  }

let test t = Re.execp t.re

let empty = { re = Re.compile Re.empty; repr = "\000" }

let universal = { re = Re.compile (Re.rep Re.any); repr = "**" }

let of_string_result repr =
  GlobLexer.parse_string repr
  |> Result.map ~f:(fun re -> { re = Re.compile re; repr })

let of_string repr =
  match of_string_result repr with
  | Error (_, msg) -> invalid_arg (Printf.sprintf "invalid glob: :%s" msg)
  | Ok t -> t

let to_string (t : t) = t.repr

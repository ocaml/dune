open Stdune

type t =
  { re : Dune_re.re
  ; repr : string
  }

let test t = Dune_re.execp t.re

let of_string repr =
  let result =
    Dune_glob_lexer.parse_string repr
    |> Result.map ~f:(fun re -> { re = Dune_re.compile re; repr })
  in
  match result with
  | Error (_, msg) -> invalid_arg (Printf.sprintf "invalid glob: :%s" msg)
  | Ok t -> t

let to_string (t : t) = t.repr

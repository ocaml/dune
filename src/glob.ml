open! Stdune
open Import

type t =
  { re : Dune_re.re
  ; repr : string
  }

let equal x y = String.equal x.repr y.repr

let hash t = String.hash t.repr

let to_sexp t = Sexp.Encoder.string t.repr

let of_string repr =
  Glob_lexer.parse_string repr
  |> Result.map ~f:(fun re ->
    { re = Re.compile re
    ; repr
    })

let of_string_exn loc repr =
  match of_string repr with
  | Error (_, msg) ->
    Errors.fail loc "invalid glob: :%s" msg
  | Ok t -> t

let decode =
  let open Stanza.Decoder in
  plain_string (fun ~loc str ->
    of_string_exn loc str)

let test t = Re.execp t.re

let filter t = List.filter ~f:(test t)

let empty =
  { re = Re.compile Re.empty
  ; repr = "\000"
  }

let to_pred t =
  let id = lazy (Sexp.List [Atom "Glob"; Atom t.repr]) in
  Predicate.create ~id ~f:(test t)

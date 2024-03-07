open Stdune
module Re = Dune_re

type t =
  | Re of
      { re : Re.re
      ; repr : string
      }
  | Literal of string

let test t s =
  match t with
  | Literal t -> String.equal t s
  | Re { re; repr = _ } -> Re.execp re s
;;

let empty = Re { re = Re.compile Re.empty; repr = "\000" }
let universal = Re { re = Re.compile (Re.rep Re.any); repr = "**" }

let of_string_result repr =
  Lexer.parse_string repr
  |> Result.map ~f:(function
    | Lexer.Literal s -> Literal s
    | Re re -> Re { re = Re.compile re; repr })
;;

let of_string repr =
  match of_string_result repr with
  | Error (_, msg) -> invalid_arg (Printf.sprintf "invalid glob: :%s" msg)
  | Ok t -> t
;;

let to_string t =
  match t with
  | Re { repr; re = _ } -> repr
  | Literal s -> s
;;

let to_dyn t = Dyn.variant "Glob" [ Dyn.string (to_string t) ]

let of_string_exn loc repr =
  match of_string_result repr with
  | Error (_, msg) -> User_error.raise ~loc [ Pp.textf "invalid glob: %s" msg ]
  | Ok t -> t
;;

let compare x y = String.compare (to_string x) (to_string y)
let hash t = String.hash (to_string t)

let matching_extensions extensions =
  let re =
    let open Re in
    [ rep any
    ; char '.'
    ; List.map extensions ~f:(fun s ->
        match of_string s with
        | Literal _ -> str s
        | Re _ ->
          (* we cannot allow anything that can be parsed as a regex
             here b/c we want the string representation to match [of_string]
          *)
          Code_error.raise "invalid extension" [ "s", Dyn.string s ])
      |> alt
    ]
    |> seq
    |> compile
  in
  Re
    { re
    ; repr =
        (match extensions with
         | [] -> Code_error.raise "empty list of extensions" []
         | [ x ] -> sprintf "*.%s" x
         | xs -> sprintf "*.{%s}" (String.concat xs ~sep:","))
    }
;;

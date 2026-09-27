open Import

module T = struct
  type t =
    | Named of string
    | Anonymous of Source_path.t

  let compare a b =
    match a, b with
    | Named x, Named y -> String.compare x y
    | Anonymous x, Anonymous y -> Source_path.compare x y
    | Named _, Anonymous _ -> Lt
    | Anonymous _, Named _ -> Gt
  ;;

  let equal a b = Ordering.is_eq (compare a b)

  let repr =
    Repr.variant
      "dune-project-name"
      [ Repr.case "Named" String.repr ~proj:(function
          | Named n -> Some n
          | Anonymous _ -> None)
      ; Repr.case "Anonymous" Source_path.repr ~proj:(function
          | Anonymous p -> Some p
          | Named _ -> None)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
end

include T
module Map = Map.Make (T)
module Infix = Comparator.Operators (T)

let to_string_hum = function
  | Named s -> s
  | Anonymous p -> sprintf "<anonymous %s>" (Source_path.to_string_maybe_quoted p)
;;

let validate name =
  let len = String.length name in
  len > 0
  && String.for_all name ~f:(function
    | '.' | '/' -> false
    | _ -> true)
;;

let anonymous path = Anonymous path

let named loc name =
  if validate name
  then Named name
  else User_error.raise ~loc [ Pp.textf "%S is not a valid Dune project name." name ]
;;

open Dune_sexp

let decode = Decoder.plain_string (fun ~loc s -> named loc s)
let encode n = Encoder.string (to_string_hum n)

let name = function
  | Anonymous _ -> None
  | Named s -> Some s
;;

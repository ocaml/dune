open Import

module T = struct
  type t =
    | Named of string
    | Anonymous of Path.Source.t

  let compare a b =
    match a, b with
    | Named x, Named y -> String.compare x y
    | Anonymous x, Anonymous y -> Path.Source.compare x y
    | Named _, Anonymous _ -> Lt
    | Anonymous _, Named _ -> Gt
  ;;

  let equal a b = Ordering.is_eq (compare a b)

  let to_dyn =
    let open Dyn in
    function
    | Named n -> variant "Named" [ string n ]
    | Anonymous p -> variant "Anonymous" [ Path.Source.to_dyn p ]
  ;;
end

include T
module Map = Map.Make (T)
module Infix = Comparator.Operators (T)

let to_string_hum = function
  | Named s -> s
  | Anonymous p -> sprintf "<anonymous %s>" (Path.Source.to_string_maybe_quoted p)
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

let decode = Dune_lang.Decoder.plain_string (fun ~loc s -> named loc s)
let encode n = Dune_lang.Encoder.string (to_string_hum n)

let name = function
  | Anonymous _ -> None
  | Named s -> Some s
;;

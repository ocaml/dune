open Stdune

type t =
  | Public
  | Private

let to_string = function
  | Public -> "public"
  | Private -> "private"
;;

let to_dyn t = Dyn.string (to_string t)

let encode =
  let open Dune_sexp.Encoder in
  function
  | Public -> string "public"
  | Private -> string "private"
;;

let decode =
  let open Dune_sexp.Decoder in
  plain_string (fun ~loc ->
       function
       | "public" -> Public
       | "private" -> Private
       | _ ->
         User_error.raise
           ~loc
           [ Pp.text "Not a valid visibility. Valid visibility is public or private" ])
;;

module Map = struct
  type 'a t =
    { public : 'a
    ; private_ : 'a
    }

  let make_both a = { public = a; private_ = a }

  let find { private_; public } = function
    | Private -> private_
    | Public -> public
  ;;
end

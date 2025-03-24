open! Stdune
open Dune_sexp

type t =
  { cases : (Blang.t * String_with_vars.t) list
  ; default : String_with_vars.t option
  ; loc : Loc.t
  }

let to_dyn { cases; default; loc } =
  Dyn.record
    [ "cases", Dyn.list (Dyn.pair Blang.to_dyn String_with_vars.to_dyn) cases
    ; "default", Dyn.option String_with_vars.to_dyn default
    ; "loc", Loc.to_dyn loc
    ]
;;

let decode =
  let open Decoder in
  let decode_condition =
    (let+ () = keyword "default" in
     `Default)
    <|> let+ blang = Blang.decode in
        `Case blang
  in
  let+ loc, conditions =
    located (repeat (pair (located decode_condition) String_with_vars.decode))
  in
  let cases, default =
    let rec loop = function
      | [] -> [], None
      | [ ((_loc, `Default), value) ] -> [], Some value
      | ((_loc, `Case blang), value) :: xs ->
        let cases, default = loop xs in
        (blang, value) :: cases, default
      | ((loc, `Default), _) :: _ ->
        User_error.raise
          ~loc
          [ Pp.text "The default case may only appear as the final case." ]
    in
    loop conditions
  in
  { cases; default; loc }
;;

let equal { cases; default; loc = _ } t =
  List.equal (Tuple.T2.equal Blang.equal String_with_vars.equal) cases t.cases
  && Option.equal String_with_vars.equal default t.default
;;

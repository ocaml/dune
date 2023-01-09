(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

(* Legacy is used to signal that we are in a mode prior to Coq syntax 0.3 ,
   where mode was not supported, this allows us support older Coq compiler
   versions with coq-lang < 0.3 *)
type t =
  | Legacy
  | VoOnly
  | Native

let decode ~coq_syntax =
  Dune_lang.Decoder.(
    enum'
      [ ("vo", return VoOnly)
      ; ( "native"
        , Dune_sexp.Syntax.deprecated_in coq_syntax (0, 7)
            ~extra_info:
              "Since Coq lang 0.7 native mode is automatically inferred from \
               the configuration of Coq."
          >>> return Native )
      ])

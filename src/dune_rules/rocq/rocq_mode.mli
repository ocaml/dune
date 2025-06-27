(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

(* Legacy is used to signal that we are in a mode prior to Rocq syntax 0.3 ,
   where mode was not supported, this allows us support older Coq compiler
   versions with coq-lang < 0.3 *)
type t =
  | Legacy
  | VoOnly
  | Native
  | VosOnly

val decode : rocq_syntax:Dune_lang.Syntax.t -> t Dune_lang.Decoder.t

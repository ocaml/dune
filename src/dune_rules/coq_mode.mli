(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

type t =
  | VoOnly
  | Native

val decode : t Dune_lang.Decoder.t

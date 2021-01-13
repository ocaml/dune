(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

type t =
  | VoOnly
  | Native

let decode = Dune_lang.Decoder.(enum [ ("vo", VoOnly); ("native", Native) ])

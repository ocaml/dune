(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

open! Stdune

(* This is in its own file due to dependency issues *)

(** A Coq library name is a dot-separated list of Coq module identifiers. *)
type t

val wrapper : t -> string

val encode : t Dune_lang.Encoder.t

val decode : (Loc.t * t) Dune_lang.Decoder.t

(* to be removed in favor of encode / decode *)
(* val _pp : t -> Pp.t *)
val to_dyn : t -> Dyn.t

module Map : Map.S with type key = t

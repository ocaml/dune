open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

(* This is in its own file due to dependency issues *)

(** A Coq library name is a dot-separated list of Coq module identifiers. *)
type t

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool

(** Returns the wrapper name, a dot-separated list of Coq module identifies *)
val wrapper : t -> string

(** Returns the directory name for a lib name, in this case library name foo.bar
    lives in foo/bar *)
val dir : t -> string

val encode : t Dune_lang.Encoder.t
val decode : (Loc.t * t) Dune_lang.Decoder.t

(* to be removed in favor of encode / decode *)
val to_string : t -> string

(** The Coq standard library name *)
val stdlib : t

val to_list : t -> string list
val append : t -> string -> t
val empty : t
val pp : t -> t Pp.t
val to_dyn : t -> Dyn.t

module Map : Map.S with type key = t

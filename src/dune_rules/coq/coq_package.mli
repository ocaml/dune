(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) Inria 2019-2024                         *)
(* (c) CNRS 2025                               *)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Li-yao Xia                      *)
open Import

val rocq_package_file : string

type t
type meta

val name : t -> Coq_lib_name.t
val path : t -> Path.t

(** List of .vo files in a path *)
val vo : t -> Path.t list

val fake_stanza : t -> Coq_stanza.Theory.t
val of_stanza : Coq_stanza.Theory.t -> meta
val parse : Path.t -> Lexbuf.t -> meta
val write : meta -> string


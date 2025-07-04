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
type meta = {
  name: Loc.t * Coq_lib_name.t;
  coq_lang_version: Syntax.Version.t;
  boot: bool;
  use_stdlib: bool;
  plugins: (Loc.t * Lib_name.t) list;
  theories: (Loc.t * Coq_lib_name.t) list;
  loc: Loc.t;
}

val meta : t -> meta
val name : t -> Coq_lib_name.t
val path : t -> Path.t

(** List of .vo files in a path *)
val vo : t -> Path.t list

val make : vo:Path.t list -> path:Path.t -> meta -> t
val of_stanza : Coq_stanza.Theory.t -> meta
val of_file : name:Coq_lib_name.t -> Path.t -> meta
val write : meta -> string


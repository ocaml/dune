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

val name : t -> Coq_lib_name.t
val path : t -> Path.t

(** List of .vo files in a path *)
val vo : t -> Path.t list

(** Unused for now, maybe be useful for coqdep -modules *)
val cmxs : t -> Path.t list

(** List of directories that contain .cmxs files and thus need to be passed to
    Coq using -I *)
val cmxs_directories : t -> Path.t list

(** Does the path correspond to Coq's stdlib? *)
val stdlib : t -> bool

val make : name:Coq_lib_name.t -> path:Path.t -> theories:Coq_lib_name.t list -> t
val parse : Path.t -> Lexbuf.t -> t
val write : t -> string


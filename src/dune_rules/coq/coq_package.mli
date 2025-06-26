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

val make : theories:Coq_lib_name.t list -> t
val parse : Path.t -> Lexbuf.t -> t
val write : t -> string


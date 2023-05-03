(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) Inria 2019-2023                         *)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)

open Import

(** A Coq Path is a non-dune source of Coq theories, these can come from Coq's
    stdlib and user-contrib location, and [COQPATH] enviroment variable. *)

(** This module is similar to [Dir_contents] but for globally installed libs *)

type t

val name : t -> Coq_lib_name.t

val path : t -> Path.t

val vo : t -> Path.t list

val cmxs : t -> Path.t list

val stdlib : t -> bool

val of_coq_install : Context.t -> t list Memo.t

(**  *)
val of_env : Env.t -> t list Memo.t

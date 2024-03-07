(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) Inria 2019-2023                         *)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)

open Import

(** A Coq Path is a non-dune source of Coq theories, these can come from Coq's
    stdlib and user-contrib location, and [COQPATH] environment variable. *)

(** This module is similar to [Dir_contents] but for globally installed libs *)

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

(** Build list of Coq paths from a Coq install ([COQLIB] and [coqc -config]) *)
val of_coq_install : Context.t -> t list Memo.t

(** Build list of Coq paths from [COQPATH] variable *)
val of_env : Env.t -> t list Memo.t

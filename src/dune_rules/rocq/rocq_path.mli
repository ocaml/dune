(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

(** A Rocq Path is a non-dune source of Rocq theories, these can come from Rocq's
    stdlib and user-contrib location, and [COQPATH] environment variable. *)

(** This module is similar to [Dir_contents] but for globally installed libs *)

type t

val name : t -> Rocq_lib_name.t
val path : t -> Path.t

(** List of .vo files in a path *)
val vo : t -> Path.t list

(** Does the path correspond to Rocq's [Corelib]? *)
val corelib : t -> bool

(** Build list of Rocq paths from a Rocq install ([COQLIB] and [coqc -config]) *)
val of_rocq_install : Context.t -> t list Memo.t

(** Build list of Rocq paths from [COQPATH] variable *)
val of_env : Env.t -> t list Memo.t

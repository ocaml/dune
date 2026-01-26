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

(** Handling of Rocq source files *)
open Import

open Rocq_stanza

type t

val empty : t

(** Rocq modules of library [name] is the Rocq library name. *)
val library : t -> name:Rocq_lib_name.t -> Rocq_module.t list

val directories : t -> name:Rocq_lib_name.t -> Path.Build.t list
val extract : t -> Extraction.t -> Rocq_module.t

val of_dir
  :  Stanza.t list
  -> dir:Path.Build.t
  -> include_subdirs:Loc.t * Include_subdirs.t
  -> dirs:Source_file_dir.t Nonempty_list.t
  -> t

(** [find_module ~source t] finds a Rocq library name and module corresponding to
    file [source], if there is one. *)
val find_module : source:Path.Build.t -> t -> (Rocq_lib_name.t * Rocq_module.t) option

val lookup_module
  :  t
  -> Rocq_module.t
  -> [ `Theory of Theory.t | `Extraction of Extraction.t ] option

val mlg_files
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> modules:Ordered_set_lang.t
  -> Path.Build.t list Memo.t

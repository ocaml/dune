(** Merlin rules *)
open! Dune_engine

open! Stdune
open Import

type t

val make :
     ?requires:Lib.t list Or_exn.t
  -> flags:Ocaml_flags.t
  -> ?preprocess:Preprocess.Without_instrumentation.t Preprocess.t
  -> ?libname:Lib_name.Local.t
  -> ?source_dirs:Path.Source.Set.t
  -> modules:Modules.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> unit
  -> t

val merlin_file_name : string

val add_source_dir : t -> Path.Source.t -> t

val merge_all : allow_approx_merlin:bool -> t list -> t option

(** Add rules for generating the .merlin in a directory *)
val add_rules :
     Super_context.t
  -> dir:Path.Build.t
  -> more_src_dirs:Path.Source.t list
  -> expander:Expander.t
  -> t
  -> unit

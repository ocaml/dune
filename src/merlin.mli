(** Merlin rules *)

open! Stdune
open Import

type t

val make
  :  ?requires:Lib.t list Or_exn.t
  -> ?flags:(unit, string list) Build.t
  -> ?preprocess:Dune_file.Preprocess.t
  -> ?libname:Lib_name.Local.t
  -> ?source_dirs: Path.Source.Set.t
  -> ?objs_dirs:Path.Set.t
  -> unit
  -> t

val add_source_dir : t -> Path.Source.t -> t

val merge_all : allow_approx_merlin:bool -> t list -> t option

(** Add rules for generating the .merlin in a directory *)
val add_rules
  : Super_context.t
  -> dir:Path.t
  -> more_src_dirs:Path.Source.t list
  -> expander:Expander.t
  -> dir_kind:Dune_lang.File_syntax.t
  -> t
  -> unit

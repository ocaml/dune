(** Merlin rules *)
open! Dune_engine

open! Stdune
open Import

(* Merlin file names and tools to manipulate them *)
val merlin_file_name : string

val merlin_exist_name : string

val make_lib_ident : Dune_file.Library.t -> string

val make_exe_ident : Dune_file.Executables.t -> string

val make_merlin_exists : ident:string -> string

type t

val add_source_dir : t -> Path.Source.t -> t

module Processed : sig
  type t

  val load_file : Path.t -> t option

  val print_file : Path.t -> unit

  val get : t -> filename:string -> Sexp.t option
end

val make :
     ?requires:Lib.t list Or_exn.t
  -> flags:Ocaml_flags.t
  -> ?preprocess:Preprocess.Without_instrumentation.t Preprocess.t
  -> ?libname:Lib_name.Local.t
  -> ?source_dirs:Path.Source.Set.t
  -> modules:Modules.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> dialects:Dialect.DB.t
  -> unit
  -> t

(** Add rules for generating the .merlin in a directory *)
val add_rules :
     Super_context.t
  -> ident:string
  -> dir:Path.Build.t
  -> more_src_dirs:Path.Source.t list
  -> expander:Expander.t
  -> t
  -> unit

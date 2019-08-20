open! Stdune
open Import
open Dune_file

val build_o_files :
     Buildable.t
  -> sctx:Super_context.t
  -> c_sources:C.Sources.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> requires:Lib.L.t Or_exn.t
  -> dir_contents:Dir_contents.t
  -> Path.Build.t list

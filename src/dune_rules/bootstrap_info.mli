(** Generate bootstrap info *)
open! Dune_engine

(** Generate an OCaml file containing a description of the dune sources for the
    bootstrap procedure *)

open Stdune

(** Generate the rules to handle the stanza *)
val gen_rules :
     Super_context.t
  -> Dune_file.Executables.t
  -> dir:Path.Build.t
  -> Lib.Compile.t
  -> unit

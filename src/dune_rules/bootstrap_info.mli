(** Generate bootstrap info *)

(** Generate an OCaml file containing a description of the dune sources for the
    bootstrap procedure *)

open Import

(** Generate the rules to handle the stanza *)
val gen_rules
  :  Super_context.t
  -> Executables.t
  -> dir:Path.Build.t
  -> Lib.Compile.t
  -> Dir_contents.t
  -> unit Memo.t

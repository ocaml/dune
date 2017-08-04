(** Utop rules *)

open Import

val exe_stanzas
  : Jbuild.Stanza.t list
  -> (Jbuild.Executables.t * Module.t String_map.t) option
(** Given a list of stanzas (from a directory with a jbuild file) return:
    1. a stanza for a utop toplevel with all the libraries linked in.
    2. an entry module that will be used to create the toplevel *)

val add_module_rules
  : Super_context.t
  -> dir:Path.t
  -> (unit, Lib.t list) Build.t
  -> unit
(** Add rules to generate a utop module that will all have all the include dirs
    for the dependencies *)

val utop_exe_dir : dir:Path.t -> Path.t
(** Return the directory in which the main module for the top level will be
    generated. *)

val utop_exe : Path.t -> Path.t
(** Return the path of the utop bytecode binary inside a directory where
    some libraries are defined. *)

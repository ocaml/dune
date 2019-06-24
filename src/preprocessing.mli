(** Preprocessing of OCaml source files *)

open! Stdune
open! Import

(** Preprocessing object *)
type t

val dummy : t

val make
  :  Super_context.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> dep_kind:Lib_deps_info.Kind.t
  -> lint:Dune_file.Preprocess_map.t
  -> preprocess:Dune_file.Preprocess_map.t
  -> preprocessor_deps:(unit, unit) Build.t
  -> lib_name:Lib_name.Local.t option
  -> scope:Scope.t
  -> dir_kind:Dune_lang.File_syntax.t
  -> t

(** Setup the preprocessing rules for the following modules and
    returns the translated modules *)
val pp_modules :  t -> ?lint:bool -> (Module.t -> Module.t)

(** Preprocess a single module, using the configuration for the given
    module name. *)
val pp_module_as
  :  t
  -> ?lint:bool
  -> Module.Name.t
  -> Module.t
  -> Module.t

(** Get a path to a cached ppx driver with some extra flags for cookies. *)
val get_ppx_driver
  :  Super_context.t
  -> loc:Loc.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> lib_name:Lib_name.Local.t option
  -> flags:String_with_vars.t list
  -> dir_kind:Dune_lang.File_syntax.t
  -> (Loc.t * Lib_name.t) list
  -> (Path.Build.t * string list) Or_exn.t

module Compat_ppx_exe_kind : sig
  (** [Dune] for directories using a [dune] file, and [Jbuild driver]
      for directories using a [jbuild] file. *)
  type t =
    | Dune
    | Jbuild of string option
end

(** Compatibility [ppx.exe] program for the findlib method. *)
val get_compat_ppx_exe
  :  Super_context.t
  -> name:Lib_name.t
  -> kind:Compat_ppx_exe_kind.t
  -> Path.Build.t

val gen_rules : Super_context.t -> string list -> unit

val chdir : Action_unexpanded.t -> Action_unexpanded.t

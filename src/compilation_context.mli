(** High-level API for compiling OCaml files *)

open Import

(** Represent a compilation context.

    A compilation context contains all the necessary information to
    preprocess and compile OCaml source files. Exactly one compilation
    context is associated to each library, executable and executbales
    stanza.
*)
type t

(** Create a compilation context. *)
val create
  :  super_context         : Super_context.t
  -> scope                 : Scope.t
  -> dir                   : Path.t
  -> dune_version          : Syntax.Version.t
  -> ?obj_dir              : Path.t
  -> modules               : Module.t Module.Name.Map.t
  -> ?alias_module         : Module.t
  -> ?lib_interface_module : Module.t
  -> flags                 : Ocaml_flags.t
  -> requires              : Lib.t list Or_exn.t
  -> ?preprocessing        : Preprocessing.t
  -> ?no_keep_locs         : bool
  -> unit
  -> t

(** Return a compilation context suitable for compiling the alias module. *)
val for_alias_module : t -> t

val super_context        : t -> Super_context.t
val scope                : t -> Scope.t
val dir                  : t -> Path.t
val dune_version         : t -> Syntax.Version.t
val obj_dir              : t -> Path.t
val modules              : t -> Module.t Module.Name.Map.t
val alias_module         : t -> Module.t option
val lib_interface_module : t -> Module.t option
val flags                : t -> Ocaml_flags.t
val requires             : t -> Lib.t list Or_exn.t
val includes             : t -> string list Arg_spec.t Cm_kind.Dict.t
val preprocessing        : t -> Preprocessing.t
val no_keep_locs         : t -> bool

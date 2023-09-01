open Stdune
open Dune_sexp

module Glob_files : sig
  (** A glob stored in a [String_with_vars.t] and functions for expanding the
      glob to a list of files, after resolving pforms in the
      [String_with_vars.t]. Globs can be recursive (indicated by the [recursive]
      field), meaning that all descendant directories of the starting directory
      will be searched for files matching the glob. *)
  type t =
    { glob : String_with_vars.t
    ; recursive : bool
    }

  val to_dyn : t -> Dyn.t
end

module Sandbox_config : sig
  type t

  val loc : t -> Loc.t

  val fold
    :  t
    -> f:([ `None | `Always | `Preserve_file_kind ] -> 'acc -> 'acc)
    -> init:'acc
    -> 'acc
end

type t =
  | File of String_with_vars.t
  | Alias of String_with_vars.t
  | Alias_rec of String_with_vars.t
  | Glob_files of Glob_files.t
  | Source_tree of String_with_vars.t
  | Package of String_with_vars.t
  | Universe
  | Env_var of String_with_vars.t
  (* [Sandbox_config] is a way to declare that your action also depends on there
     being a clean filesystem around its deps. (or, if you require
     [no_sandboxing], it's that your action depends on something undeclared
     (e.g. absolute path of cwd) and you want to allow it) *)
  | Sandbox_config of Sandbox_config.t
  | Include of string

val remove_locs : t -> t

include Conv.S with type t := t

val decode_no_files : t Decoder.t
val to_dyn : t Dyn.builder

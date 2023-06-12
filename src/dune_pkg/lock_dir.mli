(** Frontend the lock directory format *)

open Import
open Dune_lang

module Source : sig
  type t =
    | External_copy of Loc.t * Path.External.t
    | Fetch of
        { url : Loc.t * string
        ; checksum : (Loc.t * Checksum.t) option
        }
end

module Pkg_info : sig
  type t =
    { name : Package_name.t
    ; version : string
    ; dev : bool
    ; source : Source.t option
    }
end

module Pkg : sig
  type t =
    { build_command : Action.t option
    ; install_command : Action.t option
    ; deps : Package_name.t list
    ; info : Pkg_info.t
    ; lock_dir : Path.Source.t
    ; exported_env : String_with_vars.t Action.Env_update.t list
    }

  val decode :
    (lock_dir:Path.Source.t -> Package_name.t -> t) Dune_sexp.Decoder.t
end

type t =
  { version : Syntax.Version.t
  ; packages : Pkg.t Package_name.Map.t
  }

val remove_locs : t -> t

val equal : t -> t -> bool

val to_dyn : t -> Dyn.t

val create_latest_version : Pkg.t Package_name.Map.t -> t

val default_path : Path.Source.t

val metadata : Filename.t

module Metadata : Dune_sexp.Versioned_file.S with type data := unit

val write_disk : lock_dir_path:Path.Source.t -> t -> unit

val read_disk : lock_dir_path:Path.Source.t -> t Or_exn.t

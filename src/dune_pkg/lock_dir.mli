(** Frontend the lock directory format *)

open Import
open Dune_lang

module Source : sig
  type fetch =
    { url : Loc.t * string
    ; checksum : (Loc.t * Checksum.t) option
    }

  type t =
    | External_copy of Loc.t * Path.External.t
    | Fetch of fetch
end

module Pkg_info : sig
  type t =
    { name : Package_name.t
    ; version : string
    ; dev : bool
    ; source : Source.t option
    ; extra_sources : (Path.Local.t * Source.t) list
    }

  val default_version : string
end

module Pkg : sig
  type t =
    { build_command : Action.t option
    ; install_command : Action.t option
    ; deps : (Loc.t * Package_name.t) list
    ; info : Pkg_info.t
    ; exported_env : String_with_vars.t Action.Env_update.t list
    }

  val decode :
    (lock_dir:Path.Source.t -> Package_name.t -> t) Dune_sexp.Decoder.t
end

type t =
  { version : Syntax.Version.t
  ; packages : Pkg.t Package_name.Map.t
  ; ocaml : (Loc.t * Package_name.t) option
  }

val remove_locs : t -> t

val equal : t -> t -> bool

val to_dyn : t -> Dyn.t

val create_latest_version :
  Pkg.t Package_name.Map.t -> ocaml:(Loc.t * Package_name.t) option -> t

val default_path : Path.Source.t

val metadata : Filename.t

module Metadata : Dune_sexp.Versioned_file.S with type data := unit

module Package_filename : sig
  type t = Filename.t

  val of_package_name : Package_name.t -> t

  val to_package_name : t -> (Package_name.t, [ `Bad_extension ]) result
end

module Write_disk : sig
  type lock_dir := t

  type t

  val prepare : lock_dir_path:Path.Source.t -> lock_dir -> t

  val commit : t -> unit
end

val read_disk : Path.Source.t -> t

module Make_load (Io : sig
  include Monad.S

  val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

  val readdir_with_kinds : Path.Source.t -> (Filename.t * Unix.file_kind) list t

  val with_lexbuf_from_file : Path.Source.t -> f:(Lexing.lexbuf -> 'a) -> 'a t
end) : sig
  val load : Path.Source.t -> t Io.t
end

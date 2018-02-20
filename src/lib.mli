open Import

(** {1 Generals} *)

(** Representation of a library *)
type t

(** For libraries defined in the workspace, this is the [public_name] if
    present or the [name] if not. *)
val name : t -> string

(* CR-someday diml: this should be [Path.t list], since some libraries
   have multiple source directories because of [copy_files]. *)
(** Directory where the source files for the library are located. *)
val src_dir : t -> Path.t

(** Directory where the object files for the library are located. *)
val obj_dir : t -> Path.t

(** Same as [Path.is_local (obj_dir t)] *)
val is_local : t -> bool

val synopsis     : t -> string option
val kind         : t -> Jbuild.Library.Kind.t
val archives     : t -> Path.t list Mode.Dict.t
val plugins      : t -> Path.t list Mode.Dict.t
val jsoo_runtime : t -> Path.t list

module Status : sig
  type t =
    | Installed
    | Public
    | Private of Jbuild.Scope_info.Name.t
end

val status : t -> Status.t

(** Operations on list of libraries *)
module L : sig
  type nonrec t = t list

  val include_paths : t -> stdlib_dir:Path.t -> Path.Set.t
  val include_flags : t -> stdlib_dir:Path.t -> _ Arg_spec.t

  val c_include_flags : t -> stdlib_dir:Path.t -> _ Arg_spec.t

  val link_flags : t -> mode:Mode.t -> stdlib_dir:Path.t -> _ Arg_spec.t

  (** All the library archive files (.a, .cmxa, _stubs.a, ...)  that
      should be linked in when linking an executable. *)
  val archive_files : t -> mode:Mode.t -> ext_lib:string -> Path.t list

  val jsoo_runtime_files : t -> Path.t list

  val remove_dups : t -> t
end

(** {1 Raw library descriptions} *)

(** Information about a library *)
module Info : sig
  module Deps : sig
    type t =
      | Simple  of string list
      | Complex of Jbuild.Lib_dep.t list
  end

  (** Raw description of a library, where dependencies are not
      resolved. *)
  type t =
    { loc              : Loc.t
    ; kind             : Jbuild.Library.Kind.t
    ; status           : Status.t
    ; src_dir          : Path.t
    ; obj_dir          : Path.t
    ; version          : string option
    ; synopsis         : string option
    ; archives         : Path.t list Mode.Dict.t
    ; plugins          : Path.t list Mode.Dict.t
    ; foreign_archives : Path.t list Mode.Dict.t (** [.a/.lib/...] files *)
    ; jsoo_runtime     : Path.t list
    ; requires         : Deps.t
    ; ppx_runtime_deps : string list
    ; pps              : Jbuild.Pp.t list
    ; optional         : bool
    }

  val of_library_stanza : dir:Path.t -> Jbuild.Library.t -> t
  val of_findlib_package : Findlib.Package.t -> t
end

(** {1 Errors} *)

module Error : sig
  module Library_not_available : sig
    module Reason : sig
      module Hidden : sig
        type t =
          { name   : string
          ; info   : Info.t
          ; reason : string
          }
      end

      type t =
        | Not_found
        | Hidden of Hidden.t

      val to_string : t -> string
      val pp : Format.formatter -> t -> unit
    end

    type nonrec t =
      { name   : string
      ; reason : Reason.t
      }
  end

  module No_solution_found_for_select : sig
    type t = { loc : Loc.t }
  end

  module Conflict : sig
    (** When two libraries in a transitive closure conflict *)
    type nonrec t =
      { lib1 : t * With_required_by.Entry.t list
      ; lib2 : t * With_required_by.Entry.t list
      }
  end

  type t =
    | Library_not_available        of Library_not_available.t
    | No_solution_found_for_select of No_solution_found_for_select.t
    | Dependency_cycle             of (Path.t * string) list
    | Conflict                     of Conflict.t
end

exception Error of Error.t With_required_by.t

(** Raise a error about a library that is not available *)
val not_available
  :  loc:Loc.t
  -> Error.Library_not_available.Reason.t
  -> ('a, Format.formatter, unit, 'b) format4
  -> 'a

(** {1 Library compilation} *)

(** For compiling the library itself *)
module Compile : sig
  (** Return the list of dependencies needed for compiling this library *)
  val requires : t -> (L.t, Error.t With_required_by.t) result

  module Resolved_select : sig
    type t =
      { src_fn : (string, Error.No_solution_found_for_select.t) result
      ; dst_fn : string
      }
  end

  (** Resolved select forms *)
  val resolved_selects : t -> Resolved_select.t list
end

(** {1 Library name resolution} *)

(** Collection of libraries organized by names *)
module DB : sig
  type lib = t

  (** A database allow to resolve library names *)
  type t

  module Info_or_redirect : sig
    type nonrec t =
      | Info     of Info.t
      | Redirect of Loc.t * Path.t * string
      | Proxy    of lib
  end

  (** Create a new library database. [resolve] is used to resolve
      library names in this database.

      When a library is not found, it is looked up in the parent
      database if any.

      [all] returns the list of names of libraries available in this database.
  *)
  val create
    :  ?parent:t
    -> resolve:(string ->
                (Info_or_redirect.t, Error.Library_not_available.Reason.t)
                  result)
    -> all:(unit -> string list)
    -> unit
    -> t

  (** Create a database from a list of library stanzas *)
  val create_from_library_stanzas
    :  ?parent:t
    -> (Path.t * Jbuild.Library.t) list
    -> t

  val create_from_findlib : Findlib.t -> t

  val find : t -> string -> (lib, Error.Library_not_available.Reason.t) result
  val find_exn
    :  t
    -> string
    -> required_by:With_required_by.Entry.t list
    -> lib

  val available : t -> string -> bool

  (** Resolve libraries written by the user in a jbuild file. The
      resulting list of libraries is transitively closed and sorted by
      order of dependencies.

      This function is for executables stanzas.  *)
  val resolve_user_written_deps
    :  t
    -> Jbuild.Lib_dep.t list
    -> pps:Jbuild.Pp.t list
    -> (L.t, Error.t With_required_by.t) result *
       Compile.Resolved_select.t list

  val resolve_pps
    :  t
    -> Jbuild.Pp.t list
    -> (L.t, Error.t With_required_by.t) result

  (** Return the list of all libraries in this database. If
      [recursive] is true, also include libraries in parent databases
      recursively. *)
  val all : ?recursive:bool -> t -> lib list
end with type lib := t

(** {1 Transitive closure} *)

val closure : L.t -> (L.t, Error.t With_required_by.t) result

(** {1 Dependencies for META files} *)

module Meta : sig
  val requires
    :  t
    -> required_by:With_required_by.Entry.t list
    -> String_set.t

  val ppx_runtime_deps
    :  t
    -> required_by:With_required_by.Entry.t list
    -> String_set.t

  val ppx_runtime_deps_for_deprecated_method
    :  t
    -> required_by:With_required_by.Entry.t list
    -> String_set.t
end

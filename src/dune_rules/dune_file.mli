(** Representation and parsing of Dune files *)

open Import

module Executables : sig
  module Link_mode : sig
    type t =
      | Byte_complete
      | Other of
          { mode : Mode_conf.t
          ; kind : Binary_kind.t
          }

    include Dune_lang.Conv.S with type t := t

    val exe : t
    val object_ : t
    val shared_object : t
    val byte : t
    val native : t
    val js : t
    val compare : t -> t -> Ordering.t
    val to_dyn : t -> Dyn.t

    val extension
      :  t
      -> loc:Loc.t
      -> ext_obj:Filename.Extension.t
      -> ext_dll:Filename.Extension.t
      -> string

    module Map : Map.S with type key = t
  end

  type t =
    { names : (Loc.t * string) list
    ; link_flags : Link_flags.Spec.t
    ; link_deps : Dep_conf.t list
    ; modes : Loc.t Link_mode.Map.t
    ; optional : bool
    ; buildable : Buildable.t
    ; package : Package.t option
    ; promote : Rule.Promote.t option
    ; install_conf : Install_conf.t option
    ; embed_in_plugin_libraries : (Loc.t * Lib_name.t) list
    ; forbidden_libraries : (Loc.t * Lib_name.t) list
    ; bootstrap_info : string option
    ; enabled_if : Blang.t
    ; dune_version : Dune_lang.Syntax.Version.t
    }

  include Stanza.S with type t := t

  (** Check if the executables have any foreign stubs or archives. *)
  val has_foreign : t -> bool

  (** Check if the executables have any c++ foreign stubs. *)
  val has_foreign_cxx : t -> bool

  val obj_dir : t -> dir:Path.Build.t -> Path.Build.t Obj_dir.t
end

module Documentation : sig
  type t =
    { loc : Loc.t
    ; package : Package.t
    ; mld_files : Ordered_set_lang.t
    }

  include Stanza.S with type t := t
end

module Tests : sig
  type t =
    { exes : Executables.t
    ; locks : Locks.t
    ; package : Package.t option
    ; deps : Dep_conf.t Bindings.t
    ; enabled_if : Blang.t
    ; build_if : Blang.t
    ; action : Dune_lang.Action.t option
    }

  include Stanza.S with type t := t
end

module Include_subdirs : sig
  type qualification =
    | Unqualified
    | Qualified

  type t =
    | No
    | Include of qualification

  type stanza = Loc.t * t

  include Stanza.S with type t := stanza
end

(** The purpose of [Library_redirect] stanza is to create a redirection from an
    [old_name] to a [new_public_name].

    This is used in two cases:

    - When a library changes its public name, a redirection is created for
      backwards compatibility with the code using its old name.
      (deprecated_library_name stanza in dune files)

    - When hiding public libraries with [--only-packages] (or [-p]), we use this
      stanza to make sure that their project-local names remain in scope. *)
module Library_redirect : sig
  type 'old_name t = private
    { project : Dune_project.t
    ; loc : Loc.t
    ; old_name : 'old_name
    ; new_public_name : Loc.t * Lib_name.t
    }

  module Local : sig
    type nonrec t = (Loc.t * Lib_name.Local.t) t

    include Stanza.S with type t := t

    val of_private_lib : Library.t -> t option
  end
end

module Deprecated_library_name : sig
  module Old_name : sig
    type deprecation =
      | Not_deprecated
      | Deprecated of { deprecated_package : Package.Name.t }

    type t = Public_lib.t * deprecation
  end

  type t = Old_name.t Library_redirect.t

  include Stanza.S with type t := t

  val old_public_name : t -> Lib_name.t
end

val stanza_package : Stanza.t -> Package.t option

(** [of_ast project ast] is the list of [Stanza.t]s derived from decoding the
    [ast] according to the syntax given by [kind] in the context of the
    [project] *)
val of_ast : Dune_project.t -> Dune_lang.Ast.t -> Stanza.t list

(** A fully evaluated dune file *)
type t =
  { dir : Path.Source.t
  ; project : Dune_project.t
  ; stanzas : Stanza.t list
  }

val equal : t -> t -> bool
val hash : t -> int
val to_dyn : t -> Dyn.t

val parse
  :  Dune_lang.Ast.t list
  -> dir:Path.Source.t
  -> file:Path.Source.t option
  -> project:Dune_project.t
  -> t Memo.t

val fold_stanzas : t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc

module Memo_fold : sig
  val fold_stanzas
    :  t list
    -> init:'acc
    -> f:(t -> Stanza.t -> 'acc -> 'acc Memo.t)
    -> 'acc Memo.t
end

open Import

(** The purpose of [Library_redirect] stanza is to create a redirection from an
    [old_name] to a [new_public_name].

    This is used in two cases:

    - When a library changes its public name, a redirection is created for
      backwards compatibility with the code using its old name.
      (deprecated_library_name stanza in dune files)

    - When hiding public libraries with [--only-packages] (or [-p]), we use this
      stanza to make sure that their project-local names remain in scope. *)

type 'old_name t =
  { project : Dune_project.t
  ; loc : Loc.t
  ; old_name : 'old_name
  ; new_public_name : Loc.t * Lib_name.t
  }

module Local : sig
  type info =
    { lib_name : Loc.t * Lib_name.Local.t
    ; enabled : Blang.t
    }

  type nonrec t = info t

  include Stanza.S with type t := t

  val of_private_lib : Library.t -> t option
  val of_lib : Library.t -> t option
  val to_lib_id : src_dir:Path.Source.t -> t -> Lib_id.Local.t
end

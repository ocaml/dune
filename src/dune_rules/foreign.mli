open Import

(* CR-soon cwong: I'd really prefer to keep the convention that these functions
   are spelled [Foreign_language.encode] and [Foreign_language.decode], but due
   to some organizational reasons (see the rant at the top of
   [foreign_language.mli]), these need to be here instead, as they cannot reside
   in the backend. *)

(** Foreign archives appear in the [(foreign_archives ...)] field of libraries
    and executables, for example [(foreign_archives some/dir/lib)]. When parsing
    such fields, we separate the directory [some/dir] from the name [lib] of the
    archive and store them as a [{dir : Dir.t; name : Name.t}] record. *)
module Archive : sig
  (** Archive names appear as part of the [(foreign_archives ...)] fields, as
      well as in the [(archive_name ...)] field of the [(foreign_library ...)]
      stanza. For example, both [(foreign_archives some/dir/lib)] and
      [(archive_name lib)] specify the same archive name [lib]. Archive names
      are not allowed to contain path separators. *)
  module Name : sig
    type t

    module Map : Map.S with type key = t

    val to_dyn : t -> Dyn.t
    val to_string : t -> string
    val path : dir:Path.Build.t -> mode:Mode.Select.t -> t -> Path.Build.t
    val decode : t Dune_lang.Decoder.t
    val stubs : string -> t
    val lib_file_prefix : string

    val lib_file
      :  t
      -> dir:Path.Build.t
      -> ext_lib:Filename.Extension.t
      -> mode:Mode.Select.t
      -> Path.Build.t

    val dll_file
      :  t
      -> dir:Path.Build.t
      -> ext_dll:Filename.Extension.t
      -> mode:Mode.Select.t
      -> Path.Build.t
  end

  type t

  val dir_path : dir:Path.Build.t -> t -> Path.Build.t
  val name : mode:Mode.Select.t -> t -> Name.t
  val stubs : string -> t
  val decode : t Dune_lang.Decoder.t

  val lib_file
    :  archive:t
    -> dir:Path.Build.t
    -> ext_lib:Filename.Extension.t
    -> mode:Mode.Select.t
    -> Path.Build.t

  val dll_file
    :  archive:t
    -> dir:Path.Build.t
    -> ext_dll:Filename.Extension.t
    -> mode:Mode.Select.t
    -> Path.Build.t
end

(** A type of foreign library "stubs", which includes all fields of the
    [Library.t] type except for the [archive_name] field. The type is parsed as
    an optional [foreign_stubs] field of the [library] stanza, or as part of the
    top-level [foreign_library] stanza. *)
module Stubs : sig
  (* Foreign sources can depend on a directly specified directory [Dir] or on a
     source directory of a library [Lib]. *)
  module Include_dir : sig
    module Without_include : sig
      type t =
        | Dir of String_with_vars.t
        | Lib of Loc.t * Lib_name.t
    end

    type t

    val decode : t Dune_lang.Decoder.t

    val expand_include
      :  t
      -> expand:(String_with_vars.t -> Value.t Memo.t)
      -> dir:Path.t
      -> Without_include.t list Memo.t
  end

  type t =
    { loc : Loc.t
    ; language : Foreign_language.t
    ; names : Ordered_set_lang.t
    ; mode : Mode.Select.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; include_dirs : Include_dir.t list
    ; extra_deps : Dep_conf.t list
    }

  (** Construct foreign library stubs with some fields set to default values. *)
  val make
    :  loc:Loc.t
    -> language:Foreign_language.t
    -> names:Ordered_set_lang.t
    -> flags:Ordered_set_lang.Unexpanded.t
    -> t

  val decode_stubs : for_library:bool -> t Dune_lang.Decoder.fields_parser
  val decode : t Dune_lang.Decoder.t
  val is_mode_dependent : t -> bool
end

(** A foreign source file that has a [path] and all information of the
    corresponding [Foreign.Stubs.t] declaration. *)
module Source : sig
  type kind =
    | Stubs of Stubs.t
    | Ctypes of Ctypes_field.t

  type t

  val kind : t -> kind
  val language : t -> Foreign_language.t
  val mode : t -> Mode.Select.t
  val path : t -> Path.Build.t

  (** The name of the corresponding object file; for example, [name] for a
      source file [some/path/name.cpp] of [name_mode] if the stub is
      mode-specific. *)
  val object_name : t -> Filename.t

  (** The name of the corresponding object file without the mode suffix. This is
      useful for messages where the internally suffixed name would be confusing. *)
  val user_object_name : t -> Filename.t

  val make : kind -> path:Path.Build.t -> t
end

(** A map from object names to the corresponding sources. *)
module Sources : sig
  type t

  val to_list_map : t -> f:(string -> Loc.t * Source.t -> 'b) -> 'b list
  val make : (Loc.t * Source.t) String.Map.t -> t

  val object_files
    :  t
    -> dir:Path.Build.t
    -> ext_obj:Filename.Extension.t
    -> Path.Build.t list

  val has_cxx_sources : t -> bool
end

(** For the [(foreign_objects ...)] field.*)
module Objects : sig
  type t

  val empty : t
  val is_empty : t -> bool
  val decode : t Dune_lang.Decoder.t
  val build_paths : t -> ext_obj:Filename.Extension.t -> dir:Path.Build.t -> Path.t list
end

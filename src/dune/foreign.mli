open Stdune

module Language : sig
  type t =
    | C
    | Cxx

  val compare : t -> t -> ordering

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t

  (** The proper name of a language, e.g. "C++" for [Cxx]. Useful for
      diagnostic messages. *)
  val proper_name : t -> string

  (** The string used to encode a language in Dune files, e.g. "cxx" for [Cxx]. *)
  val encode : t -> string

  val decode : t Dune_lang.Decoder.t

  module Map : sig
    include Map.S with type key = t
  end

  module Dict : sig
    type language

    type 'a t =
      { c : 'a
      ; cxx : 'a
      }

    val c : 'a t -> 'a

    val cxx : 'a t -> 'a

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val mapi : 'a t -> f:(language:language -> 'a -> 'b) -> 'b t

    val make_both : 'a -> 'a t

    val make : c:'a -> cxx:'a -> 'a t

    val update : 'a t -> language -> f:('a -> 'a) -> 'a t

    val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

    val get : 'a t -> language -> 'a
  end
  with type language := t
end

val header_extension : string

val has_foreign_extension : fn:string -> bool

val drop_source_extension :
     string
  -> dune_version:Dune_lang.Syntax.Version.t
  -> (string * Language.t) option

val possible_sources :
     language:Language.t
  -> string
  -> dune_version:Dune_lang.Syntax.Version.t
  -> string list

(** A type of foreign library "stubs", which includes all fields of the
    [Library.t] type except for the [archive_name] field. The type is parsed as
    an optional [foreign_stubs] field of the [library] stanza, or as part of
    the top-level [foreign_library] stanza. *)
module Stubs : sig
  (* Foreign sources can depend on a directly specified directory [Dir] or on a
     source directory of a library [Lib]. *)
  module Include_dir : sig
    type t =
      | Dir of String_with_vars.t
      | Lib of Loc.t * Lib_name.t

    val decode : t Dune_lang.Decoder.t
  end

  type t =
    { loc : Loc.t
    ; language : Language.t
    ; names : Ordered_set_lang.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; include_dirs : Include_dir.t list
    ; extra_deps : Dep_conf.t list
    }

  (** Construct foreign library stubs with some fields set to default values. *)
  val make :
       loc:Loc.t
    -> language:Language.t
    -> names:Ordered_set_lang.t
    -> flags:Ordered_set_lang.Unexpanded.t
    -> t

  val decode : t Dune_lang.Decoder.t
end

(** Foreign libraries.

    This data type represents the contents of the top-level stanza
    [foreign_library].

    The fields have the following semantics.

    [language] selects the compiler. At the moment, we support only [c] and
    [cxx] settings, but in future other languages/compilers could be supported,
    e.g. Rust and Clang.

    [archive_name] determines the names of the resulting [.a] archive files.

    [names] are names of source files. The full paths to the files are
    determined by scanning package directories. Duplicate file names are
    disallowed to avoid conflicting object names in the resulting archive file.

    [flags] are passed when compiling source files.

    [include_dirs] are tracked as dependencies and passed to the compiler via
    the "-I" flag.

    [extra_deps] are tracked as dependencies. *)
module Library : sig
  type t =
    { archive_name : string
    ; archive_name_loc : Loc.t
    ; stubs : Stubs.t
    }

  val decode : t Dune_lang.Decoder.t
end

val lib_file :
  archive_name:string -> dir:Path.Build.t -> ext_lib:string -> Path.Build.t

val dll_file :
  archive_name:string -> dir:Path.Build.t -> ext_dll:string -> Path.Build.t

(** A foreign source file that has a [path] and all information of the
    corresponnding [Foreign.Stubs.t] declaration. *)
module Source : sig
  type t =
    { stubs : Stubs.t
    ; path : Path.Build.t
    }

  val language : t -> Language.t

  val flags : t -> Ordered_set_lang.Unexpanded.t

  val path : t -> Path.Build.t

  val make : stubs:Stubs.t -> path:Path.Build.t -> t
end

(** A map from object names to the corresponding sources. *)
module Sources : sig
  type t = (Loc.t * Source.t) String.Map.t

  val object_files :
    t -> dir:Path.Build.t -> ext_obj:string -> Path.Build.t list

  (** A map from object names to lists of possible language/path combinations. *)
  module Unresolved : sig
    type t = (Language.t * Path.Build.t) String.Map.Multi.t

    val to_dyn : t -> Dyn.t

    (** [load ~dir ~files] loads foreign sources in [dir] into a map keyed by
        the object name. *)
    val load :
         dune_version:Dune_lang.Syntax.Version.t
      -> dir:Path.Build.t
      -> files:String.Set.t
      -> t
  end
end

open Stdune

val header_ext : string

val c_cxx_or_header : fn:string -> bool

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

  type split =
    | Unrecognized
    | Not_allowed_until of Dune_lang.Syntax.Version.t
    | Recognized of string * t

  val split_extension :
    string -> dune_version:Dune_lang.Syntax.Version.t -> split

  (** [possible_fns t s] returns the possible filenames given the
      extension-less basenames [s] *)
  val possible_fns :
    t -> string -> dune_version:Dune_lang.Syntax.Version.t -> string list

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

(** A type of foreign library "stubs", which includes all fields of the
    [Library.t] type except for the [archive_name] field. The type is parsed as
    an optional [foreign_stubs] field of the [library] stanza, or as part of
    the top-level [foreign_library] stanza. *)
module Stubs : sig
  type t =
    { loc : Loc.t
    ; language : Language.t
    ; names : Ordered_set_lang.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; include_dirs : Loc.t * Ordered_set_lang.Unexpanded.t
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
    ; stubs : Stubs.t
    }

  val decode : t Dune_lang.Decoder.t
end

val lib_file :
  archive_name:string -> dir:Path.Build.t -> ext_lib:string -> Path.Build.t

val dll_file :
  archive_name:string -> dir:Path.Build.t -> ext_dll:string -> Path.Build.t

module Source : sig
  type t

  val language : t -> Language.t

  val flags : t -> Ordered_set_lang.Unexpanded.t

  val path : t -> Path.Build.t

  val make : stubs:Stubs.t -> path:Path.Build.t -> t
end

(** A map from object names to the corresponding sources. *)
module Object_map : sig
  type t = Path.Build.t Language.Map.t String.Map.t

  val to_dyn : t -> Dyn.t

  (** [load ~dir ~files] loads foreign sources in [dir] into a two-layer map
      whose first layer is keyed by the object name and the second layer is
      keyed by the language of the source. *)
  val load :
       dune_version:Dune_lang.Syntax.Version.t
    -> dir:Path.Build.t
    -> files:String.Set.t
    -> t
end

module Sources : sig
  type t = (Loc.t * Source.t) String.Map.t

  val objects : t -> dir:Path.Build.t -> ext_obj:string -> Path.Build.t list
end

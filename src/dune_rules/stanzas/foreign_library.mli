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

open Import

type t =
  { archive_name : Foreign.Archive.Name.t
  ; archive_name_loc : Loc.t
  ; stubs : Foreign.Stubs.t
  ; enabled_if : Blang.t
  }

val decode : t Dune_lang.Decoder.t

include Stanza.S with type t := t

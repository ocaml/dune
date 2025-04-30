open Import

(** Foreign libraries.

    This data type represents the contents of the top-level stanza [foreign_library].

    The fields have the following semantics:

    - [archive_name] determines the names of the resulting [.a] archive files.
    
    - [archive_name_loc] is the location of the [archive_name] field in the
      Dune file. This is used for error reporting.

    - [names] are names of source files. The full paths to the files are determined by
      scanning package directories. Duplicate file names are disallowed to avoid
      conflicting object names in the resulting archive file. 
    
    - [stubs] collects the language-specific compilation options.
    
    - [enabled_if] is a condition that determines whether the stanza is enabled.
    
    - [extra_objects] are additional object files to be linked in the archive. *)
type t =
  { archive_name : Foreign.Archive.Name.t
  ; archive_name_loc : Loc.t
  ; stubs : Foreign.Stubs.t
  ; enabled_if : Blang.t
  ; extra_objects : Ordered_set_lang.Unexpanded.t
  }

val decode : t Dune_lang.Decoder.t

include Stanza.S with type t := t

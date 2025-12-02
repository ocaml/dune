open Stdune

type using_decl =
  { name : Loc.t * string
  ; version : Loc.t * string
  }

(** Scan file contents for (using ...) declarations *)
val scan : string -> string -> using_decl list

(** Check if a string contains non-ASCII characters *)
val has_non_ascii : string -> bool

(** Check if a version string has valid format (X.Y optionally followed by extra chars) *)
val is_valid_version_format : string -> bool

(** Validate a version string - returns true if invalid (contains non-ASCII or wrong format) *)
val is_invalid_version : string -> bool

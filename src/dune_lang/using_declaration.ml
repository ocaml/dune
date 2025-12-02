open Import

(* Re-export the type from the parser *)
type using_decl = Using_declaration_parser.using_decl =
  { name : Loc.t * string
  ; version : Loc.t * string
  }

(* Scan file contents for (using ...) declarations *)
let scan contents fname = Using_declaration_parser.scan contents fname

(* Check if a string contains non-ASCII characters *)
let has_non_ascii s = String.exists s ~f:(fun c -> Char.code c > 127)

(* Check if a version string has valid format (X.Y optionally followed by extra chars) *)
let is_valid_version_format ver_str =
  match Scanf.sscanf ver_str "%u.%u%s" (fun a b s -> (a, b), s) with
  | Ok (_, "") -> true
  | Ok (_, _) -> true (* has trailing chars, but base format is valid *)
  | Error () -> false
;;

(* Validate a version string - returns true if invalid (needs error) *)
let is_invalid_version ver_str =
  has_non_ascii ver_str || not (is_valid_version_format ver_str)
;;

(** Test library that references odoc-parser types.

    This library demonstrates cross-package documentation references.

    See {{!/odoc/odoc_for_authors}the odoc author guide} for documentation syntax. *)

(** A wrapper around odoc-parser results.
    Uses {!Odoc_parser.t} internally. *)
type t

(** Parse a documentation comment.
    See {!Odoc_parser.parse_comment} for details on the parsing behavior.
    Warnings are represented as {!Odoc_parser.Warning.t}. *)
val parse : string -> t

(** Get the location of a parsed element.
    Returns a {!Odoc_parser.Loc.type-span}. *)
val get_location : t -> Odoc_parser.Loc.span

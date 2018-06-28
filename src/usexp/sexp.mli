type t = Types.Sexp.t =
  | Atom of Atom.t
  | Quoted_string of string
  | List of t list
  | Template of Types.Template.t

val atom_or_quoted_string : string -> t

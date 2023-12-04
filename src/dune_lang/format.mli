open Stdune
open Dune_sexp

type dune_file =
  | OCaml_syntax of Loc.t
  | Sexps of Cst.t list

val format_string : version:Syntax.Version.t -> string -> string
val parse : Lexing.lexbuf -> dune_file

(** Reformat a dune file in a dune action *)
val format_action : version:Syntax.Version.t -> src:Path.t -> dst:Path.Build.t -> unit

(** Pretty-print a list of toplevel s-expressions *)
val pp_top_sexps : version:Syntax.Version.t -> Cst.t list -> _ Pp.t

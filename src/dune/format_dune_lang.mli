open Import

type dune_file =
  | OCaml_syntax of Loc.t
  | Sexps of Dune_lang.Cst.t list

val parse_file : Path.t option -> dune_file
(** Read a file into its concrete syntax *)

val write_file : path:Path.t -> Dune_lang.Cst.t list -> unit
(** Write the formatted concrete syntax to the file at [path] *)

val format_file : input:Path.t option -> unit
(** Reformat a dune file. [None] corresponds to stdin. *)

val pp_top_sexps : Format.formatter -> Dune_lang.Cst.t list -> unit
(** Pretty-print a list of toplevel s-expressions *)

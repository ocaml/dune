open Import

type dune_file =
  | OCaml_syntax of Loc.t
  | Sexps of Dune_lang.Cst.t list

(** Read a file into its concrete syntax *)
val parse_file : Path.t option -> dune_file

(** Write the formatted concrete syntax to the file at [path] *)
val write_file :
     version:Dune_lang.Syntax.Version.t
  -> path:Path.t
  -> Dune_lang.Cst.t list
  -> unit

(** Reformat a dune file. [None] in [input] corresponds to stdin. [None] in
    [output] corresponds to stdout. *)
val format_file :
     version:Dune_lang.Syntax.Version.t
  -> input:Path.t option
  -> output:Path.t option
  -> unit

(** Pretty-print a list of toplevel s-expressions *)
val pp_top_sexps :
  version:Dune_lang.Syntax.Version.t -> Dune_lang.Cst.t list -> _ Pp.t

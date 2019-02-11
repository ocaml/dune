open Import

(** Reformat a dune file. [None] corresponds to stdin. *)
val format_file : input:Path.t option -> unit

(** Pretty-print a list of toplevel s-expressions *)
val pp_top_sexps : Format.formatter -> Dune_lang.Cst.t list -> unit

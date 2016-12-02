(** OCaml binaries *)

(** Contents of [PATH] *)
val path : Path.t list

val parse_path : string -> Path.t list

(** The opam tool *)
val opam : Path.t option

(** Look for a program in the PATH *)
val which : ?path:Path.t list -> string -> Path.t option

(** Return the .opt version of a tool if available. If the tool is not available at all in
    the given directory, returns [None]. *)
val best_prog : Path.t -> string -> Path.t option

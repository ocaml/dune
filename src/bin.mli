(** OCaml binaries *)

(** Directory where the compiler and other tools are installed *)
val dir : string

(** Tools *)
val ocamlc : string
val ocamlopt : string option
val ocamldep : string
val ocamllex : string

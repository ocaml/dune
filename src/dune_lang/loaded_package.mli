type t

type opam_file =
  | Exists of bool
  | Generated

val has_opam_file : t -> opam_file
val package : t -> Package.t

val create : package:Package.t -> has_opam_file:opam_file -> t

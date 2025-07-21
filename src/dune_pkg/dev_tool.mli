open! Import

type t =
  | Ocamlformat
  | Odoc
  | Ocamllsp
  | Utop
  | Ocamlearlybird

val to_dyn : t -> Dyn.t
val all : t list
val equal : t -> t -> bool
val package_name : t -> Package_name.t
val of_package_name : Package_name.t -> t
val exe_name : t -> string

(** Returns the path to this tool's executable relative to the root of
    this tool's package directory *)
val exe_path_components_within_package : t -> string list

val needs_to_build_with_same_compiler_as_project : t -> bool

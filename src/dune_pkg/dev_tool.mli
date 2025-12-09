type t =
  | Ocamlformat
  | Odoc
  | Ocamllsp
  | Utop
  | Ocamlearlybird
  | Odig
  | Opam_publish
  | Dune_release
  | Ocaml_index
  | Merlin

val to_dyn : t -> Dyn.t
val all : t list
val equal : t -> t -> bool
val hash : t -> int
val package_name : t -> Package_name.t
val of_package_name : Package_name.t -> t
val exe_name : t -> string

(** Returns the path to this tool's executable relative to the root of
    this tool's package directory *)
val exe_path_components_within_package : t -> string list

val needs_to_build_with_same_compiler_as_project : t -> bool

(** Contains a list of compiler packages used to detect the compiler and
  determine its version *)
val compiler_package_names : Package_name.t list

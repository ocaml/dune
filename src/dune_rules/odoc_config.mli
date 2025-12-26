(** Configuration for odoc documentation generation per package.

    Packages can provide a doc/{package}/odoc-config.sexp file that specifies
    additional dependencies for documentation generation. *)

open Import

type deps =
  { packages : Package.Name.t list (** Additional packages to link against *)
  ; libraries : Lib_name.t list (** Additional libraries to link against *)
  }

type t = { deps : deps }

(** Empty configuration with no additional dependencies *)
val empty : t

(** Load configuration from a file. Returns [empty] if the file doesn't exist
    or fails to parse. *)
val load : Path.t -> t

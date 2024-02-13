(** Information about a package defined in the workspace *)

open Import
include module type of Dune_lang.Package with type t = Dune_lang.Package.t

(** Construct a package description from an opam file and its contents *)
val load_opam_file_with_contents : contents:string -> Path.Source.t -> Name.t -> t

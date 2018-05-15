(** dune-project files *)

open Import

module Lang : sig
  type t =
    | Jbuilder
    | Dune of Syntax.Version.t
end

type t =
  { lang     : Lang.t
  ; name     : string
  ; root     : Path.t
  ; version  : string option
  ; packages : Package.t Package.Name.Map.t
  }

(** Load a project description from the following directory. [files]
    is the set of files in this directory. *)
val load : dir:Path.t -> files:String.Set.t -> t option

(** "dune-project" *)
val filename : string

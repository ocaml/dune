(** dune-project files *)

open Import

module Lang : sig
  type t =
    | Jbuilder
    | Dune of Syntax.Version.t
end

module Name : sig
  (** Invariants:
      - Named     s -> s <> "" and s does not contain '.' or '/'
      - Anonymous p -> p is a local path in the source tree
  *)
  type t = private
    | Named     of string
    | Anonymous of Path.t

  val compare : t -> t -> Ordering.t

  (** Convert to a string that is suitable for human readable messages *)
  val to_string_hum : t -> string

  val sexp_of_t : t -> Sexp.t

  (** Convert to/from an encoded string that is suitable to use in filenames *)
  val encode : t -> string
  val decode : string -> t
end

type t =
  { lang     : Lang.t
  ; name     : Name.t
  ; root     : Path.t
  ; version  : string option
  ; packages : Package.t Package.Name.Map.t
  }

(** Load a project description from the following directory. [files]
    is the set of files in this directory. *)
val load : dir:Path.t -> files:String.Set.t -> t option

(** "dune-project" *)
val filename : string

(** Represent the scope at the root of the workspace when the root of
    the workspace contains no [dune-project] or [<package>.opam] files. *)
val anonymous : t

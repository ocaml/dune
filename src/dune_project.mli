(** dune-project files *)

open Import

module Kind : sig
  type t =
    | Dune
    | Jbuilder
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

module Project_file : sig
  type t
end

(* CR-soon diml: make this abstract *)
type t = private
  { kind          : Kind.t
  ; name          : Name.t
  ; root          : Path.Local.t
  ; version       : string option
  ; packages      : Package.t Package.Name.Map.t
  ; stanza_parser : Stanza.t list Sexp.Of_sexp.t
  ; project_file  : Project_file.t
  }

module Lang : sig
  (** [register id stanzas_parser] register a new language. Users will
      select this language by writing:

      {[ (lang <name> <version>) ]}

      as the first line of their [dune-project] file. [stanza_parsers]
      defines what stanzas the user can write in [dune] files. *)
  val register : Syntax.t -> Stanza.Parser.t list -> unit
end

module Extension : sig
  (** [register id parser] registers a new extension. Users will
      enable this extension by writing:

      {[ (using <name> <version> <args>) ]}

      in their [dune-project] file. [parser] is used to describe
      what [<args>] might be. *)
  val register : Syntax.t -> Stanza.Parser.t list Sexp.Of_sexp.t -> unit
end

(** Load a project description from the following directory. [files]
    is the set of files in this directory. *)
val load : dir:Path.t -> files:String.Set.t -> t option

(** Read the [name] file from a dune-project file *)
val read_name : Path.t -> string option

(** "dune-project" *)
val filename : string

(** Represent the scope at the root of the workspace when the root of
    the workspace contains no [dune-project] or [<package>.opam] files. *)
val anonymous : t Lazy.t

(** Check that the dune-project file exists and create it otherwise. *)
val ensure_project_file_exists : t -> unit

(** Append the following text to the project file *)
val append_to_project_file : t -> string -> unit

(** Set the project we are currently parsing dune files for *)
val set : t -> ('a, 'k) Sexp.Of_sexp.parser -> ('a, 'k) Sexp.Of_sexp.parser
val get_exn : unit -> (t, 'k) Sexp.Of_sexp.parser

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

(* CR-soon diml: make this abstract *)
type t =
  { kind                  : Kind.t
  ; name                  : Name.t
  ; root                  : Path.t
  ; version               : string option
  ; packages              : Package.t Package.Name.Map.t
  ; mutable stanza_parser : Stanza.t list Sexp.Of_sexp.t
  ; mutable project_file  : Path.t option
  }

module Lang : sig
  type project = t

  (** One version of a language *)
  type t

  (** [make version stanzas_parser] defines one version of a
      language. Users will select this language by writing:

      {[ (lang <name> <version>) ]}

      as the first line of their [dune-project] file. [stanza_parsers]
      defines what stanzas the user can write in [dune] files. *)
  val make
    :  Syntax.Version.t
    -> (project -> Stanza.Parser.t list)
    -> t

  val version : t -> Syntax.Version.t

  (** Register all the supported versions of a language *)
  val register : string -> t list -> unit

  (** Latest version of the following language *)
  val latest : string -> t
end with type project := t

module Extension : sig
  type project = t

  (** One version of an extension *)
  type t

  (** [make version args_spec f] defines one version of an
      extension. Users will enable this extension by writing:

      {[ (using <name> <version> <args>) ]}

      in their [dune-project] file. [args_spec] is used to describe
      what [<args>] might be.
  *)
  val make
    :  Syntax.Version.t
    -> ('a, Stanza.Parser.t list) Sexp.Of_sexp.Constructor_args_spec.t
    -> (project -> 'a)
    -> t

  (** Register all the supported versions of an extension *)
  val register : string -> t list -> unit
end with type project := t

(** Load a project description from the following directory. [files]
    is the set of files in this directory. *)
val load : dir:Path.t -> files:String.Set.t -> t option

(** "dune-project" *)
val filename : string

(** Represent the scope at the root of the workspace when the root of
    the workspace contains no [dune-project] or [<package>.opam] files. *)
val anonymous : t Lazy.t

(** Check that the dune-project file exists and create it otherwise. *)
val ensure_project_file_exists : t -> unit

(** Append the following text to the project file *)
val append_to_project_file : t -> string -> unit

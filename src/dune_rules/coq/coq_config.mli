open Import

(** Data of a Coq configuration. *)
type t

(** [version ~coqc] runs coqc --print-version and returns the version of Coq as a string
    without having to run coqc --config. Exceptionally, one of the following will happen:

    - Return [Error message] if coqc --print-version exits with a non-zero code.
    - Throw a user error if coqc --print-version is not parsable.
    - Throw an [Action.Prog.Not_found] exception if the coqc binary is not found. *)
val version : coqc:Action.Prog.t -> (string, User_message.Style.t Pp.t) result Memo.t

(** [make ~coqc] runs coqc --config and returns the configuration data. Exceptionally, one
    of the following will happen:

    - Return [Error message] if coqc --config exits with a non-zero code.
    - Throw a user error if coqc --config is not parsable.
    - Throw an [Action.Prog.Not_found] exception if the coqc binary is not found. *)
val make : coqc:Action.Prog.t -> (t, User_message.Style.t Pp.t) result Memo.t

module Value : sig
  type t = private
    | Int of int
    | Path of Path.t
    | String of string

  val int : int -> t
  val string : string -> t
  val path : Path.t -> t
  val to_dyn : t -> Dyn.t
end

(** [by_name t name] returns the value of the option [name] in the Coq
    configuration [t]. If the value is not available then [None] is returned.
    Throws a code error if an invalid [name] is requested.

    Currently supported names are:

    - version.major
    - version.minor
    - version.revision
    - version.suffix
    - version
    - ocaml-version
    - coqlib
    - coqcorelib
    - coq_native_compiler_default *)
val by_name : t -> string -> Value.t Option.t

val expand
  :  Dune_lang.Template.Pform.t
  -> Pform.Macro_invocation.t
  -> Artifacts.t
  -> Dune_lang.Value.t list Memo.t

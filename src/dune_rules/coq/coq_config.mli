open Import

type t

val version :
  coqc:Action.Prog.t -> (string, User_message.Style.t Pp.t) Result.t Memo.t

val make : coqc:Action.Prog.t -> t Memo.t

val make_opt : coqc:Action.Prog.t -> t Option.t Memo.t

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

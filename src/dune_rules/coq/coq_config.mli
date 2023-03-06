open Import

type t

val version :
  coqc:Action.Prog.t -> (string, User_message.Style.t Pp.t) Result.t Memo.t

val make : coqc:Action.Prog.t -> t Memo.t

val make_opt : coqc:Action.Prog.t -> t Option.t Memo.t

module Value : sig
  type t =
    | Int of int
    | Path of Path.t
    | String of string

  val to_dyn : t -> Dyn.t
end

(** [by_name t name] returns the value of the option [name] in the Coq
    configuration [t]. Currently supported names are:

    - version.major
    - version.minor
    - version.revision
    - version.suffix
    - version
    - ocaml-version
    - coqlib
    - coq_native_compiler_default *)
val by_name : t -> string -> Value.t Option.t

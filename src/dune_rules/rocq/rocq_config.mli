(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

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

module Version : sig
  (** Data of a Rocq version. *)
  type t

  (** [make ~rocq] runs rocq --print-version and returns the version of Rocq
      and the version of OCaml used to build Rocq.
      Exceptionally, one of the following will happen:

      - Return [Error message] if rocq --print-version exits with a non-zero code.
      - Throw a user error if rocq --print-version is not parsable.
      - Throw an [Action.Prog.Not_found] exception if the rocq binary is not found. *)
  val make : rocq:Action.Prog.t -> (t, User_message.Style.t Pp.t) result Memo.t

  (** [by_name t name] returns the value of the field [name] in the Rocq
      version [t]. If the value is not available then [None] is returned.
      Throws a code error if an invalid [name] is requested.

      Currently supported names are:

      - version.major
      - version.minor
      - version.revision
      - version.suffix
      - version
      - ocaml-version *)
  val by_name : t -> string -> Value.t Option.t
end

(** Data of a Rocq configuration. *)
type t

(** [make ~rocq] runs rocq --config and returns the configuration data. Exceptionally, one
    of the following will happen:

    - Return [Error message] if rocq --config exits with a non-zero code.
    - Throw a user error if rocq --config is not parsable.
    - Throw an [Action.Prog.Not_found] exception if the rocq binary is not found. *)
val make : rocq:Action.Prog.t -> (t, User_message.Style.t Pp.t) result Memo.t

(** [by_name t name] returns the value of the option [name] in the Rocq
    configuration [t]. If the value is not available then [None] is returned.
    Throws a code error if an invalid [name] is requested.

    Currently supported names are:

    - rocqlib
    - rocq_native_compiler_default *)
val by_name : t -> string -> Value.t Option.t

val expand
  :  Dune_lang.Template.Pform.t
  -> Pform.Macro_invocation.t
  -> Artifacts.t
  -> Dune_lang.Value.t list Memo.t

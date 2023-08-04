(** Are we running inside an emacs shell? *)

val inside_emacs : bool

(** Are we running inside Dune? *)
val inside_dune : bool

(** Are we running in CI?. This checks the CI environment variable which is
    supported by travis, gitlab.*)
val inside_ci : bool

module Inside_dune : sig
  type t =
    | Yes
    | In_context of Stdune.Path.Build.t

  val var : Stdune.Env.Var.t
  val value : t -> string
end

(** An automation harness can be used to control dune polling mode by an
    automated system. The main use case is tests, but production uses are also
    possible. For example, a CI service may want to use this mechanism.

    In particular, an automation harness can:

    - learn when a dune build is finished
    - control when a dune starts a new build
    - tell dune to exit instead of starting a new build *)
type t

module Response : sig
  (** Response from automation harness *)
  type t =
    | Exit
    | Files_changed
end

val create : command:string -> t

(** By calling [build_finished], dune tells the automation harness that a build
    is finished. The function call is expected to block until the automation
    harness decides what dune should do next. *)
val build_finished : t -> Response.t

(** Controls when the errors are reported.

    - [Early] - report errors as soon as they are discovered. This means the
      ordering of errors can depend on build scheduling.
    - [Deterministic] - report errors at the end of the build in a deterministic
      order.
    - [Twice] - report each error twice: once as soon as the error is discovered
      and then again at the end of the build, in a deterministic order. *)
type t =
  | Early
  | Deterministic
  (* CR-someday aalekseyev: The deterministic mode is not entirely deterministic
     as far as error stack traces are concerned, since we're choosing an
     arbitrary stack trace to display, and the choice can be affected by
     scheduling. *)
  | Twice

let default : t =
  match Dune_util.Execution_env.inside_dune with
  | true -> Deterministic
  | false -> Early
;;

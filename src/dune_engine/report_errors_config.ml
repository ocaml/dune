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
  | Twice

let default : t = Early

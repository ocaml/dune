(** This library uses Lwt for async operations. *)

(** Returns a promise that resolves to an integer.
    Uses {!Lwt.t} type from the Lwt library. *)
val get_value : unit -> int Lwt.t

(** Runs an Lwt promise and returns the result.
    See {!Lwt.bind} for more details on promise composition. *)
val run_promise : 'a Lwt.t -> 'a

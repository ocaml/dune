(** Track pending work and let only the latest debounce timer consume it. *)

type t
type token

(** [create ()] returns a debouncer with no pending work. *)
val create : unit -> t

(** [push t] marks work as pending and returns a token for the new debounce
    generation. Any earlier token can no longer consume the pending work. *)
val push : t -> token

(** [latest t] returns the latest token if [t] has pending work. *)
val latest : t -> token option

(** [take_if_latest t token] clears pending work and returns [true] iff [token]
    is the latest generation and [t] has pending work. It returns [false]
    without changing [t] for stale tokens or when no work is pending. *)
val take_if_latest : t -> token -> bool

(** [is_pending t] is [true] iff [t] has pending work. *)
val is_pending : t -> bool

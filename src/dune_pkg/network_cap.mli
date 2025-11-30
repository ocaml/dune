(** Represents the right to access the network. Used to track which functions
    may require internet in order to make it harder to accidentally break
    offline mode. *)
type t

val create : reason_for_network_access:string -> t
val for_unit_test : t

(** Prints the reason for network access in the verbose display mode. *)
val log : t -> unit

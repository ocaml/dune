(** This is the bar library that depends on foo. *)

(** A function that uses {!Foolib.foo_type} from the foo package. *)
val use_foo : Foolib.foo_type -> int

(** Another reference to {!Foolib.foo_function}. *)
val call_foo : int -> int

(** Module Foo that references Bar. *)

(** Uses {!Bar.bar_type} from the same library. *)
val use_bar : Bar.bar_type -> int

(** Also see {!Bar.bar_function}. *)
val call_bar : int -> int

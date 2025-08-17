(** Information about groups in a match. *)

(** Result of a successful match. *)
type t

val create : string -> gcount:int -> gpos:int array -> Mark_infos.t -> Pmark.Set.t -> t

(** Raise [Not_found] if the group did not match *)
val get : t -> int -> string

(** Similar to {!get}, but returns an option instead of using an exception. *)
val get_opt : t -> int -> string option

(** Raise [Not_found] if the group did not match *)
val offset : t -> int -> int * int

val offset_opt : t -> int -> (int * int) option

(** Return the start of the match. Raise [Not_found] if the group did not match. *)
val start : t -> int -> int

val start_opt : t -> int -> int option

(** Return the end of the match. Raise [Not_found] if the group did not match. *)
val stop : t -> int -> int

val stop_opt : t -> int -> int option

(** Return the empty string for each group which did not match *)
val all : t -> string array

(** Return [(-1,-1)] for each group which did not match *)
val all_offset : t -> (int * int) array

(** Test whether a group matched *)
val test : t -> int -> bool

val pmarks : t -> Pmark.Set.t

(** Returns the total number of groups defined - matched or not.
    This function is experimental. *)
val nb_groups : t -> int

val pp : t Fmt.t

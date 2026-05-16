type t

(** Execute permissions. *)
val execute : t

(** Write permissions. *)
val write : t

(** Add permissions to a given mask for the current user. *)
val add : t -> int -> int

(** Test permissions of a given mask for the current user. *)
val test : t -> int -> bool

(** Remove permissions from a given mask for all users. *)
val remove : t -> int -> int

module Mode : sig
  type permission = t
  type t

  val of_int : int -> t
  val to_int : t -> int
  val default_file : t
end

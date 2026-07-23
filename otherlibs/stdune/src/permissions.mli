type t

(** No permissions. *)
val none : t

(** Read permissions. *)
val read : t

(** Execute permissions. *)
val execute : t

(** Write permissions. *)
val write : t

(** Combine permissions. *)
val ( + ) : t -> t -> t

module Mode : sig
  type permission = t
  type t

  (** Build a Unix permission mode by assigning permissions to each class. *)
  val create : ?user:permission -> ?group:permission -> ?other:permission -> unit -> t

  (** Wrap a mode produced by Unix APIs. Prefer [create] for literal modes. *)
  val of_int : int -> t

  val to_int : t -> int
  val default_file : t
  val executable_file : t
  val default_dir : t
  val private_file : t
end

(** Add permissions to a given mode for the current user. *)
val add : t -> Mode.t -> Mode.t

(** Test permissions of a given mode for the current user. *)
val test : t -> Mode.t -> bool

(** Test permissions of a given mode for any user. *)
val test_any : t -> Mode.t -> bool

(** Remove permissions from a given mode for all users. *)
val remove : t -> Mode.t -> Mode.t

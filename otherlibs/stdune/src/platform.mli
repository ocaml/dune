(** Platform detection. *)

module OS : sig
  (** Detect the operating system. *)

  type t =
    | Darwin
    | Linux
    | Windows
    | FreeBSD
    | NetBSD
    | OpenBSD
    | DragonFly
    | Haiku
    | Other

  (** [value] is the current os we're running on. *)
  val value : t
end

(** Number of logical processors available to this process, or [0] if it cannot be
    determined. *)
val cpu_count : unit -> int

(** [assert_os os] assert that we're running on [os]. *)
val assert_os : OS.t -> unit

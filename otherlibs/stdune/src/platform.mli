(** Platform detection. *)

module OS : sig
  (** Detect the operating system. *)

  type t =
    | Darwin
    | Linux
    | Windows
    | Other

  (** [value] is the current os we're running on. *)
  val value : t
end

(** [assert_os os] assert that we're running on [os]. *)
val assert_os : OS.t -> unit

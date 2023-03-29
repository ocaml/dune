module OS : sig
  type t =
    | Darwin
    | Linux
    | Other

  val value : t
end

val assert_os : OS.t -> unit

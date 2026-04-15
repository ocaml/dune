type t =
  | Batch
  | Watch of int

val to_int : t -> int

module State : sig
  type run_id := t

  type t =
    | Batch_not_started
    | Batch_started
    | Watch of int

  val create : watch_mode:bool -> t
  val is_watch : t -> bool
  val next_to_start : t -> run_id
  val start : t -> t * run_id
end

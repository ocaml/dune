(** Simple thread pool *)

(** A thread pool *)
type t

(** [create ~min_workers ~max_workers] requires
    [0 <= min_workers <= max_workers] and [max_workers > 0]. *)
val create
  :  min_workers:int (** minimum number of threads to keep alive *)
  -> max_workers:int (** maximum number of threads to spawn *)
  -> t

(** [task t ~f] runs [f] inside the pool [f]. [f] must not raise. *)
val task : t -> f:(unit -> unit) -> unit

(** Simple thread pool *)

(** A thread pool *)
type t

val create
  :  min_workers:int (** minimum number of threads to spawn *)
  -> max_workers:int (** maximum number of threads to spawn *)
  -> spawn_thread:((unit -> unit) -> unit)
       (** [spawn_thread f] launches [f] in a thread *)
  -> t

(** [task t ~f] runs [f] inside the pool [f]. [f] must not raise. *)
val task : t -> f:(unit -> unit) -> unit

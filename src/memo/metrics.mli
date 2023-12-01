(** Various performance metrics. *)

open! Stdune

module Counter : sig
  type t

  val read : t -> int
  val incr : t -> unit
  val add : t -> int -> unit
end

(** Counters related to Memo attempting (and possibly succeeding) to restore node values
    from cache (i.e. from Memo tables). *)
module Restore : sig
  (** How many nodes did Memo attempt to restore from cache? *)
  val nodes : Counter.t

  (** How many node dependencies were confirmed as up-to-date? *)
  val edges : Counter.t

  (** How many times was restoring a node blocked on other nodes? *)
  val blocked : Counter.t
end

(** Counters related to Memo (re)computing node values. *)
module Compute : sig
  (** How many nodes were (re)computed? *)
  val nodes : Counter.t

  (** How many dependencies did (re)computed nodes have? *)
  val edges : Counter.t

  (** How many times was computing a node blocked on other nodes? *)
  val blocked : Counter.t
end

(** Counters related to Memo checking for dependency cycles between nodes. *)
module Cycle_detection : sig
  (** Number of nodes added to the cycle detection DAG.

      This number can't exceed [Restore.nodes + Compute.nodes]. *)
  val nodes : Counter.t

  (** Number of edges added to the cycle detection DAG.

      This number can't exceed [Restore.edges + Compute.edges]. *)
  val edges : Counter.t
end

(** Reset all counters to zero. *)
val reset : unit -> unit

(** A concise summary of performance counters. Resets all counters to zero after reporting
    if [reset_after_reporting=true]. *)
val report : reset_after_reporting:bool -> string

(** Raise if any internal invariants are violated. *)
val assert_invariants : unit -> unit

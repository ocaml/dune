open Import

(** Jane Street implementation of jobs display, modified to take in a generic rendering
    call. *)

module Job_map : sig
  type key = Dune_rpc.Job.Id.t
  type 'a t

  val cardinal : 'a t -> int
  val filteri : 'a t -> f:(key -> 'a -> bool) -> 'a t
  val to_list_map : 'a t -> f:(key -> 'a -> 'b) -> 'b list
end

module State : sig
  module Displayed_job : sig
    type t = private
      { job : Dune_rpc.Job.t
      ; row : int
      ; time_first_displayed : float
      ; mutable status : [ `Running | `Finished_at of float ]
      }
  end

  type t = private
    { mutable running_jobs_by_id : Dune_rpc.Job.t Job_map.t
    ; mutable displayed_jobs_by_id : Displayed_job.t Job_map.t
    ; mutable keep_refreshing : bool
    ; mutable free_rows : Int.Set.t
    }
end

module Display (_ : Jobs_display_intf.S with type state = State.t) : sig
  val display_jobs
    :  Dune_rpc_client.Client.t
    -> max_rows:int
    -> min_duration_sec:float
    -> min_display_duration_sec:float
    -> unit Fiber.t
end

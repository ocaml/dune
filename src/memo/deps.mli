(** Dependencies of a Memo node. *)

type 'node t

val empty : 'node t

(* CR-soon amokhov: Push accumulation of dependencies inside this module to avoid dealing
   with reversed lists in [memo.ml]. *)
val create : deps_rev:'node list -> 'node t
val length : 'node t -> int
val to_list : 'node t -> 'node list

(** Checking dependencies of a node can lead to one of these outcomes:

    - [Unchanged]: All dependencies of the node are up to date. We can therefore skip
      recomputing the node and can reuse the value computed in the previous run.

    - [Changed]: One of the dependencies has changed since the previous run and the node
      should therefore be recomputed.

    - [Cancelled _]: One of the dependencies leads to a dependency cycle. In this case,
      there is no point in recomputing the current node: it's impossible to bring its
      dependencies up to date! *)
module Changed_or_not : sig
  type 'cycle t =
    | Unchanged
    | Changed
    | Cancelled of { dependency_cycle : 'cycle }
end

val changed_or_not
  :  'node t
  -> f:('node -> 'cycle Changed_or_not.t Fiber.t)
  -> 'cycle Changed_or_not.t Fiber.t

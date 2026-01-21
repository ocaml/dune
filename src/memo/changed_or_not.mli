open! Import

(** Checking dependencies of a Memo node can lead to one of these outcomes:

    - [Unchanged]: All dependencies of the node are up to date. We can therefore skip
      recomputing the node and can reuse the value computed in the previous run.

    - [Changed]: One of the dependencies has changed since the previous run and the node
      should therefore be recomputed.

    - [Cancelled _]: One of the dependencies leads to a dependency cycle. In this case,
      there is no point in recomputing the current node: it's impossible to bring its
      dependencies up to date! *)
type 'cycle t =
  | Unchanged
  | Changed
  | Cancelled of { dependency_cycle : 'cycle }

(** Prefers left dependency cycle errors in [Cancelled] but is otherwise commutative. *)
val combine : 'cycle t -> 'cycle t -> 'cycle t

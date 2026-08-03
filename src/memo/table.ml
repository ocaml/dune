open! Import

(** A table memoizing the results of a function: the function's [spec] plus a
    store mapping inputs to their dependency nodes. *)
type ('input, 'output) t =
  { spec : ('input, 'output) Spec.t
  ; cache : ('input, ('input, 'output) Node.Dep_node.t) Store.t
  }

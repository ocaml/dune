(** Dependencies of a Memo node. *)

type 'node t

val empty : 'node t

(* CR-soon amokhov: Push accumulation of dependencies inside this module to avoid dealing
   with reversed lists in [memo.ml]. *)
val create : deps_rev:'node list -> 'node t
val length : 'node t -> int
val to_list : 'node t -> 'node list

val changed_or_not
  :  'node t
  -> f:('node -> 'cycle Changed_or_not.t Fiber.t)
  -> 'cycle Changed_or_not.t Fiber.t

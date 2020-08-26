open Import

module type Key = sig
  type t

  val compare : t -> t -> Ordering.t

  module Map : Map.S with type key = t
end

module type Unordered_eval = sig
  (** Evaluate an ordered set. [standard] is the interpretation of [:standard]
      inside the DSL. *)
  type t

  module Key : Key

  (** Same as [eval] but the result is unordered *)
  val eval :
       t
    -> parse:(loc:Loc.t -> string -> 'a)
    -> key:('a -> Key.t)
    -> standard:'a Key.Map.t
    -> 'a Key.Map.t

  (** Same as [eval] but the result is unordered *)
  val eval_loc :
       t
    -> parse:(loc:Loc.t -> string -> 'a)
    -> key:('a -> Key.t)
    -> standard:(Loc.t * 'a) Key.Map.t
    -> (Loc.t * 'a) Key.Map.t
end

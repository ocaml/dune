(** State monad transformer. *)

module Make
    (S : sig
       (* The state isn't a type variable as is done traditionally, because we want
          to reuse our existing monad machinery. All that machinery requires the
          monad to have only one type variable *)
       type t
     end)
    (M : Monad.S) : sig
  include Monad.S

  (** [run t state] runs computation [t] with [state] as the initial state. The
      final state and the computed result are returned *)
  val run : 'a t -> S.t -> (S.t * 'a) M.t

  (** [get] returns the current state *)
  val get : S.t t

  (** [set s] sets the current state to [s] *)
  val set : S.t -> unit t

  (** [lift m] lifts [m] into the transformer *)
  val lift : 'a M.t -> 'a t

  (** [modify f] lifts [f] into the monad. [f] is executed with the current
      state to produce a new state. *)
  val modify : (S.t -> S.t) -> unit t
end

(** Errors found when building targets. *)

open Import

module Id : sig
  type t

  module Map : Map.S with type key = t

  val compare : t -> t -> Ordering.t
  val to_int : t -> int
  val to_dyn : t -> Dyn.t
end

type t

(** Construct a list of errors from an exception. *)
val of_exn : Exn_with_backtrace.t -> t list

val id : t -> Id.t

(** the directory where the rule the error is originating from *)
val dir : t -> Path.t option

(** The description of the error. Errors from build rules contain useful
    metadata that are extracted into [`Diagnostic] *)
val description
  :  t
  -> [ `Exn of Exn_with_backtrace.t | `Diagnostic of Compound_user_error.t ]

val promotion : t -> Diff_promotion.Annot.t option

module Event : sig
  type nonrec t =
    | Add of t
    | Remove of t
end

module Set : sig
  type error := t
  type t

  val add : t -> error -> t

  (** [one_event_diff ~prev ~next] returns the event that constructs [next]
      from [prev] if [next] is in the successive "generation" of [prev] *)
  val one_event_diff : prev:t -> next:t -> Event.t option

  val equal : t -> t -> bool
  val current : t -> error Id.Map.t
  val empty : t
end

module For_tests : sig
  (** Internal helpers for testing purposes. Do not use. *)

  (** Construct an [Error.t] *)
  val make
    :  description:[ `Exn of Exn_with_backtrace.t | `Diagnostic of Compound_user_error.t ]
    -> dir:Path.t option
    -> promotion:Diff_promotion.Annot.t option
    -> unit
    -> t
end

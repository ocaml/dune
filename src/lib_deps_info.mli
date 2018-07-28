(** Tracking of library dependencies *)

(** This module implements tracking of external library dependencies,
    for [dune external-lib-deps] *)

open Import

module Kind : sig
  type t =
    | Optional
    | Required

  val merge : t -> t -> t
end

type t = Kind.t String.Map.t

val merge : t -> t -> t

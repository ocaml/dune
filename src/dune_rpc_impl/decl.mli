open Import
open Dune_rpc

(** Internal RPC requests *)

module Build_outcome_with_diagnostics : sig
  type t =
    | Success
    | Failure of Dune_engine.Compound_user_error.t list

  val sexp : (t, Conv.values) Conv.t
end

module Status : sig
  module Menu : sig
    type t =
      | Uninitialized
      | Menu of (string * int) list

    val sexp : (t, Conv.values) Conv.t
  end

  type t = { clients : (Id.t * Menu.t) list }

  val sexp : (t, Conv.values) Conv.t
end

val build : (string list, Build_outcome_with_diagnostics.t) Decl.Request.t
val status : (unit, Status.t) Decl.Request.t

val promote
  : ( Promote.Diff_promotion.files_to_promote
      , Build_outcome_with_diagnostics.t )
      Decl.Request.t

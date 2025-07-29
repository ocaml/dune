open Import
open Dune_rpc

(** Internal RPC requests *)

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
(*
   val format
  : (Dune_engine.Clflags.Promote.t, Build_outcome_with_diagnostics.t) Decl.Request.t *)

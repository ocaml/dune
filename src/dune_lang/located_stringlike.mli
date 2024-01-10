open! Stdune

(** Implement [Stringlike_intf.S] with a value accompanied by a [Loc.t] for
    string that carry around their own source location for easier error
    reporting. *)
module Make (S : Dune_util.Stringlike_base) :
  Dune_util.Stringlike with type t = Loc.t * S.t

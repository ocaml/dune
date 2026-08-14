(** Shared s-expressions and RPC declarations for rule plugins. *)

module Sexp = Stdune.Dune_sexp

(** Locations are omitted on the wire and decoded as [Stdune.Loc.none]. *)
val sexp : Sexp.t Stdune.Conv.value

val instance_id_env_var : string
val load_rules : (Sexp.t list, Sexp.t list) Types.Decl.Request.t

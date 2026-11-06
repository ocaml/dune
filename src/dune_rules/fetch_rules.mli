open Import

(** Fetch remote sources from the revision store or http.
    Note: this module does not handle source on local disk *)

val context : Build_context.t

module Kind : sig
  type t =
    | File
    | Dir
end

val fetch
  :  target:Path.Build.t
  -> Kind.t
  -> Dune_pkg.Source.t
  -> Action.Full.t With_targets.t

val gen_rules : string list -> Build_config.Gen_rules.t Memo.t

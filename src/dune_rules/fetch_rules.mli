open Import

(** Fetch or extract sources that need a built backing. Local directories
    are read directly by package source rules. *)

val context : Build_context.t

module Kind : sig
  type t =
    | File
    | Dir
end

(** Where a source is fetched to. *)
val make_target : kind:Kind.t -> Dune_pkg.Source.t -> Path.Build.t

val fetch
  :  target:Path.Build.t
  -> Kind.t
  -> Dune_pkg.Source.t
  -> Action.Full.t With_targets.t

val gen_rules : string list -> Build_config.Gen_rules.t Memo.t

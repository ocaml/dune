open Import

(** Fetch remote sources from the revision store or http.
    Note: this module does not handle source on local disk *)

val context : Build_context.t

val fetch
  :  target:Path.Build.t
  -> [ `File | `Directory ]
  -> Loc.t * OpamUrl.t
  -> (Loc.t * Dune_pkg.Checksum.t) option
  -> Action.Full.t With_targets.t

val gen_rules
  :  dir:Path.Build.t
  -> components:Filename.t list
  -> Build_config.Gen_rules.t Memo.t

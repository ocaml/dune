open! Import

(** Encode the targets as [Dune_lang.t], and then as strings suitable to
    be sent via RPC. *)
val prepare_targets : Dune_lang.Dep_conf.t list -> string list

(** dune rpc build command *)
val cmd : unit Cmdliner.Cmd.t

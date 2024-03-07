open Import

(** Tie the knot between [Dune_engine] and [Dune_rules]. *)
val init
  :  ?action_runner:(Dune_engine.Action_exec.input -> Dune_engine.Action_runner.t option)
  -> ?action_runners:(unit -> Dune_engine.Action_runner.t list)
  -> stats:Dune_stats.t option
  -> sandboxing_preference:Sandbox_mode.t list
  -> cache_config:Dune_cache.Config.t
  -> cache_debug_flags:Dune_engine.Cache_debug_flags.t
  -> unit
  -> unit

type build_system =
  { contexts : Context.t list
  ; scontexts : Super_context.t Context_name.Map.t
  }

val get : unit -> build_system Memo.t
val find_context_exn : build_system -> name:Context_name.t -> Context.t
val find_scontext_exn : build_system -> name:Context_name.t -> Super_context.t

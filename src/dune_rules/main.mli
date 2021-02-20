open! Dune_engine
open! Stdune
open! Import

(** Tie the knot between [Dune_engine] and [Dune_rules]. *)
val init :
     stats:Stats.t option
  -> sandboxing_preference:Sandbox_mode.t list
  -> cache_config:Dune_cache.Config.t
  -> handler:Build_system.Handler.t option
  -> unit

type build_system =
  { conf : Dune_load.conf
  ; contexts : Context.t list
  ; scontexts : Super_context.t Context_name.Map.t
  }

val get : unit -> build_system Memo.Build.t

val find_context_exn : build_system -> name:Context_name.t -> Context.t

val find_scontext_exn : build_system -> name:Context_name.t -> Super_context.t

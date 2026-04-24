open Import

(** Tie the knot between [Dune_engine] and [Dune_rules]. *)
val init
  :  ?action_runner:(Dune_engine.Action_exec.input -> Dune_engine.Action_runner.t option)
  -> ?action_runners:(unit -> Dune_engine.Action_runner.t list)
  -> ?sandbox_actions:bool
  -> sandboxing_preference:Sandbox_mode.t list
  -> unit
  -> unit

type build_system =
  { contexts : Context.t list
  ; scontexts : Super_context.t Context_name.Map.t
  }

val get : unit -> build_system Memo.t
val find_context_exn : build_system -> name:Context_name.t -> Context.t
val find_scontext_exn : build_system -> name:Context_name.t -> Super_context.t

(* The working context/environment for executing actions. Name suggestions
   welcome.

   For now, this entire module is exposed to the frontend and (aside from in
   [Action_exec]) should be used primarily for defining custom actions. In the
   future, it might be nice to expose a more restricted or cleaner interface
   specifically for that purpose. *)

open! Stdune

type context =
  { targets : Path.Build.Set.t
  ; context : Build_context.t option
  ; purpose : Process.purpose
  ; rule_loc : Loc.t
  }

type env =
  { working_dir : Path.t
  ; env : Env.t
  ; stdout_to : Process.Io.output Process.Io.t
  ; stderr_to : Process.Io.output Process.Io.t
  ; stdin_from : Process.Io.input Process.Io.t
  ; exit_codes : int Predicate_lang.t
  }

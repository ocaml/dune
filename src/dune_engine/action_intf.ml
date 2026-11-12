open Import

module Exec = struct
  type context =
    { root : Path.t
    ; metadata : Process_metadata.t
    ; sandbox : Process.Sandbox.t option
    ; rule_loc : Loc.t
    ; build_deps : Dep.Set.t -> unit Fiber.t
    }

  type env =
    { working_dir : Path.t
    ; env : Env.t
    ; stdout_to : Process.Io.output Process.Io.t
    ; stderr_to : Process.Io.output Process.Io.t
    ; stdin_from : Process.Io.input Process.Io.t
    ; exit_codes : int Predicate.t
    }
end

module Ext = struct
  module Exec = Exec

  module type Spec = sig
    type ('path, 'target) t

    val name : string
    val version : int
    val is_useful_to_memoize : bool
    val is_dynamic : bool
    val runs_process : bool
    val can_run_in_action_runner : bool
    val encode : ('p, 't) t -> ('p -> Sexp.t) -> ('t -> Sexp.t) -> Sexp.t
    val bimap : ('a, 'b) t -> ('a -> 'x) -> ('b -> 'y) -> ('x, 'y) t

    val action
      :  (Path.t, Path.Build.t) t
      -> ectx:Exec.context
      -> eenv:Exec.env
      -> unit Fiber.t
  end

  module type Instance = sig
    type target
    type path

    module Spec : Spec

    val v : (path, target) Spec.t
  end
end

open Import
open Dune_util.Action

module Simplified = struct
  type destination =
    | Dev_null
    | File of string

  type source = string

  type t =
    | Run of string * string list
    | Chdir of string
    | Setenv of string * string
    | Redirect_out of t list * Outputs.t * destination
    | Redirect_in of t list * Inputs.t * source
    | Pipe of t list list * Outputs.t
    | Sh of string
end

module type Ast = sig
  type program
  type path
  type target
  type string
  type ext

  type t =
    | Run of program * string Array.Immutable.t
    | With_accepted_exit_codes of int Predicate_lang.t * t
    | Dynamic_run of program * string list
    | Chdir of path * t
    | Setenv of string * string * t
    (* It's not possible to use a build path here since jbuild supports
       redirecting to /dev/null. In [dune] files this is replaced with
       %{null} *)
    | Redirect_out of Outputs.t * target * File_perm.t * t
    | Redirect_in of Inputs.t * path * t
    | Ignore of Outputs.t * t
    | Progn of t list
    | Concurrent of t list
    | Echo of string list
    | Cat of path list
    | Copy of path * target
    | Symlink of path * target
    | Hardlink of path * target
    | Bash of string
    | Write_file of target * File_perm.t * string
    | Rename of target * target
    | Remove_tree of target
    | Mkdir of target
    | Pipe of Outputs.t * t list
    | Extension of ext
    | Needed_deps of path list
end

module type Helpers = sig
  type program
  type path
  type target
  type string
  type t

  (* TODO consider changing this to a [string array] to save some conversion *)
  val run : program -> string list -> t
  val chdir : path -> t -> t
  val setenv : string -> string -> t -> t
  val with_stdout_to : ?perm:File_perm.t -> target -> t -> t
  val with_stderr_to : ?perm:File_perm.t -> target -> t -> t
  val with_outputs_to : ?perm:File_perm.t -> target -> t -> t
  val with_stdin_from : path -> t -> t
  val ignore_stdout : t -> t
  val ignore_stderr : t -> t
  val ignore_outputs : t -> t
  val progn : t list -> t
  val concurrent : t list -> t
  val echo : string list -> t
  val cat : path list -> t
  val copy : path -> target -> t
  val symlink : path -> target -> t
  val bash : string -> t
  val write_file : ?perm:File_perm.t -> target -> string -> t
  val rename : target -> target -> t
  val remove_tree : target -> t
  val mkdir : target -> t
  val needed_deps : path list -> t
end

module Exec = struct
  type context =
    { targets : Targets.Validated.t option
    ; context : Build_context.t option
    ; metadata : Process.metadata
    ; rule_loc : Loc.t
    ; build_deps : Dep.Set.t -> Dep.Facts.t Fiber.t
    }

  type env =
    { working_dir : Path.t
    ; env : Env.t
    ; stdout_to : Process.Io.output Process.Io.t
    ; stderr_to : Process.Io.output Process.Io.t
    ; stdin_from : Process.Io.input Process.Io.t
    ; prepared_dependencies : Dune_action_plugin.Private.Protocol.Dependency.Set.t
    ; exit_codes : int Predicate.t
    }
end

module Ext = struct
  module type Spec = sig
    type ('path, 'target) t

    val name : string
    val version : int
    val is_useful_to : memoize:bool -> bool
    val encode : ('p, 't) t -> ('p -> Sexp.t) -> ('t -> Sexp.t) -> Sexp.t
    val bimap : ('a, 'b) t -> ('a -> 'x) -> ('b -> 'y) -> ('x, 'y) t

    val action
      :  (Path.t, Path.Build.t) t
      -> ectx:Exec.context
      -> eenv:Exec.env
      -> (* cwong: For now, I think we should only worry about extensions with
            known dependencies. In the future, we may generalize this to return
            an [Action_exec.done_or_more_deps], but that may be trickier to get
            right, and is a bridge we can cross when we get there. *)
      unit Fiber.t
  end

  module type Instance = sig
    type target
    type path

    module Spec : Spec

    val v : (path, target) Spec.t
  end
end

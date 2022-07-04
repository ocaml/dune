open Import

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
    | Run of program * string list
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
    | Echo of string list
    | Cat of path list
    | Copy of path * target
    | Symlink of path * target
    | Hardlink of path * target
    | System of string
    | Bash of string
    | Write_file of target * File_perm.t * string
    | Rename of target * target
    | Remove_tree of target
    | Mkdir of path
    | Diff of (path, target) Diff.t
    | Merge_files_into of path list * string list * target
    | No_infer of t
    | Pipe of Outputs.t * t list
    | Extension of ext
end

module type Helpers = sig
  type program

  type path

  type target

  type string

  type t

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

  val echo : string list -> t

  val cat : path list -> t

  val copy : path -> target -> t

  val symlink : path -> target -> t

  val system : string -> t

  val bash : string -> t

  val write_file : ?perm:File_perm.t -> target -> string -> t

  val rename : target -> target -> t

  val remove_tree : target -> t

  val mkdir : path -> t

  val diff : ?optional:bool -> ?mode:Diff.Mode.t -> path -> target -> t
end

module Ext = struct
  type context =
    { targets : Targets.Validated.t option
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
    ; exit_codes : int Predicate.t
    }

  module type Spec = sig
    type ('path, 'target) t

    val name : string

    val version : int

    val is_useful_to : distribute:bool -> memoize:bool -> bool

    val encode :
      ('p, 't) t -> ('p -> Dune_lang.t) -> ('t -> Dune_lang.t) -> Dune_lang.t

    val bimap : ('a, 'b) t -> ('a -> 'x) -> ('b -> 'y) -> ('x, 'y) t

    val action :
         (Path.t, Path.Build.t) t
      -> ectx:context
      -> eenv:env
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

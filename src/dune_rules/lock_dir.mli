open Import
module Pkg = Dune_pkg.Lock_dir.Pkg

type t := Dune_pkg.Lock_dir.t

val get_with_path : Context_name.t -> (Path.t * t, User_message.t) result Memo.t
val get : Context_name.t -> (t, User_message.t) result Memo.t
val get_exn : Context_name.t -> t Memo.t
val of_dev_tool : Dune_pkg.Dev_tool.t -> t Memo.t

(** Returns [None] if the lock_dir for the specified dev tool does not exist. *)
val of_dev_tool_if_lock_dir_exists : Dune_pkg.Dev_tool.t -> t option Memo.t

val lock_dir_active : Context_name.t -> bool Memo.t
val get_path : Context_name.t -> Path.t option Memo.t

(** The default filesystem location where the lock dir is going to get created *)
val default_path : Path.t

(** The default path where the lock dir will be written to manually *)
val default_source_path : Path.Source.t

(** The location in the source tree where a dev tool lock dir is expected *)
val dev_tool_source_lock_dir : Dune_pkg.Dev_tool.t -> Path.Source.t

(** Returns the path to the lock_dir that will be used to lock the
    given dev tool *)
val dev_tool_lock_dir : Dune_pkg.Dev_tool.t -> Path.t

val select_lock_dir : Workspace.Lock_dir_selection.t -> Path.Source.t Memo.t

module Sys_vars : sig
  type t =
    { os : string option Memo.Lazy.t
    ; os_version : string option Memo.Lazy.t
    ; os_distribution : string option Memo.Lazy.t
    ; os_family : string option Memo.Lazy.t
    ; arch : string option Memo.Lazy.t
    ; sys_ocaml_version : string option Memo.Lazy.t
    }

  val os : t -> Dune_lang.Pform.Var.Os.t -> string option Memo.t
  val poll : t
  val solver_env : unit -> Dune_pkg.Solver_env.t Memo.t
end

val source_kind
  :  Dune_pkg.Source.t
  -> [ `Local of [ `Directory | `File ] * Path.External.t | `Fetch ] Memo.t

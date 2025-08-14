open Import
module Pkg = Dune_pkg.Lock_dir.Pkg

type t := Dune_pkg.Lock_dir.t

val get_with_path : Context_name.t -> (Path.t * t, User_message.t) result Memo.t
val get : Context_name.t -> (t, User_message.t) result Memo.t
val get_exn : Context_name.t -> t Memo.t
val of_dev_tool : Dune_pkg.Dev_tool.t -> t Memo.t
val lock_dir_active : Context_name.t -> bool Memo.t
val get_path : Context_name.t -> Path.t option Memo.t

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

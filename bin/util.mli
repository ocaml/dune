open Import

type checked =
  | In_build_dir of (Context.t * Path.Source.t)
  | In_private_context of Path.Build.t
  | In_install_dir of (Context.t * Path.Source.t)
  | In_source_dir of Path.Source.t
  | External of Path.External.t

val check_path : Context.t list -> Path.t -> checked
val restore_cwd_and_execve : Workspace_root.t -> string -> string list -> Env.t -> 'a

(** Resolve the Dune executable running this process, including when [argv[0]] was found
    via [PATH]. *)
val dune_executable : unit -> Path.t

val setup : unit -> Dune_rules.Main.build_system Memo.t

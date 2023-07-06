open Import

module Shell_spec : sig
  type 'path t =
    | System_shell
    | Bash_shell
    | Exec_file_shell of 'path

  val conv : ('p1 -> 'p2) -> 'p1 t -> 'p2 t

  val default : _ t
end

type ('path, _) spec =
  { script : 'path
  ; shell_spec : 'path Shell_spec.t
  }

val action : (Path.t, Import.Path.Build.t) spec -> Action.t

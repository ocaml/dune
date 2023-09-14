open Import

type checked =
  | In_build_dir of (Context.t * Path.Source.t)
  | In_private_context of Path.Build.t
  | In_install_dir of (Context.t * Path.Source.t)
  | In_source_dir of Path.Source.t
  | External of Path.External.t

val check_path : Context.t list -> Path.t -> checked

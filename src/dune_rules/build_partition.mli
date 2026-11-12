open Import

type purpose =
  | Workspace
  | Mounted

type t

val workspace : Context.t -> t
val mounted : resolver:Context.t -> output_root:Path.Build.t -> t
val build_context : t -> Build_context.t
val output_root : t -> Path.Build.t
val purpose : t -> purpose
val to_dyn : t -> Dyn.t

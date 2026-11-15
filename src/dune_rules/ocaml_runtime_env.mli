(** Default GC policy for compiler and dependency-analysis commands. *)

open Import

(** Return only action-specific overrides for compilers and dependency analysis,
    inspecting inherited settings without copying them. Existing overrides, such
    as linker settings, are preserved. *)
val toolchain : Super_context.t -> dir:Path.Build.t -> Env.t -> Env.t Action_builder.t

(** Use the same policy for Melange source compilation, probing its linked runtime
    for support and preserving either runtime-parameter environment variable. *)
val melange
  :  Super_context.t
  -> dir:Path.Build.t
  -> Action.Prog.t
  -> Env.t Action_builder.t

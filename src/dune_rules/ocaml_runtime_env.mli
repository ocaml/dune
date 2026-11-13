(** Default GC policy for compiler and dependency-analysis commands. *)

open Import

(** Return only action-specific overrides for compilers and dependency analysis,
    inspecting inherited settings without copying them. Existing overrides, such
    as linker settings, are preserved. *)
val toolchain : Super_context.t -> dir:Path.Build.t -> Env.t -> Env.t Action_builder.t

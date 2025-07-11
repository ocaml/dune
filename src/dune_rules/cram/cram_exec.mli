open Import

(** Produces the script containing only the commands to run *)
val make_script : src:Path.t -> script:Path.Build.t -> Action.t

(** Runs the script created in [make_script] *)
val run : dir:Path.t -> script:Path.t -> output:Path.Build.t -> Action.t

(** Produces a diff if [src] needs to be updated *)
val diff : src:Path.t -> output:Path.t -> Action.t

(** Corresponds the user written cram action *)
val action : Path.t -> Action.t

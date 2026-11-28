open Import

(** The default environment in which to run commands. The environment of
    individual build contexts augment this environment. *)
val env : unit -> Env.t

(** [remove_build_dir_from_path path] is [path], interpreted as the PATH
    environment variable, with the entries pointing inside the build directory
    removed. *)
val remove_build_dir_from_path : string -> string

(** [remove_build_dir_from_env env] is [env] with PATH run through
    {!remove_build_dir_from_path}.  *)
val remove_build_dir_from_env : Env.t -> Env.t

(** [rule_env ()] is [remove_build_dir_from_env (env ())]. *)
val rule_env : unit -> Env.t

(** Initialises this module. *)
val init : capture_outputs:bool -> unit

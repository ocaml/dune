open Stdune
module Process = Dune_engine.Process

(** Run a fiber thunk using the scheduler. *)
val run : (unit -> 'a Fiber.t) -> 'a

val display : Dune_engine.Display.t
val output_limit : int
val make_stdout : unit -> Process.Io.output Process.Io.t
val make_stderr : unit -> Process.Io.output Process.Io.t

(** Run a git command in the given directory. *)
val git : dir:Path.t -> string list -> unit Fiber.t

(** Run a git command and capture a single line of output. *)
val git_out : dir:Path.t -> string list -> string Fiber.t

(** Initialize a git repository with user name and email configured. *)
val git_init_and_config_user : Path.t -> unit Fiber.t

(** Create a simple git repository with one commit. Returns the HEAD commit SHA. *)
val create_repo_at : Path.t -> string Fiber.t

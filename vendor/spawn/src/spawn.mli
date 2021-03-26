(** Mini spawn library *)

(** Note: on Unix, spawn uses vfork by default. It has been tested, but if you
    believe this is causing a problem in your application, you can change this
    default at runtime by setting the environment variable [SPAWN_USE_FORK]. *)

module Working_dir : sig
  type t =
    | Path of string  (** Path in the filesystem *)
    | Fd of Unix.file_descr
        (** File descriptor pointing to a directory. Not supported on Windows. *)
    | Inherit  (** Inherit the working directory of the current process *)
end

module Unix_backend : sig
  (** System call to use on Unix. *)
  type t =
    | Fork
    | Vfork

  (** [Fork] if the [SPAWN_USE_FORK] environment variable is set, [Vfork]
      otherwise. *)
  val default : t
end

module Env : sig
  (** Representation of an environment *)
  type t

  (** Create an environment from a list of strings of the form ["KEY=VALUE"]. *)
  val of_list : string list -> t
end

(** Spawn a sub-command and return its PID. This function is low-level and
    should be used to build higher-level APIs.

    In case of errors, it raises [Unix.Unix_error].

    {b Binary}

    [prog] is not searched in [PATH]. It is up to the caller to do the path
    resolution before calling this function. Note that there is no special
    treatment of executable text files without a proper #!. The execvp function
    from the C library calls [/bin/sh] in this case to imitate the behaviors of
    a shell but this function doesn't.

    Note that when [prog] is a relative filename, it is interpreted as a path
    relative to the working directory specified by the [cwd] argument. On
    Windows, this differs from what the underlying [CreateProcess] function
    does.

    {b Command line arguments}

    [argv] is the full command line. The first element should be the program
    name and subsequent elements the command line arguments. Note that the head
    of [argv] doesn't necessarily have to be equal to [prog]. For instance it
    might be [foo] while [prog] might be [/usr/bin/foo].

    {b Environment}

    [env] represents the environment in which the sub-process is executed. If
    not specified, the environment from the process calling this function is
    used.

    {b Working directory}

    [cwd] describes what the current working directory of the sub-process should
    be. It defaults to [Inherit]. It is an error to pass [Fd _] on Windows.

    {b Standard input/outputs}

    [stdin], [stdout] and [stderr] are the file descriptors used as standard
    input, output and error output of the sub-process. When not specified, they
    default to the ones from the calling process.

    {b Signals}

    On Unix, the sub-process will have all its signals unblocked.

    {b Implementation}

    [unix_backend] describes what backend to use on Unix. If set to [Default],
    [vfork] is used unless the environment variable [SPAWN_USE_FORK] is set. On
    Windows, [CreateProcess] is used. *)
val spawn :
     ?env:Env.t
  -> ?cwd:Working_dir.t (* default: [Inherit] *)
  -> prog:string
  -> argv:string list
  -> ?stdin:Unix.file_descr
  -> ?stdout:Unix.file_descr
  -> ?stderr:Unix.file_descr
  -> ?unix_backend:Unix_backend.t (* default: [Unix_backend.default] *)
  -> unit
  -> int

(**/**)

(* Create a pipe with [O_CLOEXEC] sets for both fds. This is the same as
   creating a pipe and setting [O_CLOEXEC] manually on both ends, with the
   difference that there is no race condition between [spawn] and [safe_pipe].
   I.e. if a thread calls [safe_pipe] and another calls [spawn], it is
   guaranteed that the sub-process doesn't have the pipe without [O_CLOEXEC] set
   on one or the two file descriptors. The latter situation is problematic as
   one often reads a pipe until it is closed, however if some random process
   keeps a handle of it because it inherited it from its parent by mistake, the
   pipe will never be closed.

   It is implemented using the [pipe2] system calls, except on OSX where [pipe2]
   is not available. On OSX, both [safe_pipe] and [spawn] lock the same mutex to
   prevent race conditions. *)
val safe_pipe : unit -> Unix.file_descr * Unix.file_descr

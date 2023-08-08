(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2018 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Process and job handling, with logs, termination status, etc. *)

(** The type of shell commands *)
type command = private {
  cmd: string;
  args: string list;
  cmd_text: string option;
  cmd_dir: string option;
  cmd_env: string array option;
  cmd_stdin: bool option;
  cmd_stdout: string option;
  cmd_verbose: bool option;
  cmd_name: string option;
  cmd_metadata: (string * string) list option;
}

(** Builds a shell command for later execution.
    @param env         environment for the command
    @param verbose     force verbosity
    @param name        title, used to name log files, etc.
    @param metadata    additional info to log
    @param dir         CWD for the command
    @param allow_stdin whether to forward stdin
    @param stdout      redirect stdout to the given file instead of the logs
    @param text        Short text that may be displayed in the status-line
    @param command      The command itself
    @param args         Command-line arguments *)
val command:
  ?env:string array ->
  ?verbose:bool ->
  ?name:string ->
  ?metadata:(string*string) list ->
  ?dir:string ->
  ?allow_stdin:bool ->
  ?stdout:string ->
  ?text:string ->
  string ->
  string list ->
  command

val string_of_command: command -> string
val text_of_command: command -> string option
val is_verbose_command: command -> bool

(** Returns a label suitable for printing the summary of running commands. First
    string is the topic (e.g. package), second the action (e.g. command name).
    Optional command arguments may be used for details (e.g. make action). *)
val make_command_text:
  ?color:OpamConsole.text_style -> string -> ?args:string list -> string ->
  string

(** The type for processes *)
type t = {
  p_name   : string;        (** Command name *)
  p_args   : string list;   (** Command args *)
  p_pid    : int;           (** Process PID *)
  p_cwd    : string;        (** Process initial working directory *)
  p_time   : float;         (** Process start time *)
  p_stdout : string option; (** stdout dump file *)
  p_stderr : string option; (** stderr dump file *)
  p_env    : string option; (** dump environment variables *)
  p_info   : string option; (** dump process info *)
  p_metadata: (string * string) list; (** Metadata associated to the process *)
  p_verbose: bool;          (** whether output of the process should be
                                displayed *)
  p_tmp_files: string list; (** temporary files that should be cleaned up upon
                                completion *)
}

(** Process results *)
type result = {
  r_code     : int;         (** Process exit code, or 256 on error *)
  r_signal   : int option;  (** Signal received if the processed was killed *)
  r_duration : float;       (** Process duration *)
  r_info     : (string * string) list; (** Process info *)
  r_stdout   : string list; (** Content of stdout dump file *)
  r_stderr   : string list; (** Content of stderr dump file *)
  r_cleanup  : string list; (** List of files to clean-up *)
}

(** [run command] synchronously call the command [command.cmd] with
    arguments [command.args]. It waits until the process is finished. The files
    [name.info], [name.env], [name.out] and [name.err], with
    [name = command.cmd_name] are
    created, and contain the process main description, the environment
    variables, the standard output and the standard error.
    Don't forget to call [cleanup result] afterwards *)
val run : command -> result

(** Same as [run], but doesn't wait. Use wait_one to wait and collect
    results;
    Don't forget to call [cleanup result] afterwards *)
val run_background: command -> t

(** Similar to [run_background], except that no process is created, and a dummy
    process (suitable for dry_wait_one) is returned. *)
val dry_run_background: command -> t

(** [wait p] waits for the processus [p] to end and returns its results. Be
    careful to handle Sys.Break *)
val wait: t -> result

(** Like [wait], but returns None immediately if the process hasn't ended *)
val dontwait: t -> result option

(** Wait for the first of the listed processes to terminate, and return its
    termination status *)
val wait_one: t list -> t * result

(** Similar to [wait_one] for simulations, to be used with
    [dry_run_background] *)
val dry_wait_one: t list -> t * result

(** Send SIGINT to a process (or SIGKILL on Windows) *)
val interrupt: t -> unit

(** Is the process result a success? *)
val is_success : result -> bool

(** Is the process result a failure? *)
val is_failure : result -> bool

(** Should be called after process termination, to cleanup temporary files.
    Leaves artefacts in case OpamGlobals.debug is on and on failure, unless
    force has been set. *)
val cleanup : ?force:bool -> result -> unit

(** Like [is_success], with an added cleanup side-effect (as [cleanup
    ~force:true]). Use this when not returning 0 is not an error case: since the
    default behaviour is to cleanup only when the command returned 0, which is
    not what is expected in that case. *)
val check_success_and_cleanup : result -> bool

(** {2 Misc} *)

(** Reads a text file and returns a list of lines *)
val read_lines: string -> string list

(** Detailed report on process outcome *)
val string_of_result: ?color:OpamConsole.text_style -> result -> string

(** Short report on process outcome *)
val result_summary: result -> string

(** Higher-level interface to allow parallelism *)
module Job: sig

  (** Open type and add combinators. Meant to be opened *)
  module Op: sig
    type 'a job =
      | Done of 'a
      | Run of command * (result -> 'a job)

    (** Stage a shell command with its continuation, eg:
        {[
          command "ls" ["-a"] @@> fun result ->
          if OpamProcess.is_success result then Done result.r_stdout
          else failwith "ls"
        ]}
    *)
    val (@@>): command -> (result -> 'a job) -> 'a job

    (** [job1 @@+ fun r -> job2] appends the computation of tasks in [job2] after
        [job1] *)
    val (@@+): 'a job -> ('a -> 'b job) -> 'b job

    (** [job @@| f] maps [f] on the results of [job].
        Equivalent to [job @@+ fun r -> Done (f r)] *)
    val (@@|): 'a job -> ('a -> 'b) -> 'b job
  end

  (** Sequential run of a job *)
  val run: 'a Op.job -> 'a

  (** Same as [run] but doesn't actually run any shell command,
      and feed a dummy result to the cont. *)
  val dry_run: 'a Op.job -> 'a

  (** Catch exceptions raised within a job *)
  val catch: (exn -> 'a Op.job) -> (unit -> 'a Op.job) -> 'a Op.job

  (** Ignore all non-fatal exceptions raised by job and return default *)
  val ignore_errors: default:'a -> ?message:string ->
    (unit -> 'a Op.job) -> 'a Op.job

  (** Register an exception-safe finaliser in a job.
      [finally job fin] is equivalent to
      [catch job (fun e -> fin (); raise e) @@+ fun r -> fin (); Done r] *)
  val finally: (unit -> unit) -> (unit -> 'a Op.job) -> 'a Op.job

  (** Converts a list of commands into a job that returns None on success, or
      the first failed command and its result.
      Unless [keep_going] is true, stops on first error. *)
  val of_list: ?keep_going:bool -> command list ->
    (command * result) option Op.job

  (** As [of_list], but takes a list of functions that return the commands. The
      functions will only be evaluated when the command needs to be run. *)
  val of_fun_list: ?keep_going:bool -> (unit -> command) list ->
    (command * result) option Op.job

  (** Returns the job made of the given homogeneous jobs run sequentially *)
  val seq: ('a -> 'a Op.job) list -> 'a -> 'a Op.job

  (** Sequentially maps jobs on a list *)
  val seq_map: ('a -> 'b Op.job) -> 'a list -> 'b list Op.job

  (** Sets and overrides text of the underlying commands *)
  val with_text: string -> 'a Op.job -> 'a Op.job
end

type 'a job = 'a Job.Op.job

(**/**)
val set_resolve_command :
  (?env:string array -> ?dir:string -> string -> string option) -> unit

(** Like Unix.create_process_env, but with correct escaping of arguments when
    invoking a cygwin executable from a native Windows executable. *)
val create_process_env :
  string -> string array -> string array ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  int

val default_env : unit -> string array

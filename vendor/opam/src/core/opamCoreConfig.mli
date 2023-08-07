(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2018 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options for the core lib (record, global reference and
    setter) *)

module E : sig
  type OpamStd.Config.E.t +=
    | COLOR of OpamStd.Config.when_ option
    | CONFIRMLEVEL of OpamStd.Config.answer option
    | DEBUG of int option
    | DEBUGSECTIONS of OpamStd.Config.sections option
    | ERRLOGLEN of int option
    | KEEPLOGS of bool option
    | LOGS of string option
    | MERGEOUT of bool option
    | NO of bool option
    | PRECISETRACKING of bool option
    | SAFE of bool option
    | STATUSLINE of OpamStd.Config.when_ option
    | UTF8 of OpamStd.Config.when_ext option
    | UTF8MSGS of bool option
    | VERBOSE of OpamStd.Config.level option
    | YES of bool option

    val confirmlevel: unit -> OpamStd.Config.answer option
    val debug: unit -> int option
    val logs: unit -> string option
    val yes: unit -> bool option
end

type t = private {
  debug_level : int;
  (** Controls debug messages, 0 to disable *)
  debug_sections : OpamStd.Config.sections;
  (** Controls which sections display debugging messages. If empty, all messages
      are displayed. *)
  verbose_level : OpamStd.Config.level;
  (** Controls printing of external commands and output, 0 to disable, more
      means print more low-level commands *)
  color : OpamStd.Config.when_;
  (** Console ANSI color control *)
  utf8 : OpamStd.Config.when_ext;
  (** Controls usage of UTF8 in OPAM-generated messages. Extended adds camel
      emojis *)
  disp_status_line: OpamStd.Config.when_;
  (** Controls on-line display of parallel commands being run, using ANSI
      escapes *)
  confirm_level : [ OpamStd.Config.answer | `undefined ];
  yes: bool option;
  (** Affects interactive questions in OpamConsole: used to compute the
      automatic ansering level *)
  safe_mode : bool;
  (** Fail on writes or delays, don't ask questions (for quick queries, e.g.
      for shell completion) *)
  log_dir : string;
  (** Where to store log and temporary files (output from commands...) *)
  keep_log_dir : bool;
  (** Whether to cleanup temporary and log files on exit *)
  errlog_length : int;
  (** The number of log lines displayed on process error. 0 for all *)
  merged_output : bool;
  (** If set, stderr of commands is merged into their stdout *)
  precise_tracking : bool;
  (** If set, will take full md5 of all files when checking diffs (to track
      installations), rather than rely on just file size and mtime *)
  cygbin: string option;
  set : bool;
  (** Options have not yet been initialised (i.e. defaults are active) *)
}

type 'a options_fun =
  ?debug_level:int ->
  ?debug_sections:OpamStd.Config.sections ->
  ?verbose_level:OpamStd.Config.level ->
  ?color:OpamStd.Config.when_ ->
  ?utf8:OpamStd.Config.when_ext ->
  ?disp_status_line:OpamStd.Config.when_ ->
  ?confirm_level:OpamStd.Config.answer ->
  ?yes:bool option ->
  ?safe_mode:bool ->
  ?log_dir:string ->
  ?keep_log_dir:bool ->
  ?errlog_length:int ->
  ?merged_output:bool ->
  ?precise_tracking:bool ->
  ?cygbin:string ->
  'a

val default : t

val set : t -> (unit -> t) options_fun

val setk : (t -> 'a) -> t -> 'a options_fun

val r : t ref

val update : ?noop:_ -> (unit -> unit) options_fun

(** Sets the OpamCoreConfig options, reading the environment to get default
    values when unspecified *)
val init: ?noop:_ -> (unit -> unit) options_fun

(** Like [init], but returns the given value. For optional argument
    stacking *)
val initk: 'a -> 'a options_fun

(** Automatic answering levels
    * [`ask]: prompt and ask user
    * [`all_no]: answer no to all opam questions
    * [`all_yes]: answer yes to all opam questions
    * [`unsafe_yes]: answer yes to all opam question and launch system package
                     command wit non interactive options
    If confirm-level is set (from cli or environment variable), its value is
    returned. Otherwise, is takes last yes/no cli flag. For environment
    variables, if [OPAMYES] is set to true, it has priority over [OPAMNO]. As
    other environment variables, cli flags content is taken if given.
    [answer_is] and [answer_is_yes] computes the answer lazily, use [answer] in
    case of config update.
*)
val answer_is: OpamStd.Config.answer -> bool
val answer_is_yes : unit -> bool
val answer: unit -> OpamStd.Config.answer

(** [true] if OPAM was compiled in developer mode *)
val developer : bool

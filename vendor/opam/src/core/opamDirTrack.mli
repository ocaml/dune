(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016-2019 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** uniquely identifies a filesystem item value *)
type digest

(** Defines a change concerning a fs item; The [digest] parameter is the new
    value of the item *)
type change =
  | Added of digest
  | Removed
  | Contents_changed of digest
  (** For links, corresponds to a change of target *)
  | Perm_changed of digest
  | Kind_changed of digest
  (** Used e.g. when a file is replaced by a directory, a link
      or a fifo *)

type t = change OpamStd.String.Map.t

(** Returns a printable, multi-line string *)
val to_string: t -> string

(** Returns a summary of the changes as a printable, single-line string *)
val to_summary_string: t -> string

val digest_of_string: string -> digest
val string_of_digest: digest -> string

(** Return the [change] action, with digest if [full] is set to true *)
val string_of_change: ?full:bool -> change -> string

(** Wraps a job to track the changes that happened under [dirname] during its
    execution (changes done by the application of the job function to [()] are
    tracked too, for consistency with jobs without commands) *)
val track:
  OpamFilename.Dir.t -> ?except:OpamFilename.Base.Set.t ->
  (unit -> 'a OpamProcess.job) -> ('a * t) OpamProcess.job

(** [track_files prefix paths ?except job] as [track] wraps a job to track
    changes for a predefined list of [paths] (files and directories).
    [paths] are relative to [prefix]. *)
val track_files:
  prefix:OpamFilename.Dir.t -> string list -> ?except:OpamFilename.Base.Set.t ->
  (unit -> 'a OpamProcess.job) -> ('a * t) OpamProcess.job

(** Removes the added and kind-changed items unless their contents changed and
    [force] isn't set, and prints warnings for other changes unless [verbose] is
    set to [false]. Ignores non-existing files.
    [title] is used to prefix messages if specified. *)
val revert:
  ?title:string -> ?verbose:bool -> ?force:bool -> ?dryrun:bool ->
  OpamFilename.Dir.t -> t -> unit

(** Checks the items that were added or kind-changed in the given diff, and
    returns their status *)
val check:
  OpamFilename.Dir.t -> t ->
  (OpamFilename.t * [`Unchanged | `Removed | `Changed]) list

(** Reload all the digests from the directory [prefix]. Remove a file
    from the map if it has been removed from the file-system. *)
val update : OpamFilename.Dir.t -> t -> t

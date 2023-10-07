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

open OpamTypes

(** Configuration init and handling of downloading commands *)

exception Download_fail of string option * string
exception Checksum_mismatch of OpamHash.t

(** downloads a file from an URL, using Curl, Wget, or a custom configured
    tool, to the given directory. Returns the downloaded filename.
    @raise Failure if the download failed or if the checksum is specified and
    doesn't match*)
val download:
  ?quiet:bool -> ?validate:bool -> overwrite:bool -> ?compress:bool ->
  ?checksum:OpamHash.t ->
  OpamUrl.t -> OpamFilename.Dir.t ->
  OpamFilename.t OpamProcess.job

(** As [download], but with a specified output filename. *)
val download_as:
  ?quiet:bool -> ?validate:bool -> overwrite:bool -> ?compress:bool ->
  ?checksum:OpamHash.t ->
  OpamUrl.t -> OpamFilename.t ->
  unit OpamProcess.job


(** Software Heritage fallback *)
module SWHID: sig

  (* [archive_fallback ?timeout url dirnames] downloads archived archive from
     SWH platform and copies it in target directory. [dirnames] is the list of
     dirnames in which the archive will be copied:
     string label * dirname * subpath option. *)
  val archive_fallback:
    ?max_tries:int -> OpamFile.URL.t ->
    (string * dirname * subpath option) list ->
    string option download OpamProcess.job
end

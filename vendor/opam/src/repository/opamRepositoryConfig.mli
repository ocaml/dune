(**************************************************************************)
(*                                                                        *)
(*    Copyright 2015-2016 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Configuration options for the repository lib (record, global reference,
    setter, initialisation) *)

module E : sig
  type OpamStd.Config.E.t +=
    | CURL of string option
    | FETCH of string option
    | NOCHECKSUMS of bool option
    | REPOSITORYTARRING of bool option
    | REQUIRECHECKSUMS of bool option
    | RETRIES of int option
    | VALIDATIONHOOK of string option

  val curl: unit -> string option
  val fetch: unit -> string option

 (* Non lazy access *)
  val curl_t: unit -> string option
  val fetch_t: unit -> string option
end

(** Toggles parsing of the tool's output to detect errors
    (curl returns 0 on a 404) *)
type dl_tool_kind = [ `Curl | `Default ]

type t = {
  download_tool: (OpamTypes.arg list * dl_tool_kind) Lazy.t;
  validation_hook: OpamTypes.arg list option;
  retries: int;
  force_checksums: bool option;
  repo_tarring : bool;
}

type 'a options_fun =
  ?download_tool:(OpamTypes.arg list * dl_tool_kind) Lazy.t ->
  ?validation_hook:OpamTypes.arg list option ->
  ?retries:int ->
  ?force_checksums:bool option ->
  ?repo_tarring:bool ->
  'a

include OpamStd.Config.Sig
  with type t := t
   and type 'a options_fun := 'a options_fun

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamStd.Op

module type ARG = sig
  type t
  val name: string
end

module Make (X: ARG): sig

  type t = X.t

  val save: OpamFilename.t -> t -> unit

  val load: OpamFilename.t -> t option

  val remove: OpamFilename.t -> unit

end = struct

  let log fmt = OpamConsole.log (Printf.sprintf "CACHE(%s)" X.name) fmt
  let slog = OpamConsole.slog

  type t = X.t

  let check_marshaled_file fd =
    try
    let ic = Unix.in_channel_of_descr fd in
    let this_magic = OpamVersion.magic () in
    let magic_len = String.length this_magic in
    let file_magic =
      let b = Bytes.create magic_len in
      really_input ic b 0 magic_len;
      Bytes.to_string b in
    if not OpamCoreConfig.developer &&
      file_magic <> this_magic then (
      log "Bad %s cache: incompatible magic string %S (expected %S)."
        X.name file_magic this_magic;
      None
    ) else
      Some ic
    with e ->
      OpamStd.Exn.fatal e;
      log "Bad %s cache: %s" X.name (Printexc.to_string e);
      None

  let marshal_from_file file fd =
    let chrono = OpamConsole.timer () in
    let f ic =
      try
        let (cache: t) = Marshal.from_channel ic in
        log "Loaded %a in %.3fs" (slog OpamFilename.to_string) file (chrono ());
        Some cache
      with End_of_file | Failure _ ->
        log "Bad %s cache: likely a truncated file, ignoring." X.name;
        None
    in
    OpamStd.Option.Op.(check_marshaled_file fd >>= f)

  let load cache_file =
    match OpamFilename.opt_file cache_file with
    | Some file ->
        let r =
          OpamFilename.with_flock `Lock_read file @@ fun fd ->
          marshal_from_file file fd
        in
        if r = None then begin
          log "Invalid %s cache, removing" X.name;
          OpamFilename.remove file
        end;
        r
    | None -> None

  let save cache_file t =
    if OpamCoreConfig.(!r.safe_mode) then
      log "Running in safe mode, not upgrading the %s cache" X.name
    else
    try
      let chrono = OpamConsole.timer () in
      OpamFilename.with_flock `Lock_write cache_file @@ fun fd ->
      log "Writing the %s cache to %s ..."
        X.name (OpamFilename.prettify cache_file);
      let oc = Unix.out_channel_of_descr fd in
      output_string oc (OpamVersion.magic ());
      Marshal.to_channel oc t [];
      flush oc;
      log "%a written in %.3fs" (slog OpamFilename.prettify) cache_file (chrono ())
    with Unix.Unix_error _ ->
      log "Could not acquire lock for writing %s, skipping %s cache update"
        (OpamFilename.prettify cache_file) X.name

  let remove cache_file =
    OpamFilename.remove cache_file

end

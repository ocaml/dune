(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamStd.Op
open OpamProcess.Job.Op

let log msg = OpamConsole.log "CURL" msg
let slog = OpamConsole.slog

let index_archive_name = "index.tar.gz"

let remote_index_archive url = OpamUrl.Op.(url / index_archive_name)

let sync_state name destdir url =
  OpamFilename.with_tmp_dir_job @@ fun dir ->
  let local_index_archive = OpamFilename.Op.(dir // index_archive_name) in
  OpamDownload.download_as ~quiet:true ~overwrite:true
    (remote_index_archive url)
    local_index_archive
  @@+ fun () ->
  List.iter OpamFilename.rmdir (OpamFilename.dirs destdir);
  OpamProcess.Job.with_text
    (Printf.sprintf "[%s: unpacking]"
       (OpamConsole.colorise `green (OpamRepositoryName.to_string name))) @@
  OpamFilename.extract_in_job local_index_archive destdir @@+ function
    | None -> Done ()
    | Some err -> raise err

module B = struct

  let name = `http

  let fetch_repo_update repo_name ?cache_dir:_ repo_root url =
    log "pull-repo-update";
    let quarantine =
      OpamFilename.Dir.(of_string (to_string repo_root ^ ".new"))
    in
    OpamFilename.mkdir quarantine;
    let finalise () = OpamFilename.rmdir quarantine in
    OpamProcess.Job.catch (fun e ->
        finalise ();
        Done (OpamRepositoryBackend.Update_err e))
    @@ fun () ->
    OpamRepositoryBackend.job_text repo_name "sync"
      (sync_state repo_name quarantine url) @@+ fun () ->
    if not (OpamFilename.exists_dir repo_root) ||
       OpamFilename.dir_is_empty repo_root then
      Done (OpamRepositoryBackend.Update_full quarantine)
    else
      OpamProcess.Job.finally finalise @@ fun () ->
      OpamRepositoryBackend.job_text repo_name "diff"
        (OpamRepositoryBackend.get_diff
           (OpamFilename.dirname_dir repo_root)
           (OpamFilename.basename_dir repo_root)
           (OpamFilename.basename_dir quarantine))
      @@| function
      | None -> OpamRepositoryBackend.Update_empty
      | Some patch -> OpamRepositoryBackend.Update_patch patch

  let repo_update_complete _ _ = Done ()

  let pull_url ?cache_dir:_ ?subpath:_ dirname checksum remote_url =
    log "pull-file into %a: %a"
      (slog OpamFilename.Dir.to_string) dirname
      (slog OpamUrl.to_string) remote_url;
    OpamProcess.Job.catch
      (fun e ->
        match e with
        | OpamDownload.Checksum_mismatch e -> 
          Done (Checksum_mismatch e)
        | _ ->
         OpamStd.Exn.fatal e;
         let s,l =
           let str = Printf.sprintf "%s (%s)" (OpamUrl.to_string remote_url) in
           match e with
           | OpamDownload.Download_fail (s,l) -> s, str l
           | _ -> Some "Download failed", str "download failed"
         in
         Done (Not_available (s,l)))
    @@ fun () ->
    OpamDownload.download ~quiet:true ~overwrite:true ?checksum remote_url dirname
    @@+ fun local_file -> Done (Result (Some local_file))

  let revision _ =
    Done None

  let sync_dirty ?subpath:_ dir url = pull_url dir None url
  (* do not propagate *)

  let get_remote_url ?hash:_ _ =
    Done None

end

(* Helper functions used by opam-admin *)

let make_index_tar_gz repo_root =
  OpamFilename.in_dir repo_root (fun () ->
    let to_include = [ "version"; "packages"; "repo" ] in
    match List.filter Sys.file_exists to_include with
    | [] -> ()
    | d  -> OpamSystem.command ("tar" :: "czhf" :: "index.tar.gz" :: "--exclude=.git*" :: d)
  )

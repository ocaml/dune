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
open OpamProcess.Job.Op

let log fmt = OpamConsole.log "RSYNC" fmt

(* Rsync args recap:
  - recurse into directories (r)
  - skip based on checksum, not mod-time & size (c)
  - preserve permissions (p), times (t), group (g), owner (o),
    device & special files (D)
  - transform symlink into referent file/dir (L)
  *)
let rsync_arg = "-rLptgoDvc"

(* if rsync -arv return 4 lines, this means that no files have changed *)
let rsync_trim = function
  | [] -> []
  | _ :: t ->
    match List.rev t with
    | _ :: _ :: _ :: l -> List.filter ((<>) "./") l
    | _ -> []

let convert_path =
  OpamSystem.get_cygpath_function ~command:"rsync"

let call_rsync check args =
  OpamSystem.make_command "rsync" args
  @@> fun r ->
  match r.OpamProcess.r_code with
  | 0 -> Done (Some (rsync_trim r.OpamProcess.r_stdout))
  | 3 | 5 | 10 | 11 | 12 -> (* protocol or file errors *)
    Done None
  | 20 -> (* signal *)
    raise Sys.Break
  | 23 | 24 ->
    (* partial, mostly mode, link or perm errors. But may also be a
       complete error so we do an additional check *)
    if check () then
      (OpamConsole.warning "Rsync partially failed:\n%s"
         (OpamStd.Format.itemize ~bullet:"" (fun x -> x) r.OpamProcess.r_stderr);
       Done (Some (rsync_trim r.OpamProcess.r_stdout)))
    else Done None
  | 30 | 35 -> (* timeouts *)
    Done None
  | _ -> OpamSystem.process_error r

let rsync ?(args=[]) ?(exclude_vcdirs=true) src dst =
  log "rsync: src=%s dst=%s" src dst;
  let remote = String.contains src ':' in
  let overlap src dst =
    let norm d = Filename.concat d "" in
    OpamStd.String.starts_with ~prefix:(norm src) (norm dst) &&
    not (OpamStd.String.contains ~sub:OpamSwitch.external_dirname (norm dst)) ||
    OpamStd.String.starts_with ~prefix:(norm dst) (norm src)
  in
  (* See also [OpamVCS.sync_dirty] *)
  let exclude_args =
    (if not exclude_vcdirs then [] else
       [ "--exclude"; ".git";
         "--exclude"; "_darcs";
         "--exclude"; ".hg";
       ])
    @ [
      "--exclude"; ".#*";
      "--exclude"; OpamSwitch.external_dirname ^ "*";
      "--exclude"; "_build";
    ]
  in
  if not(remote || Sys.file_exists src) then
    Done (Not_available (None, src))
  else if src = dst then
    Done (Up_to_date [])
  else if overlap src dst then
    (OpamConsole.error "Cannot sync %s into %s: they overlap" src dst;
     Done (Not_available (None, src)))
  else (
    OpamSystem.mkdir dst;
    let convert_path = Lazy.force convert_path in
    call_rsync (fun () -> not (OpamSystem.dir_is_empty dst))
      ( rsync_arg :: args @ exclude_args @
        [ "--delete"; "--delete-excluded"; convert_path src; convert_path dst; ])
    @@| function
    | None -> Not_available (None, src)
    | Some [] -> Up_to_date []
    | Some lines -> Result lines
  )

let is_remote url = url.OpamUrl.transport <> "file"

let rsync_dirs ?args ?exclude_vcdirs url dst =
  let src_s = OpamUrl.(Op.(url / "").path) in (* Ensure trailing '/' *)
  let dst_s = OpamFilename.Dir.to_string dst in
  if not (is_remote url) &&
     not (OpamFilename.exists_dir (OpamFilename.Dir.of_string src_s))
  then
    Done (Not_available (None, Printf.sprintf "Directory %s does not exist" src_s))
  else
  rsync ?args ?exclude_vcdirs src_s dst_s @@| function
  | Not_available _ as na -> na
  | Result _ ->
    if OpamFilename.exists_dir dst then Result dst
    else Not_available (None, dst_s)
  | Up_to_date _ -> Up_to_date dst

let rsync_file ?(args=[]) url dst =
  let src_s = url.OpamUrl.path in
  let dst_s = OpamFilename.to_string dst in
  log "rsync_file src=%s dst=%s" src_s dst_s;
  if not (is_remote url || OpamFilename.(exists (of_string src_s))) then
    Done (Not_available (None, src_s))
  else if src_s = dst_s then
    Done (Up_to_date dst)
  else
    (OpamFilename.mkdir (OpamFilename.dirname dst);
     let convert_path = Lazy.force convert_path in
     call_rsync (fun () -> Sys.file_exists dst_s)
       ( rsync_arg :: args @ [ convert_path src_s; convert_path dst_s ])
     @@| function
     | None -> Not_available (None, src_s)
     | Some [] -> Up_to_date dst
     | Some [_] ->
       if OpamFilename.exists dst then Result dst
       else Not_available (None, src_s)
     | Some l ->
       OpamSystem.internal_error
         "unknown rsync output: {%s}"
         (String.concat ", " l))

module B = struct

  let name = `rsync

  let pull_dir_quiet local_dirname url =
    rsync_dirs url local_dirname

  let fetch_repo_update repo_name ?cache_dir:_ repo_root url =
    log "pull-repo-update";
    let quarantine =
      OpamFilename.Dir.(of_string (to_string repo_root ^ ".new"))
    in
    let finalise () = OpamFilename.rmdir quarantine in
    OpamProcess.Job.catch (fun e ->
        finalise ();
        Done (OpamRepositoryBackend.Update_err e))
    @@ fun () ->
    OpamRepositoryBackend.job_text repo_name "sync"
      (match OpamUrl.local_dir url with
       | Some dir ->
         OpamFilename.copy_dir ~src:dir ~dst:quarantine;
         (* fixme: Would be best to symlink, but at the moment our filename api
            isn't able to cope properly with the symlinks afterwards
            OpamFilename.link_dir ~target:dir ~link:quarantine; *)
         Done (Result quarantine)
       | None ->
         if OpamFilename.exists_dir repo_root then
           OpamFilename.copy_dir ~src:repo_root ~dst:quarantine
         else
           OpamFilename.mkdir quarantine;
         pull_dir_quiet quarantine url) @@+ function
    | Not_available _ ->
      finalise ();
      Done (OpamRepositoryBackend.Update_err (Failure "rsync failed"))
    | Up_to_date _ ->
      finalise (); Done OpamRepositoryBackend.Update_empty
    | Result _ ->
      if not (OpamFilename.exists_dir repo_root) ||
         OpamFilename.dir_is_empty repo_root then
        Done (OpamRepositoryBackend.Update_full quarantine)
      else
        OpamProcess.Job.finally finalise @@ fun () ->
        OpamRepositoryBackend.job_text repo_name "diff" @@
        OpamRepositoryBackend.get_diff
          (OpamFilename.dirname_dir repo_root)
          (OpamFilename.basename_dir repo_root)
          (OpamFilename.basename_dir quarantine)
        @@| function
        | None -> OpamRepositoryBackend.Update_empty
        | Some p -> OpamRepositoryBackend.Update_patch p

  let repo_update_complete _ _ = Done ()

  let pull_url ?cache_dir:_ ?subpath local_dirname _checksum remote_url =
    let local_dirname = OpamFilename.SubPath.(local_dirname /? subpath) in
    OpamFilename.mkdir local_dirname;
    let dir = OpamFilename.Dir.to_string local_dirname in
    let remote_url =
      OpamStd.Option.map_default (fun x -> OpamUrl.Op.(remote_url / OpamFilename.SubPath.to_string x))
        remote_url subpath
    in
    let remote_url =
      match OpamUrl.local_dir remote_url with
      | Some _ ->
        (* ensure that rsync doesn't recreate a subdir: add trailing '/' *)
        OpamUrl.Op.(remote_url / "")
      | None -> remote_url
    in
    rsync remote_url.OpamUrl.path dir
    @@| function
    | Not_available _ as na -> na
    | (Result _ | Up_to_date _) as r ->
      let res x = match r with
        | Result _ -> Result x
        | Up_to_date _ -> Up_to_date x
        | _ -> assert false
      in
      if OpamUrl.has_trailing_slash remote_url then
        res None
      else
      let filename =
        OpamFilename.Op.(local_dirname // OpamUrl.basename remote_url)
      in
      if OpamFilename.exists filename then res (Some filename)
      else
        Not_available
          (None, Printf.sprintf
             "Could not find target file %s after rsync with %s. \
              Perhaps you meant %s/ ?"
             (OpamUrl.basename remote_url)
             (OpamUrl.to_string remote_url)
             (OpamUrl.to_string remote_url))

  let revision _ =
    Done None

  let sync_dirty ?subpath dir url = pull_url ?subpath dir None url

  let get_remote_url ?hash:_ _ =
    Done None

end

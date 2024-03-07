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

let log fmt = OpamConsole.log "REPOSITORY" fmt
let slog = OpamConsole.slog


let find_backend_by_kind = function
  | `http -> (module OpamHTTP.B: OpamRepositoryBackend.S)
  | `rsync -> (module OpamLocal.B: OpamRepositoryBackend.S)
  | `git -> (module OpamGit.B: OpamRepositoryBackend.S)
  | `hg -> (module OpamHg.B: OpamRepositoryBackend.S)
  | `darcs -> (module OpamDarcs.B: OpamRepositoryBackend.S)

let find_vcs_backend = function
  | `git -> (module OpamGit.VCS: OpamVCS.VCS)
  | `hg -> (module OpamHg.VCS: OpamVCS.VCS)
  | `darcs -> (module OpamDarcs.VCS: OpamVCS.VCS)

let url_backend url = find_backend_by_kind url.OpamUrl.backend

let find_backend r = url_backend r.repo_url

let cache_url root_cache_url checksum =
  List.fold_left OpamUrl.Op.(/) root_cache_url
    (OpamHash.to_path checksum)

let cache_file cache_dir checksum =
  let rec aux acc = function
    | [f] -> OpamFilename.Op.(acc // f)
    | d::d1 -> aux OpamFilename.Op.(acc / d) d1
    | [] -> assert false
  in
  aux cache_dir (OpamHash.to_path checksum)

let fetch_from_cache =
  let currently_downloading = ref [] in
  let rec no_concurrent_dls key f x =
    if List.mem key !currently_downloading then
      Run (OpamProcess.command "sleep" ["1"],
           (fun _ -> no_concurrent_dls key f x))
    else
      (currently_downloading := key :: !currently_downloading;
       OpamProcess.Job.finally
         (fun () ->
            currently_downloading :=
              List.filter (fun k -> k <> key) !currently_downloading)
         (fun () -> f x))
  in
  fun cache_dir cache_urls checksums ->
  let mismatch file =
    OpamConsole.error
      "Conflicting file hashes, or broken or compromised cache!\n%s"
      (OpamStd.Format.itemize (fun ck ->
           OpamHash.to_string ck ^
           if OpamHash.check_file (OpamFilename.to_string file) ck
           then OpamConsole.colorise `green " (match)"
           else OpamConsole.colorise `red " (MISMATCH)")
          checksums);
    OpamFilename.remove file;
    let m = "cache CONFLICT" in
    Done (Not_available (Some m, m))
  in
  let dl_from_cache_job root_cache_url checksum file =
    let url = cache_url root_cache_url checksum in
    match url.OpamUrl.backend with
    | `http ->
      OpamDownload.download_as
        ~quiet:true ~validate:false ~overwrite:true ~checksum
        url file
    | `rsync ->
      begin match OpamUrl.local_file url with
        | Some src ->
          OpamFilename.copy ~src ~dst:file;
          OpamProcess.Job.Op.Done ()
        | None ->
          (OpamLocal.rsync_file url file @@| function
            | Result _ | Up_to_date _-> ()
            | Not_available (s,l) -> raise (OpamDownload.Download_fail (s,l)))
      end
    | #OpamUrl.version_control ->
      failwith "Version control not allowed as cache URL"
  in
  try
    let hit_file =
      OpamStd.List.find_map (fun ck ->
          let f = cache_file cache_dir ck in
          if OpamFilename.exists f then Some f else None)
        checksums
    in
    if List.for_all
        (fun ck -> OpamHash.check_file (OpamFilename.to_string hit_file) ck)
        checksums
    then Done (Up_to_date (hit_file, OpamUrl.empty))
    else mismatch hit_file
  with Not_found -> match checksums with
    | [] -> let m = "cache miss" in Done (Not_available (Some m, m))
    | checksum::_ ->
      (* Try all cache urls in order, but only the first checksum *)
      let local_file = cache_file cache_dir checksum in
      let tmpfile = OpamFilename.add_extension local_file "tmp" in
      let rec try_cache_dl = function
        | [] -> let m = "cache miss" in Done (Not_available (Some m, m))
        | root_cache_url::other_caches ->
          OpamProcess.Job.catch
            (function Failure _
                    | OpamDownload.Download_fail _ ->
                      try_cache_dl other_caches
                    | e -> raise e)
          @@ fun () ->
          dl_from_cache_job root_cache_url checksum tmpfile
          @@+ fun () ->
          if List.for_all (OpamHash.check_file (OpamFilename.to_string tmpfile))
              checksums
          then
            (OpamFilename.move ~src:tmpfile ~dst:local_file;
             Done (Result (local_file, root_cache_url)))
          else mismatch tmpfile
      in
      no_concurrent_dls checksum try_cache_dl cache_urls

let validate_and_add_to_cache label url cache_dir file checksums =
  try
    let mismatch, expected =
      OpamStd.List.find_map (fun c ->
          match OpamHash.mismatch (OpamFilename.to_string file) c with
          | Some found -> Some (found, c)
          | None -> None)
        checksums
    in
    OpamConsole.error "%s: Checksum mismatch for %s:\n\
                      \  expected %s\n\
                      \  got      %s"
      label (OpamUrl.to_string url)
      (OpamHash.to_string expected)
      (OpamHash.to_string mismatch);
    OpamFilename.remove file;
    `Expected expected
  with Not_found ->
    (let checksums = OpamHash.sort checksums in
     match cache_dir, checksums with
     | Some dir, best_chks :: others_chks ->
       OpamFilename.copy ~src:file ~dst:(cache_file dir best_chks);
       List.iter (fun checksum ->
           let target = cache_file dir best_chks in
           let link = cache_file dir checksum in
           try
             OpamFilename.link ~relative:true ~target ~link
           with Sys_error _ -> ())
         others_chks;
     | _ -> ());
    `Match

(* [cache_dir] used to add to cache only *)
let pull_from_upstream
    label ?(working_dir=false) ?subpath cache_dir destdir checksums url =
  let module B = (val url_backend url: OpamRepositoryBackend.S) in
  let cksum = match checksums with [] -> None | c::_ -> Some c in
  let text =
    OpamProcess.make_command_text label
      (OpamUrl.string_of_backend url.OpamUrl.backend)
  in
  OpamProcess.Job.with_text text @@
  (if working_dir then B.sync_dirty ?subpath destdir url
   else
   let pin_cache_dir = OpamRepositoryPath.pin_cache url in
   let url, pull =
     if OpamUrl.(match url.backend with | #version_control -> false | _ -> true)
     && OpamFilename.exists_dir pin_cache_dir then
       (log "Pin cache existing for %s : %s\n"
          (OpamUrl.to_string url) @@ OpamFilename.Dir.to_string pin_cache_dir;
        let rsync =
          OpamUrl.parse ~backend:`rsync ~from_file:false
          @@ OpamFilename.Dir.to_string pin_cache_dir
        in
        let pull =
          let module BR = (val url_backend rsync: OpamRepositoryBackend.S) in
          BR.pull_url
        in
        rsync, pull
       )
     else if OpamUrl.(match url.backend with | `git -> true | _ -> false)
          && OpamFilename.exists_dir pin_cache_dir then
       (log "Pin cache (git) existing for %s : %s\n"
          (OpamUrl.to_string url) @@ OpamFilename.Dir.to_string pin_cache_dir;
        let git_cached =
          OpamUrl.parse ~backend:`git
          @@ OpamFilename.Dir.to_string pin_cache_dir
        in
        let pull =
          let module BR = (val url_backend git_cached: OpamRepositoryBackend.S) in
          BR.pull_url
        in
        git_cached, pull
       )
     else url, B.pull_url
   in
   pull ?cache_dir ?subpath destdir cksum url
  )
  @@| function
  | (Result (Some file) | Up_to_date (Some file)) as ret ->
    if OpamRepositoryConfig.(!r.force_checksums) = Some false then
      ret
    else
      begin match validate_and_add_to_cache label url cache_dir file checksums with
      | `Expected e ->
          Checksum_mismatch e
      | `Match -> ret
      end
  | (Result None | Up_to_date None) as ret -> ret
  | Checksum_mismatch _ as na -> na
  | Not_available _ as na -> na

let pull_from_mirrors label ?working_dir ?subpath cache_dir destdir checksums urls =
  let rec aux = function
    | [] -> invalid_arg "pull_from_mirrors: empty mirror list"
    | [url] ->
      pull_from_upstream label ?working_dir ?subpath cache_dir destdir checksums url
      @@| fun r -> url, r
    | url::mirrors ->
      pull_from_upstream label ?working_dir ?subpath cache_dir destdir checksums url
      @@+ function
      | Not_available (_,s) ->
        OpamConsole.warning "%s: download of %s failed (%s), trying mirror"
          label (OpamUrl.to_string url) s;
        aux mirrors
      | r -> Done (url, r)
  in
  aux urls @@| function
  | url, (Result None | Up_to_date None) when checksums <> [] ->
    OpamConsole.error "%s: file checksum specified, but a directory was \
                       retrieved from %s"
      label (OpamUrl.to_string url);
    OpamFilename.rmdir destdir;
    let m = "can't check directory checksum" in
    url, Not_available (Some m, m)
  | ret -> ret

(* handle subpathes *)
let pull_tree_t
    ?cache_dir ?(cache_urls=[]) ?working_dir
    dirnames checksums remote_urls =
  let extract_archive =
    let fallback success = function
      | None -> success ()
      | Some (Failure s) ->
        Done (Not_available (Some s, "Could not extract archive:\n"^s))
      | Some (OpamSystem.Process_error pe) ->
        Done (Not_available (Some (OpamProcess.result_summary pe),
                             OpamProcess.string_of_result pe))
      | Some e -> Done (Not_available (None, Printexc.to_string e))
    in
    match dirnames with
    | [ _label, local_dirname, _subpath ] ->
      (fun archive msg ->
         OpamFilename.cleandir local_dirname;
         OpamFilename.extract_job archive local_dirname
         @@+ fallback (fun () ->  Done (Up_to_date msg)))
    | _ ->
      fun archive msg ->
        OpamFilename.with_tmp_dir_job @@ fun tmpdir ->
        let copies () = failwith "stubbed out" in
        OpamFilename.extract_job archive tmpdir
        @@+ fallback (fun () ->
            let failing =
              OpamStd.List.filter_map (function
                  | Result _ | Up_to_date _ -> None
                  | Not_available (Some s,l) -> Some (s,l)
                  | Not_available (None, _) -> assert false
                ) (copies ())
            in
            if failing = [] then Done (Up_to_date msg) else
            let simple =
              Printf.sprintf "Failed to copy source of %s"
                (OpamStd.Format.pretty_list (List.map fst failing))
            in
            let long =
              Printf.sprintf "Failed to copy source of:\n%s"
                (OpamStd.Format.itemize (fun (nv, msg) ->
                     Printf.sprintf "%s: %s" nv msg)
                    failing)
            in
            Done (Not_available (Some simple, long)))
  in
  let label = OpamStd.List.concat_map ", " (fun (x,_,_) -> x) dirnames in
  (match cache_dir with
   | Some cache_dir ->
     let text = OpamProcess.make_command_text label "dl" in
     OpamProcess.Job.with_text text @@
     fetch_from_cache cache_dir cache_urls checksums
   | None ->
     assert (cache_urls = []);
     let m = "no cache" in
     Done (Not_available (Some m, m)))
  @@+ function
  | Checksum_mismatch e -> Done (Checksum_mismatch e)
  | Up_to_date (archive, _) ->
    extract_archive archive "cached"
  | Result (archive, url) ->
    let msg = match url.OpamUrl.backend with
      | `rsync -> url.OpamUrl.path
      | _ -> OpamUrl.to_string url
    in
    extract_archive archive msg
  | Not_available _ ->
    if checksums = [] && OpamRepositoryConfig.(!r.force_checksums = Some true)
    then
      Done (
        Not_available (
          Some ("missing checksum"),
          label ^ ": Missing checksum, and `--require-checksums` was set."))
    else
      OpamFilename.with_tmp_dir_job @@ fun tmpdir ->
      let extract url archive =
        match dirnames with
        | [_] ->
          let tmp_archive = OpamFilename.(create tmpdir (basename archive)) in
          OpamFilename.move ~src:archive ~dst:tmp_archive;
          extract_archive tmp_archive url
        | _ -> extract_archive archive url
      in
      let pull label checksums remote_urls =
        match dirnames with
        | [ label, local_dirname, subpath ] ->
          pull_from_mirrors label ?working_dir ?subpath cache_dir local_dirname
            checksums remote_urls
          @@| fun (url, res) ->
          (OpamUrl.to_string_w_subpath subpath url),
          res
        | _ ->
          pull_from_mirrors label ?working_dir cache_dir tmpdir
            checksums remote_urls
          @@| fun (url, res) -> OpamUrl.to_string url, res
      in
      pull label checksums remote_urls
      @@+ function
      | _, Up_to_date None -> Done (Up_to_date "no changes")
      | url, (Up_to_date (Some archive) | Result (Some archive)) ->
        extract url archive
      | url, Result None -> Done (Result url)
      | _, (Checksum_mismatch _ as na) -> Done na
      | _, (Not_available _ as na) -> Done na


let pull_tree label ?cache_dir ?(cache_urls=[]) ?working_dir ?subpath
    local_dirname  =
  pull_tree_t ?cache_dir ~cache_urls ?working_dir
  [label, local_dirname, subpath]

let pull_shared_tree ?cache_dir ?(cache_urls=[]) dirnames checksums remote_urls =
  pull_tree_t ?cache_dir ~cache_urls dirnames checksums remote_urls

let revision dirname url =
  let kind = url.OpamUrl.backend in
  let module B = (val find_backend_by_kind kind: OpamRepositoryBackend.S) in
  B.revision dirname

let pull_file label ?cache_dir ?(cache_urls=[])  ?(silent_hits=false)
    file checksums remote_urls =
  (match cache_dir with
   | Some cache_dir ->
     let text = OpamProcess.make_command_text label "dl" in
     OpamProcess.Job.with_text text @@
     fetch_from_cache cache_dir cache_urls checksums
   | None ->
     assert (cache_urls = []);
     let m = "no cache" in
     Done (Not_available (Some m, m)))
  @@+ function
  | Checksum_mismatch e -> Done (Checksum_mismatch e)
  | Up_to_date (f, _) ->
    if not silent_hits then
      OpamConsole.msg "[%s] found in cache\n"
        (OpamConsole.colorise `green label);
    OpamFilename.copy ~src:f ~dst:file;
    Done (Result ())
  | Result (f, url) ->
    OpamConsole.msg "[%s] downloaded from %s\n"
      (OpamConsole.colorise `green label)
      (OpamUrl.to_string url);
    OpamFilename.copy ~src:f ~dst:file;
    Done (Result ())
  | Not_available _ ->
    if checksums = [] && OpamRepositoryConfig.(!r.force_checksums = Some true)
    then
      Done (
        Not_available
          (Some "missing checksum",
           label ^ ": Missing checksum, and `--require-checksums` was set."))
    else
      OpamFilename.with_tmp_dir_job (fun tmpdir ->
          pull_from_mirrors label cache_dir tmpdir checksums remote_urls
          @@| function
          | _, Up_to_date _ -> assert false
          | _, Result (Some f) -> OpamFilename.move ~src:f ~dst:file; Result ()
          | _, Result None -> let m = "is a directory" in Not_available (Some m, m)
          | _, (Checksum_mismatch _ as na) -> na
          | _, (Not_available _ as na) -> na)

let pull_file_to_cache label ~cache_dir ?(cache_urls=[]) checksums remote_urls =
  let text = OpamProcess.make_command_text label "dl" in
  OpamProcess.Job.with_text text @@
  fetch_from_cache cache_dir cache_urls checksums @@+ function
  | Checksum_mismatch e -> Done (Checksum_mismatch e)
  | Up_to_date (_, _) ->
    Done (Up_to_date "cached")
  | Result (_, url) ->
    Done (Result (OpamUrl.to_string url))
  | Not_available _ ->
    OpamFilename.with_tmp_dir_job (fun tmpdir ->
        pull_from_mirrors label (Some cache_dir) tmpdir checksums remote_urls
        @@| function
        | _, Up_to_date _ -> assert false
        | url, Result (Some _) -> Result (OpamUrl.to_string url)
        | _, Result None -> let m = "is a directory" in Not_available (Some m, m)
        | _, (Not_available _ as na) -> na)

let packages repo_root =
  OpamPackage.list (OpamRepositoryPath.packages_dir repo_root)

let packages_with_prefixes repo_root =
  OpamPackage.prefixes (OpamRepositoryPath.packages_dir repo_root)

let validate_repo_update repo repo_root update =
  match
    repo.repo_trust,
    OpamRepositoryConfig.(!r.validation_hook),
    OpamRepositoryConfig.(!r.force_checksums)
  with
  | None, Some _, Some true ->
    OpamConsole.error
      "No trust anchors for repository %s, and security was enforced: \
       not updating"
      (OpamRepositoryName.to_string repo.repo_name);
    Done false
  | None, _, _ | _, None, _ | _, _, Some false ->
    Done true
  | Some ta, Some hook, _ ->
    let cmd =
      let open OpamRepositoryBackend in
      let env v = match OpamVariable.Full.to_string v, update with
        | "anchors", _ -> Some (S (String.concat "," ta.fingerprints))
        | "quorum", _ -> Some (S (string_of_int ta.quorum))
        | "repo", _ -> Some (S (OpamFilename.Dir.to_string repo_root))
        | "patch", Update_patch f -> Some (S (OpamFilename.to_string f))
        | "incremental", Update_patch _ -> Some (B true)
        | "incremental", _ -> Some (B false)
        | "dir", Update_full d -> Some (S (OpamFilename.Dir.to_string d))
        | _ -> None
      in
      match OpamFilter.single_command env hook with
      | cmd::args ->
        OpamSystem.make_command
          ~name:"validation-hook"
          ~verbose:OpamCoreConfig.(!r.verbose_level >= 2)
          cmd args
      | [] -> failwith "Empty validation hook"
    in
    cmd @@> fun r ->
    log "validation: %s" (OpamProcess.result_summary r);
    Done (OpamProcess.check_success_and_cleanup r)

open OpamRepositoryBackend

let apply_repo_update repo repo_root = function
  | Update_full d ->
    log "%a: applying update from scratch at %a"
      (slog OpamRepositoryName.to_string) repo.repo_name
      (slog OpamFilename.Dir.to_string) d;
    OpamFilename.rmdir repo_root;
    if OpamFilename.is_symlink_dir d then
      (OpamFilename.copy_dir ~src:d ~dst:repo_root;
       OpamFilename.rmdir d)
    else
      OpamFilename.move_dir ~src:d ~dst:repo_root;
    OpamConsole.msg "[%s] Initialised\n"
      (OpamConsole.colorise `green
         (OpamRepositoryName.to_string repo.repo_name));
    Done ()
  | Update_patch f ->
    OpamConsole.msg "[%s] synchronised from %s\n"
      (OpamConsole.colorise `green
         (OpamRepositoryName.to_string repo.repo_name))
      (OpamUrl.to_string repo.repo_url);
    log "%a: applying patch update at %a"
      (slog OpamRepositoryName.to_string) repo.repo_name
      (slog OpamFilename.to_string) f;
    let preprocess =
      match repo.repo_url.OpamUrl.backend with
      | `http | `rsync -> false
      | _ -> true
    in
    (OpamFilename.patch ~preprocess f repo_root @@+ function
      | Some e ->
        if not (OpamConsole.debug ()) then OpamFilename.remove f;
        raise e
      | None -> OpamFilename.remove f; Done ())
  | Update_empty ->
    OpamConsole.msg "[%s] no changes from %s\n"
      (OpamConsole.colorise `green
         (OpamRepositoryName.to_string repo.repo_name))
      (OpamUrl.to_string repo.repo_url);
    log "%a: applying empty update"
      (slog OpamRepositoryName.to_string) repo.repo_name;
    Done ()
  | Update_err _ -> assert false

let cleanup_repo_update upd =
  if not (OpamConsole.debug ()) then
    match upd with
    | Update_full d -> OpamFilename.rmdir d
    | Update_patch f -> OpamFilename.remove f
    | _ -> ()

let update repo repo_root =
  log "update %a" (slog OpamRepositoryBackend.to_string) repo;
  let module B = (val find_backend repo: OpamRepositoryBackend.S) in
  B.fetch_repo_update repo.repo_name repo_root repo.repo_url @@+ function
  | Update_err e -> raise e
  | Update_empty ->
    log "update empty, no validation performed";
    apply_repo_update repo repo_root Update_empty @@+ fun () ->
    B.repo_update_complete repo_root repo.repo_url @@+ fun () ->
    Done `No_changes
  | (Update_full _ | Update_patch _) as upd ->
    OpamProcess.Job.catch (fun exn ->
        cleanup_repo_update upd;
        raise exn)
    @@ fun () ->
    validate_repo_update repo repo_root upd @@+ function
    | false ->
      cleanup_repo_update upd;
      failwith "Invalid repository signatures, update aborted"
    | true ->
      apply_repo_update repo repo_root upd @@+ fun () ->
      B.repo_update_complete repo_root repo.repo_url @@+ fun () ->
      Done `Changes

let on_local_version_control url ~default f =
  match url.OpamUrl.backend with
  | #OpamUrl.version_control as backend ->
    (match OpamUrl.local_dir url with
     | None -> default
     | Some dir ->
       f dir (find_vcs_backend backend))
  | #OpamUrl.backend -> default

let current_branch url =
  on_local_version_control url ~default:(Done None) @@
  fun dir (module VCS) -> VCS.current_branch dir

let is_dirty ?subpath url =
  on_local_version_control url ~default:(Done false) @@
  fun dir (module VCS) -> VCS.is_dirty ?subpath dir

let report_fetch_result pkg = function
  | Checksum_mismatch s ->
    let msg = "Checksum Mismatch" in
    OpamConsole.msg "[%s] fetching sources failed: %s\n"
      (OpamConsole.colorise `red (OpamPackage.to_string pkg)) msg;
      Checksum_mismatch s
  | Result msg ->
    OpamConsole.msg
      "[%s] synchronised (%s)\n"
      (OpamConsole.colorise `green (OpamPackage.to_string pkg))
      msg;
    Result ()
  | Up_to_date msg ->
    OpamConsole.msg
      "[%s] synchronised (%s)\n"
      (OpamConsole.colorise `green (OpamPackage.to_string pkg))
      msg;
    Up_to_date ()
  | Not_available (s, l) ->
    let msg = match s with None -> l | Some s -> s in
    OpamConsole.msg "[%s] fetching sources failed: %s\n"
      (OpamConsole.colorise `red (OpamPackage.to_string pkg)) msg;
    Not_available (s, l)

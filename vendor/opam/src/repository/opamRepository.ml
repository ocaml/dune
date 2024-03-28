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
            | Checksum_mismatch h -> raise (OpamDownload.Checksum_mismatch h)
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
              | OpamDownload.Download_fail _
              | OpamDownload.Checksum_mismatch _ -> try_cache_dl other_caches
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
    ~label ~local_dirname ~subpath checksums remote_urls =
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
    fun archive msg ->
      OpamFilename.cleandir local_dirname;
      OpamFilename.extract_job archive local_dirname
      @@+ fallback (fun () ->  Done (Up_to_date msg))
  in
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
          let tmp_archive = OpamFilename.(create tmpdir (basename archive)) in
          OpamFilename.move ~src:archive ~dst:tmp_archive;
          extract_archive tmp_archive url
      in
      let pull label checksums remote_urls =
        pull_from_mirrors label ?working_dir ?subpath cache_dir local_dirname
          checksums remote_urls
        @@| fun (url, res) ->
        (OpamUrl.to_string_w_subpath subpath url),
        res
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
    ~label ~local_dirname ~subpath

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

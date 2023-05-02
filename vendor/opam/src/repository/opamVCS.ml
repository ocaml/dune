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

module type VCS = sig
  val name: OpamUrl.backend
  val exists: dirname -> bool
  val init: dirname -> url -> unit OpamProcess.job
  val fetch:
    ?cache_dir:dirname -> ?subpath:subpath -> dirname -> url ->
    unit OpamProcess.job
  val reset_tree: dirname -> url -> unit OpamProcess.job
  val patch_applied: dirname -> url -> unit OpamProcess.job
  val diff: dirname -> url -> filename option OpamProcess.job
  val is_up_to_date: ?subpath:subpath -> dirname -> url -> bool OpamProcess.job
  val revision: dirname -> string option OpamProcess.job
  val versioned_files: dirname -> string list OpamProcess.job
  val vc_dir: dirname -> dirname
  val current_branch: dirname -> string option OpamProcess.job
  val is_dirty: ?subpath:subpath -> dirname -> bool OpamProcess.job
  val modified_files: dirname -> string list OpamProcess.job
  val get_remote_url: ?hash:string -> dirname -> url option OpamProcess.job
  val clean: dirname -> unit OpamProcess.job
end

let convert_path =
  OpamSystem.get_cygpath_function ~command:"rsync"

module Make (VCS: VCS) = struct

  let name = VCS.name

  let fetch_repo_update repo_name ?cache_dir repo_root repo_url =
    if VCS.exists repo_root then
      OpamProcess.Job.catch (fun e -> Done (OpamRepositoryBackend.Update_err e))
      @@ fun () ->
      OpamRepositoryBackend.job_text repo_name "sync"
        (VCS.fetch ?cache_dir repo_root repo_url)
      @@+ fun () ->
      OpamRepositoryBackend.job_text repo_name "diff"
        (VCS.diff repo_root repo_url)
      @@| function
      | None -> OpamRepositoryBackend.Update_empty
      | Some patch -> OpamRepositoryBackend.Update_patch patch
    else
      OpamProcess.Job.catch (fun e ->
          OpamFilename.rmdir repo_root;
          Done (OpamRepositoryBackend.Update_err e))
      @@ fun () ->
      OpamRepositoryBackend.job_text repo_name "init"
        (VCS.init repo_root repo_url)
      @@+ fun () ->
      OpamRepositoryBackend.job_text repo_name "sync"
        (VCS.fetch ?cache_dir repo_root repo_url)
      @@+ fun () ->
      let tmpdir = OpamFilename.Dir.(of_string (to_string repo_root ^".new")) in
      OpamFilename.copy_dir ~src:repo_root ~dst:tmpdir;
      OpamProcess.Job.catch (fun e -> OpamFilename.rmdir tmpdir; raise e)
      @@ fun () ->
      VCS.reset_tree tmpdir repo_url @@| fun () ->
      OpamRepositoryBackend.Update_full tmpdir

  let repo_update_complete dirname url =
    VCS.patch_applied dirname url @@+ fun () ->
    Done ()

  let pull_url ?cache_dir ?subpath dirname checksum url =
    if checksum <> None then invalid_arg "VC pull_url doesn't allow checksums";
    OpamProcess.Job.catch
      (fun e ->
         OpamConsole.error "Could not synchronize %s from %S:\n%s"
           (OpamFilename.Dir.to_string dirname)
           (OpamUrl.to_string url)
           (match e with Failure fw -> fw | _ -> Printexc.to_string e);
         Done (Not_available (None, OpamUrl.to_string url)))
    @@ fun () ->
    if VCS.exists dirname then
      VCS.clean dirname @@+ fun () ->
      VCS.fetch ?cache_dir ?subpath dirname url @@+ fun () ->
      VCS.is_up_to_date ?subpath dirname url @@+ function
      | true -> Done (Up_to_date None)
      | false ->
        VCS.reset_tree dirname url @@+ fun () ->
        Done (Result None)
    else
      (OpamFilename.mkdir dirname;
       VCS.init dirname url @@+ fun () ->
       VCS.fetch ?cache_dir ?subpath dirname url @@+ fun () ->
       VCS.reset_tree dirname url @@+ fun () ->
       Done (Result None))

  let revision repo_root =
    VCS.revision repo_root @@+ fun r ->
    Done (OpamStd.Option.map OpamPackage.Version.of_string r)

  let sync_dirty ?subpath repo_root repo_url =
    let filter_subpath files =
      match subpath with
      | None -> files
      | Some sp ->
        OpamStd.List.filter_map
          (fun f ->
             if OpamStd.String.remove_prefix
                 ~prefix:(OpamFilename.SubPath.to_string sp ^ Filename.dir_sep) f
                <> f then Some f else None)
          files
    in
    pull_url ?subpath repo_root None repo_url @@+ fun result ->
    match OpamUrl.local_dir repo_url with
    | None -> Done (result)
    | Some dir ->
      VCS.versioned_files dir @@+ fun vc_files ->
      VCS.modified_files dir @@+ fun vc_dirty_files ->
      let files =
        filter_subpath
          (List.map OpamFilename.(remove_prefix dir)
             (OpamFilename.rec_files dir))
      in
      (* Remove non-listed files from destination *)
      (* fixme: doesn't clean directories *)
      let fset = OpamStd.String.Set.of_list files in
      let rm_list =
        List.filter (fun f ->
            let basename = OpamFilename.remove_prefix repo_root f in
            not (OpamFilename.(starts_with (VCS.vc_dir repo_root) f)
                 || OpamStd.String.Set.mem basename fset))
          (OpamFilename.rec_files repo_root)
      in
      List.iter OpamFilename.remove rm_list;
      (* We do the list cleaning here because of rsync options: with
         `--files-from`, `--exclude` need to be explicitly given directory
         descendants, e.g `--exclude _build/**`
      *)
      let excluded =
        (* from [OpamLocal.rsync] exclude list *)
        let exc =
          [ OpamSwitch.external_dirname; "_build";
            ".git"; "_darcs"; ".hg" ]
        in
        OpamStd.String.Set.filter (fun f ->
            List.exists (fun prefix ->
                OpamStd.String.starts_with ~prefix f)
              exc)
          fset
      in
      let vcset = OpamStd.String.Set.of_list (filter_subpath vc_files) in
      let vc_dirty_set =
        OpamStd.String.Set.of_list (filter_subpath vc_dirty_files)
      in
      let final_set =
        OpamStd.String.Set.Op.(fset -- vcset ++ vc_dirty_set -- excluded)
      in
      let stdout_file =
        let f = OpamSystem.temp_file "rsync-files" in
        let fd = open_out f in
        (* Using the set here to keep the list file sorted, it helps rsync *)
        OpamStd.String.Set.iter (fun s ->
            output_string fd s; output_char fd '\n')
          final_set;
        close_out fd;
        f
      in
      let args = [
        "--files-from"; (Lazy.force convert_path) stdout_file;
      ] in
      OpamLocal.rsync_dirs ~args repo_url repo_root @@+ fun result ->
      OpamSystem.remove stdout_file;
      Done (match result with
          | Up_to_date _ when rm_list = [] -> Up_to_date None
          | Up_to_date _ | Result _ -> Result None
          | Not_available _ as na -> na)

  let get_remote_url = VCS.get_remote_url

end

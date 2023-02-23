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

open OpamFilename.Op
open OpamProcess.Job.Op

module VCS = struct

  let name = `hg

  let mark_prefix = "opam-mark"

  let exists repo_root =
    OpamFilename.exists_dir (repo_root / ".hg")

  let hg repo_root =
    let dir = OpamFilename.Dir.to_string repo_root in
    fun ?verbose ?env ?stdout args ->
      OpamSystem.make_command ~dir ?verbose ?env ?stdout "hg" args

  let init repo_root _repo_url =
    OpamFilename.mkdir repo_root;
    hg repo_root [ "init" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let mark_from_url url =
    match url.OpamUrl.hash with
    | None -> mark_prefix
    | Some fragment -> mark_prefix ^ "-" ^ fragment

  let fetch ?cache_dir:_ ?subpath:_ repo_root repo_url =
    let src = OpamUrl.base_url repo_url in
    let rev = OpamStd.Option.default "default" repo_url.OpamUrl.hash in
    let mark = mark_from_url repo_url in
    hg repo_root [ "pull"; "--rev"; rev; src ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    hg repo_root [ "bookmark"; "--force"; "--rev"; rev; mark ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let revision repo_root =
    hg repo_root [ "identify"; "--id" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> Done None
    | full::_ ->
      Done (Some full)

  let clean repo_root =
    hg repo_root ["revert"; "--all"; "--no-backup"]
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let reset_tree repo_root repo_url =
    let mark = mark_from_url repo_url in
    hg repo_root [ "update"; "--clean"; "--rev"; mark ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let patch_applied = reset_tree

  let diff repo_root repo_url =
    let patch_file = OpamSystem.temp_file ~auto_clean:false "hg-diff" in
    let finalise () = OpamSystem.remove_file patch_file in
    OpamProcess.Job.catch (fun e -> finalise (); raise e) @@ fun () ->
    let mark = mark_from_url repo_url in
    hg repo_root ~stdout:patch_file [ "diff"; "--text"; "--subrepos"; "--reverse";
        "--rev"; mark ] @@> fun r ->
    if OpamProcess.is_failure r then
      (finalise ();
       OpamSystem.internal_error "Hg error: '%s' not found." mark)
    else if OpamSystem.file_is_empty patch_file then
      (finalise (); Done None)
    else
      Done (Some (OpamFilename.of_string patch_file))

  let is_up_to_date ?subpath:_ repo_root repo_url =
    let mark = mark_from_url repo_url in
    hg repo_root [ "status"; "--subrepos"; "--rev"; mark ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done (r.OpamProcess.r_stdout = [])

  let versioned_files repo_root =
    hg repo_root [ "locate" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let vc_dir repo_root = OpamFilename.Op.(repo_root / ".hg")

  let current_branch repo_root =
    hg repo_root [ "identify"; "--bookmarks" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    match r.OpamProcess.r_stdout with
    | [] -> Done None
    | marks::_ ->
        let marks = OpamStd.String.split marks ' ' in
        let marks =
            List.filter (OpamStd.String.starts_with ~prefix:mark_prefix) marks
        in
        match marks with
        | mark::_ -> Done (Some mark)
        | [] ->
            hg repo_root [ "identify"; "--branch" ] @@> fun r ->
            OpamSystem.raise_on_process_error r;
            match r.OpamProcess.r_stdout with
            | branch::_ when branch <> "default" -> Done (Some branch)
            | _ -> Done None

  let is_dirty ?subpath:_ repo_root =
    hg repo_root [ "status"; "--subrepos" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done (r.OpamProcess.r_stdout = [])

  let modified_files repo_root =
    hg repo_root [ "status"; "--subrepos" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    let files =
      OpamStd.List.filter_map (fun line ->
          match OpamStd.String.split line ' ' with
          | ("A" | "M")::file::[] -> Some file
          | _ -> None) r.OpamProcess.r_stdout
    in
    Done files

  let get_remote_url ?hash repo_root =
    hg repo_root [ "paths"; "default" ]
    @@> function
    | { OpamProcess.r_code = 0; _ } as r ->
      (match r.r_stdout with
       | [url] ->
         (let url = OpamUrl.parse ~backend:`hg url in
       if OpamUrl.local_dir url <> None then Done None else
          let check_remote hash =
            hg repo_root [ "id"; "-r"; hash; "default" ]
            @@> fun r ->
            if OpamProcess.is_success r then
              Done (Some { url with hash = Some hash })
              (* default branch of hg is default *)
            else Done (Some { url with hash = None})
          in
          match hash with
          | None ->
            (hg repo_root ["branch"] @@> function
              | { OpamProcess.r_code = 0; OpamProcess.r_stdout = [hash]; _ } ->
                check_remote hash
              | { OpamProcess.r_code = 0; _ }
              | { OpamProcess.r_code = 1; _ } -> Done (Some url)
              | r -> OpamSystem.process_error r)
          | Some hash -> check_remote hash)
       | _ -> Done None)
    | { OpamProcess.r_code = 1; _ } -> Done None
    | r -> OpamSystem.process_error r

end

module B = OpamVCS.Make(VCS)

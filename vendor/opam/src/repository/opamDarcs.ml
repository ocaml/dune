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

  let name = `darcs

  let exists repo_root =
    OpamFilename.exists_dir (repo_root / "_darcs")

  let darcs repo_root =
    let dir = OpamFilename.Dir.to_string repo_root in
    fun ?verbose ?env ?stdout args ->
      OpamSystem.make_command ~dir ?verbose ?env ?stdout "darcs" args

  let with_tag repo_url = match repo_url.OpamUrl.hash with
    | None -> fun cmd -> cmd
    | Some t -> fun cmd -> cmd @ [ "-t"; t ]

  let init repo_root _repo_url =
    OpamFilename.mkdir repo_root;
    darcs repo_root [ "init" ]
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let vc_dir repo_root = repo_root / "_darcs"

  (* Darcs has no branches, no proper diff, no way to reset, can't return a
     workdir diff including added/removed files... That makes it hard for
     handling as a remote, and the following is a bit convoluted. *)

  let opam_remote_tag = "opam-remote-tag"
  (* Marks the last fetched state *)
  let opam_reverse_commit = "opam-revert-laststate"
  let opam_local_tag = "opam-local-tag"
  (* Marks the current state, in the form of a reversing patch on top of the
     fetched state *)

  let fetch ?cache_dir:_ ?subpath:_ repo_root repo_url =
    (* Just do a fresh pull into a temp directory, and replace _darcs/
       There is no easy way to diff or make sure darcs forgets about local
       patches otherwise. *)
    let repo_str =
      match OpamUrl.local_dir repo_url with
      | Some path -> OpamFilename.Dir.to_string path
      | None -> OpamUrl.base_url repo_url
    in
    OpamFilename.with_tmp_dir_job @@ fun d ->
    let repodir = d / "repo" in
    darcs repo_root
      (with_tag repo_url
         [ "get"; repo_str;
           "--repodir"; OpamFilename.Dir.to_string repodir;
           "--quiet"; "--lazy" ])
      (* --no-working-dir would be fine, except it is stored in _darcs/format *)
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    let darcsdir = vc_dir repo_root in
    OpamFilename.rmdir darcsdir;
    OpamFilename.move_dir ~src:(vc_dir repodir) ~dst:darcsdir;
    (* We put the patch that reverts to the current state on top *)
    darcs repo_root [ "tag"; opam_remote_tag; "-A"; "opam" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    darcs repo_root
      [ "record"; "-l"; "--boring"; "--all";
        "-m"; opam_reverse_commit; "-A"; "opam" ] @@> fun _r ->
    (* May fail if patch empty, it's ok, we keep the two tags for comparison *)
    darcs repo_root [ "tag"; opam_local_tag; "-A"; "opam" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let clean repo_root =
    darcs repo_root [ "obliterate"; "--all"; "-t"; opam_local_tag ]
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let reset_tree repo_root _repo_url =
    clean repo_root @@+ fun () ->
    darcs repo_root [ "obliterate"; "--all"; "-p"; opam_reverse_commit ]
    @@> fun r ->
    (* returns 0 even if patch doesn't exist *)
    OpamSystem.raise_on_process_error r;
    Done ()

  let patch_applied _ _ = Done ()

  let revision repo_root =
    (* 'Weak hash' is only supported from 2.10.3, so provide a fallback *)
    darcs repo_root [ "show"; "repo" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    try
      OpamStd.List.find_map
        (fun s ->
           match OpamStd.String.rcut_at s ' ' with
           | Some (label, value)
             when OpamStd.String.contains ~sub:"Weak Hash" label ->
             Some (Done (Some value))
           | _ -> None)
        r.OpamProcess.r_stdout
    with Not_found ->
    try
      OpamStd.List.find_map
        (fun s ->
           match OpamStd.String.rcut_at s ' ' with
           | Some (label, value)
             when OpamStd.String.contains ~sub:"Num Patches" label ->
             Some (Done (Some (Printf.sprintf "darcs-%s" value)))
           | _ -> None)
        r.OpamProcess.r_stdout
    with Not_found ->
      Done None

  let is_up_to_date ?subpath:_ repo_root _repo_url =
    darcs repo_root [ "log"; "-p"; opam_reverse_commit; "--last"; "2" ]
    (* last 2 since the tag counts as one *)
    @@> function
    | { OpamProcess.r_code = 0; _ } -> Done false
    | { OpamProcess.r_code = 1; _ } as r->
      OpamProcess.cleanup ~force:true r;
      Done true
    | r -> OpamSystem.process_error r

  let diff repo_root repo_url =
    is_up_to_date repo_root repo_url @@+ function
    | true -> Done None
    | false ->
      let patch_file = OpamSystem.temp_file ~auto_clean: false "darcs-diff" in
      let finalise () = OpamSystem.remove_file patch_file in
      OpamProcess.Job.catch (fun e -> finalise (); raise e) @@ fun () ->
      darcs repo_root ~stdout:patch_file
        [ "diff";
          "--from-tag"; opam_remote_tag; "--to-tag"; opam_local_tag;
          "--diff-command"; "diff -ruNa %2 %1"; "--no-pause-for-gui"; ]
      (* changing 'from' and 'to' doesn't work, so run a reverse diff command
         instead*)
      @@> fun r ->
      OpamSystem.raise_on_process_error r;
      if OpamSystem.file_is_empty patch_file then
        (finalise (); Done None)
      else
        Done (Some (OpamFilename.of_string patch_file))

  let versioned_files repo_root =
    darcs repo_root [ "show" ; "files" ]
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done r.OpamProcess.r_stdout

  let current_branch _dir = Done None (* No branches in Darcs *)

  let is_dirty ?subpath:_ dir =
    darcs dir [ "whatsnew"; "--quiet"; "--summary" ]
    @@> fun r -> Done (OpamProcess.check_success_and_cleanup r)

  let modified_files repo_root =
    darcs repo_root [ "whatsnew"; "--summary" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    let files =
      OpamStd.List.filter_map (fun line ->
          match OpamStd.String.split line ' ' with
          | ("A" | "M")::file::[]
          | _::"->"::file::[] -> Some file
          | _ -> None) r.OpamProcess.r_stdout
    in
    Done (List.sort_uniq compare files)

  let get_remote_url ?hash:_ repo_root =
    darcs repo_root [ "show"; "repo" ]
    @@> function
    | { OpamProcess.r_code = 0; _ } as r ->
      let res =
        (let valid c e =
           match OpamStd.String.cut_at (OpamStd.String.strip e) ':' with
           | Some (p,rhs) when p = c -> Some rhs
           | _ -> None
         in
         match OpamStd.List.filter_map (valid "Cache") r.r_stdout with
         | [line] ->
           (let repo =
              OpamStd.List.filter_map (valid "repo")
              (OpamStd.String.split line ',')
            in
            match repo with
            | [repo] -> Some repo
            | _ -> None)
         | _ -> None)
      in
      Done (OpamStd.Option.map (fun u ->
          OpamUrl.parse ~backend:`darcs u) res)
    | { OpamProcess.r_code = 1; _ } -> Done None
    | r -> OpamSystem.process_error r

end

module B = OpamVCS.Make(VCS)

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

(* let log fmt = OpamConsole.log "GIT" fmt *)

module VCS : OpamVCS.VCS = struct

  let name = `git

  let exists repo_root =
    OpamFilename.exists_dir (repo_root / ".git") ||
    OpamFilename.exists (repo_root // ".git")

  let cygpath = OpamSystem.get_cygpath_function ~command:"git"

  let git repo_root =
    let dir = OpamFilename.Dir.to_string repo_root in
    (* If the ?env arg is restored here, then the caching for the Cygwin-ness
       of git will need to change, as altering PATH could select a different
       Git *)
    fun ?verbose ?stdout args ->
      OpamSystem.make_command ~dir ?verbose ?stdout "git" args

  let init repo_root repo_url =
    OpamFilename.mkdir repo_root;
    OpamProcess.Job.of_list [
      git repo_root [ "init" ];
      (* Enforce this option, it can break our use of git if set *)
      git repo_root [ "config" ; "--local" ; "fetch.prune"; "false"];
      (* We reset diff.noprefix to ensure we get a `-p1` patch and avoid <https://github.com/ocaml/opam/issues/3627>. *)
      git repo_root [ "config" ; "--local" ; "diff.noprefix"; "false"];
      (* Disable automatic line-ending conversion and switch core.eol to Unix.
         THIS DOES NOT MEAN ALL FILES GET LF-ONLY LINE-ENDINGS!
         This combination of settings means that files will be checked out
         exactly as they appear in the repository, so if files are checked in
         with CRLF line-endings (either by not having .gitattributes with
         core.autocrlf = false, or having an explicit eol=crlf in
         .gitattributes), then they will still be checked out with CRLF endings.
       *)
      git repo_root [ "config" ; "--local" ; "core.autocrlf"; "false"];
      git repo_root [ "config" ; "--local" ; "core.eol"; "lf"];
      git repo_root [ "config" ; "--local" ; "color.ui"; "false" ];
      (* Document the remote for user-friendliness (we don't use it) *)
      git repo_root [ "remote"; "add"; "origin"; OpamUrl.base_url repo_url ];
    ] @@+ function
    | None -> Done ()
    | Some (_,err) -> OpamSystem.process_error err

  let remote_ref url =
    match url.OpamUrl.hash with
    | Some h -> "refs/remotes/opam-ref-"^h
    | None -> "refs/remotes/opam-ref"

  let fetch ?cache_dir ?subpath repo_root repo_url =
    (match subpath with
     | Some sp ->
       git repo_root [ "config"; "--local"; "core.sparseCheckout"; "true" ]
       @@> fun r -> OpamSystem.raise_on_process_error r;
       OpamFilename.write (repo_root / ".git" / "info" // "sparse-checkout")
         (OpamFilename.SubPath.normalised_string sp);
       Done()
     | None -> Done())
    @@+ fun _ ->
    (match cache_dir with
     | Some c when OpamUrl.local_dir repo_url = None ->
       let dir = c / "git" in
       if not (OpamFilename.exists_dir dir) then
         (OpamFilename.mkdir dir;
          git dir [ "init"; "--bare" ] @@> fun r ->
          OpamSystem.raise_on_process_error r;
          Done (Some dir))
       else Done (Some dir)
     | _ -> Done None)
    @@+ fun global_cache ->
    let repo_url = OpamUrl.map_file_url (Lazy.force cygpath) repo_url in
    let origin = OpamUrl.base_url repo_url in
    let branch = OpamStd.Option.default "HEAD" repo_url.OpamUrl.hash in
    let opam_ref = remote_ref repo_url in
    let refspec = Printf.sprintf "+%s:%s" branch opam_ref in
    git repo_root [ "remote" ; "set-url"; "origin"; origin ] @@> fun _ ->
    OpamStd.Option.iter (fun cache ->
        let alternates = repo_root / ".git" / "objects" / "info" // "alternates" in
        if not (OpamFilename.exists alternates) then
          OpamFilename.write alternates
            (OpamFilename.Dir.to_string (cache / "objects")))
      global_cache;
    git repo_root [ "fetch" ; "-q"; origin; "--update-shallow"; "--depth=1"; refspec ]
    @@> fun r ->
    if OpamProcess.check_success_and_cleanup r then
      let refspec =
        Printf.sprintf "+%s:refs/remotes/%s" opam_ref
          (Digest.to_hex (Digest.string (OpamUrl.to_string repo_url)))
      in
      match global_cache with
      | Some cache ->
        git repo_root [ "push" ; OpamFilename.Dir.to_string cache ;
                        refspec ]
        @@> fun _ -> Done ()
      | None -> Done ()
    else
      (* fallback to fetching all first (workaround, git 2.1 fails silently
         on 'fetch HASH' when HASH isn't available locally already).
         Also, remove the [--update-shallow] option in case git is so old that
         it didn't exist yet, as that is not needed in the general case *)
      git repo_root [ "fetch" ; "-q" ] @@> fun r ->
      OpamSystem.raise_on_process_error r;
      (* retry to fetch the specific branch *)
      git repo_root [ "fetch" ; "-q"; origin; refspec ] @@> fun r ->
      if OpamProcess.check_success_and_cleanup r then Done ()
      else if
        OpamStd.String.fold_left (fun acc c -> match acc, c with
            | true, ('0'..'9' | 'a'..'f' | 'A'..'F') -> true
            | _ -> false)
          true branch
      then
        (* the above might still fail on raw, untracked hashes: try to bind to
           the direct refspec, if found *)
        (git repo_root [ "update-ref" ; opam_ref; branch ] @@> fun r ->
         if OpamProcess.check_success_and_cleanup r then
           Done()
         else
           (* check if the commit exists *)
           (git repo_root [ "fetch"; "-q" ] @@> fun r ->
            OpamSystem.raise_on_process_error r;
            git repo_root [ "show"; "-s"; "--format=%H"; branch ] @@> fun r ->
            if OpamProcess.check_success_and_cleanup r then
              failwith "Commit found, but unreachable: enable uploadpack.allowReachableSHA1InWant on server"
            else failwith "Commit not found on repository"))
      else
      let error = r in
      git repo_root ["branch"; "-a"] @@> function
      | { OpamProcess.r_code = 0; OpamProcess.r_stdout = []; _ } ->
        failwith "Git repository seems just initialized, \
                  try again after your first commit"
      | { OpamProcess.r_code = 0; OpamProcess.r_stdout = _::_; _ } ->
        failwith (Printf.sprintf "Branch %s not found" branch)
      | _ -> OpamSystem.process_error error

  let revision repo_root =
    git repo_root ~verbose:false [ "rev-parse"; "HEAD" ] @@>
    fun r ->
    if r.OpamProcess.r_code = 128 then
      (OpamProcess.cleanup ~force:true r; Done None)
    else
      (OpamSystem.raise_on_process_error r;
       match r.OpamProcess.r_stdout with
       | []      -> Done None
       | full::_ ->
         Done (Some full))

  let clean repo_root =
    git repo_root [ "clean"; "-fdx" ]
    @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done ()

  let reset_tree repo_root repo_url =
    let rref = remote_ref repo_url in
    git repo_root [ "reset" ; "--hard"; rref; "--" ]
    @@> fun r ->
    if OpamProcess.is_failure r then
      OpamSystem.internal_error "Git error: %s not found." rref
    else
      clean repo_root @@+ fun () ->
      if OpamFilename.exists (repo_root // ".gitmodules") then
        git repo_root [ "submodule"; "update"; "--init"; "--recursive" ]
        @@> fun r ->
        if OpamProcess.is_failure r then
          OpamConsole.warning "Git submodule update failed in %s"
            (OpamFilename.Dir.to_string repo_root);
        Done ()
      else Done ()

  let patch_applied _ _ =
    (* This might be a good place to do 'git reset --soft' and check for
       unstaged changes. See <https://github.com/ocaml/opam/pull/3283>. *)
    Done ()

  let diff repo_root repo_url =
    let rref = remote_ref repo_url in
    let patch_file = OpamSystem.temp_file ~auto_clean: false "git-diff" in
    let finalise () = OpamSystem.remove_file patch_file in
    OpamProcess.Job.catch (fun e -> finalise (); raise e) @@ fun () ->
    git repo_root [ "add"; "." ] @@> fun r ->
    (* Git diff is to the working dir, but doesn't work properly for
       unregistered directories. *)
    OpamSystem.raise_on_process_error r;
    (* We also reset diff.noprefix here to handle already existing repo. *)
    git repo_root ~stdout:patch_file [ "-c" ; "diff.noprefix=false" ; "diff" ; "--text" ; "--no-ext-diff" ; "-R" ; "-p" ; rref; "--" ]
    @@> fun r ->
    if not (OpamProcess.check_success_and_cleanup r) then
      (finalise ();
       OpamSystem.internal_error "Git error: %s not found." rref)
    else if OpamSystem.file_is_empty patch_file then
      (finalise (); Done None)
    else
      Done (Some (OpamFilename.of_string patch_file))

  let is_up_to_date ?subpath repo_root repo_url =
    let rref = remote_ref repo_url in
    git repo_root ([ "diff" ; "--no-ext-diff" ; "--quiet" ; rref; "--" ]
                   @ List.map OpamFilename.SubPath.to_string
                     (OpamStd.Option.to_list subpath))
    @@> function
    | { OpamProcess.r_code = 0; _ } -> Done true
    | { OpamProcess.r_code = 1; _ } as r ->
      OpamProcess.cleanup ~force:true r; Done false
    | r -> OpamSystem.process_error r

  let versioned_files repo_root =
    git repo_root ~verbose:false [ "ls-files" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    Done (List.map OpamSystem.forward_to_back r.OpamProcess.r_stdout)

  let vc_dir repo_root = OpamFilename.Op.(repo_root / ".git")

  let current_branch dir =
    git dir [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ]
    @@> function
    | { OpamProcess.r_code = 0; OpamProcess.r_stdout = [s]; _ } ->
      Done (Some s)
    | _ ->
      Done (Some "HEAD")

  let is_dirty ?subpath dir =
    let subpath =
      match subpath with
      | None -> []
      | Some dir -> ["--" ; OpamFilename.SubPath.to_string dir]
      in
    git dir ([ "diff"; "--no-ext-diff"; "--quiet" ; "HEAD" ] @ subpath)
    @@> function
    | { OpamProcess.r_code = 0; _ } ->
      (git dir ["ls-files"; "--others"; "--exclude-standard"]
       @@> function
       | { OpamProcess.r_code = 0; OpamProcess.r_stdout = []; _ } ->
         Done false
       | { OpamProcess.r_code = 0; _ }
       | { OpamProcess.r_code = 1; _ } as r ->
         OpamProcess.cleanup ~force:true r; Done true
       | r -> OpamSystem.process_error r
      )
    | { OpamProcess.r_code = 1; _ } as r ->
      OpamProcess.cleanup ~force:true r; Done true
    | r -> OpamSystem.process_error r

  let modified_files repo_root =
    git repo_root ~verbose:false [ "status" ; "--short" ] @@> fun r ->
    OpamSystem.raise_on_process_error r;
    let files =
      OpamStd.List.filter_map (fun line ->
          match OpamStd.String.split line ' ' with
          | ("A" | "M" | "AM")::file::[]
          | ("R"|"RM"|"C"|"CM")::_::"->"::file::[] -> Some file
          | _ -> None) r.OpamProcess.r_stdout
    in
    Done files

  let origin = "origin"

  (** check if a hash or branch is present in remote origin and returns  *)
  let check_remote repo_root hash_or_b =
    (* get the hash of the branch *)
    let hash =
      git repo_root ["branch"] @@> fun r ->
      if OpamProcess.is_success r then
        let is_branch =
          List.exists (OpamStd.String.contains ~sub:hash_or_b) r.r_stdout
        in
        if is_branch then
          git repo_root [ "rev-list"; hash_or_b; "-1" ] @@> fun r ->
          if OpamProcess.is_success r then
            (match List.filter OpamStd.String.is_hex r.r_stdout with
             | [hash] ->
               Done (Some hash)
             | _ -> Done None)
          else Done None
        else
        if OpamStd.String.is_hex hash_or_b then
          Done (Some hash_or_b)
        else Done None
      else
        Done None
    in
    hash @@+ function
    | Some hash ->
      (* check if hash / branch is present in remote *)
      (git repo_root ["branch"; "-r"; "--contains"; hash]
       @@> function
       | { OpamProcess.r_code = 0; _ } as r ->
         if r.r_stdout <> [] &&
            (List.exists (OpamStd.String.contains ~sub:origin) r.r_stdout) then
           Done (Some hash_or_b)
         else
           Done None
       | { OpamProcess.r_code = 1; _ } ->
         Done None
       | r -> OpamSystem.process_error r)
    | None -> Done None

  let get_remote_url ?hash repo_root =
    git repo_root ["remote"; "get-url"; origin]
    @@> function
    | { OpamProcess.r_code = 0; OpamProcess.r_stdout = [url]; _ } ->
      (let u = OpamUrl.parse ~backend:`git url in
       if OpamUrl.local_dir u <> None then Done None else
       let hash_in_remote =
         match hash with
         | None ->
           (current_branch repo_root @@+ function
             | None | Some "HEAD" -> Done None
             | Some hash -> check_remote repo_root hash)
         | Some hash -> check_remote repo_root hash
       in
       hash_in_remote @@+ function
       | Some _ as hash ->
         Done (Some { u with OpamUrl.hash = hash })
       | None ->
         Done (Some { u with OpamUrl.hash = None })
      )
    | { OpamProcess.r_code = 0; _ }
    | { OpamProcess.r_code = 1; _ } -> Done None
    | r -> OpamSystem.process_error r

end

module B = OpamVCS.Make(VCS)

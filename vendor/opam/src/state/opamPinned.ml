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
open OpamStateTypes
open OpamFilename.Op
open OpamStd.Option.Op

let log fmt = OpamConsole.log "PIN" fmt

let package st name = OpamPackage.package_of_name st.pinned name

let package_opt st name = try Some (package st name) with Not_found -> None

let version st name = (package st name).version

let version_opt st name = try Some (version st name) with Not_found -> None

let packages st = st.pinned

let possible_definition_filenames dir name = [
  dir / (OpamPackage.Name.to_string name ^ ".opam") // "opam";
  dir // (OpamPackage.Name.to_string name ^ ".opam");
  dir / "opam" / (OpamPackage.Name.to_string name ^ ".opam") // "opam";
  dir / "opam" // (OpamPackage.Name.to_string name ^ ".opam");
  dir / "opam" // "opam";
  dir // "opam"
]

let lock_filename ?locked file =
  locked
  >>+ (fun () ->  OpamStateConfig.(!r.locked))
  >>| (fun e -> OpamFilename.add_extension file e, e)

let check_locked ?locked default =
  match lock_filename ?locked default with
  | None -> default, None
  | Some (locked, ext) ->
    if not (OpamFilename.exists locked) then default, None else
      (log "Lock file found %s" (OpamFilename.to_string default);
       let base_depends =
         OpamFile.make default
         |> OpamFile.OPAM.read
         |> OpamFile.OPAM.depends
       in
       let lock_depends =
         OpamFile.make locked
         |> OpamFile.OPAM.read
         |> OpamFile.OPAM.depends
       in
       let ldep_names =
         OpamFormula.fold_left
           (fun acc (n,_) -> OpamPackage.Name.Set.add n acc)
           OpamPackage.Name.Set.empty lock_depends
       in
       let base_formula =
         OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false
           ~dev_setup:false ~dev:false base_depends
       in
       let lock_formula =
         OpamFilter.filter_deps ~build:true ~post:true ~test:false ~doc:false
           ~dev_setup:false ~dev:false lock_depends
       in
       let lpkg_f =
         lock_formula
         |> OpamFormula.atoms
         |> OpamPackage.Name.Map.of_list
       in
       (* Check consistency between them. It is based on the fact that locked file
          dependencies are an and list with precise version, i.e., pkg { =v0.1}.
          Construction of a two list: missing dependencies and inconsistent ones
          (version mismatch) *)
       let (@) = List.rev_append in
       let rec fold formula =
         List.fold_left cross ([],[]) (OpamFormula.ands_to_list formula)
       and cross (cont,cons) formula =
         match formula with
         | Atom (bn, bvf) ->
           ( let cont =
               if OpamPackage.Name.Set.mem bn ldep_names then cont
               else bn::cont
             in
             let cons =
               match OpamPackage.Name.Map.find_opt bn lpkg_f with
               | Some (Some (`Eq, lv)) ->
                 if OpamFormula.check_version_formula bvf lv then cons
                 else (bn, lv, bvf)::cons
               | _ -> cons
             in
             (cont,cons))
         | Or (or1, or2) ->
           let or1_cont, or1_cons = fold or1 in
           let or2_cont, or2_cons = fold or2 in
           let cont =
             if or1_cont = [] || or2_cont = [] then cont
             else or1_cont @ or2_cont @ cont
           in
           let cons =
             if or1_cons = [] || or2_cons = [] then cons
             else or1_cons @ or2_cons @ cons
           in
           (cont,cons)
         | And (and1, and2) ->
           let and1_cont, and1_cons = fold and1 in
           let and2_cont, and2_cons = fold and2 in
           ((and1_cont @ and2_cont @ cont), (and1_cons @ and2_cons @ cons))
         | Block f -> cross (cont,cons) f
         | Empty -> (cont,cons)
       in
       let contains, consistent = fold base_formula in
       if contains <> [] || consistent <> [] then
         (OpamConsole.warning "Lock file %s is outdated, you may want to re-run opam lock:\n%s"
            (OpamConsole.colorise `underline (OpamFilename.Base.to_string (OpamFilename.basename locked)))
            ((if contains <> [] then
                Printf.sprintf "Dependencies present in opam file not in lock file:\n%s"
                  (OpamStd.Format.itemize OpamPackage.Name.to_string contains)
              else "")
             ^
             (if consistent <> [] then
                Printf.sprintf "Dependencies in lock file not consistent wit opam file filter:\n%s"
                  (OpamStd.Format.itemize (fun (n,lv,(bv: OpamFormula.version_formula)) ->
                       Printf.sprintf "%s: %s in not contained in {%s}"
                         (OpamPackage.Name.to_string n)
                         (OpamPackage.Version.to_string lv)
                         (OpamFormula.string_of_formula
                            (fun (op, vc) ->
                               Printf.sprintf "%s %s"
                                 (OpamPrinter.FullPos.relop_kind op) (OpamPackage.Version.to_string vc))
                            bv))
                      consistent)
              else "")));
       locked, Some ext)

let find_opam_file_in_source ?locked name dir =
  let opt =
    OpamStd.List.find_opt OpamFilename.exists
      (possible_definition_filenames dir name)
  in
  opt
  >>| check_locked ?locked
  >>| (fun (o,l) -> OpamFile.make o, l)

let name_of_opam_filename ?locked dir file =
  let suffix= ".opam" in
  let get_name s =
    if Filename.check_suffix s suffix
    then Some Filename.(chop_suffix (basename s) suffix)
    else None
  in
  let rel = OpamFilename.remove_prefix dir file in
  let rel =
    match locked with
    | None -> rel
    | Some suf ->
      let ext = "."^suf in
      if OpamStd.String.ends_with ~suffix:(suffix^ext) rel then
        OpamStd.String.remove_suffix ~suffix:ext rel
      else rel
  in
  (get_name (Filename.basename rel) >>+ fun () ->
   get_name (Filename.dirname rel)) >>= fun name ->
  try Some (OpamPackage.Name.of_string name)
  with Failure _ -> None

let files_in_source ?locked ?(recurse=false) ?subpath d =
  let baseopam = OpamFilename.Base.of_string "opam" in
  let files =
    let rec files_aux acc base d =
      let acc =
        OpamStd.List.filter_map (fun f ->
            if OpamFilename.basename f = baseopam ||
               OpamFilename.check_suffix f ".opam" then
              let base =
                match base, subpath with
                | Some b, Some sp ->
                  Some (Filename.concat (OpamFilename.SubPath.to_string sp) b)
                | Some b, _ -> Some b
                | _, Some sp -> Some (OpamFilename.SubPath.to_string sp)
                | None, None -> None
              in
              Some (f, base)
            else
              None)
          (OpamFilename.files d) @ acc
      in
      List.fold_left
        (fun acc d ->
           if OpamFilename.(basename_dir d = Base.of_string "opam") ||
              OpamStd.String.ends_with ~suffix:".opam"
                (OpamFilename.Dir.to_string d)
           then
             match OpamFilename.opt_file OpamFilename.Op.(d//"opam") with
             | None -> acc
             | Some f -> (f, base) :: acc
           else
           let base_dir = OpamFilename.basename_dir d in
           let basename = OpamFilename.Base.to_string base_dir in
           if recurse &&
              not (base_dir = OpamFilename.Base.of_string OpamSwitch.external_dirname ||
                   base_dir = OpamFilename.Base.of_string "_build" ||
                   OpamStd.String.starts_with ~prefix:"." basename)
           then
             let base = match base with
               | None -> Some basename
               | Some base -> Some (Filename.concat base basename) in
             files_aux acc base d
           else
             acc)
        acc (OpamFilename.dirs d)
    in
    files_aux [] None
  in
  let d = OpamFilename.SubPath.(d /? subpath) in
  files d @ files (d / "opam") |>
  List.map (fun (f,s) -> (check_locked ?locked f), s) |>
  OpamStd.List.filter_map
    (fun ((f, locked), subpath) ->
       try
         (* Ignore empty files *)
         if (Unix.stat (OpamFilename.to_string f)).Unix.st_size = 0 then None
         else
           Some { pin_name = name_of_opam_filename ?locked d f;
                  pin = {
                    pin_file = OpamFile.make f;
                    pin_locked = locked;
                    pin_subpath =
                      OpamStd.Option.map OpamFilename.SubPath.of_string subpath;
                    pin_url = ();
                  }}
       with Unix.Unix_error _ ->
         OpamConsole.error "Can not read %s, ignored."
           (OpamFilename.to_string f);
         None)

let files_in_source_w_target ?locked ?recurse ?subpath
    ?(same_kind=fun _ -> true) url dir =
  OpamStd.List.filter_map (fun name_and_file ->
      let pin_url =
        match url.OpamUrl.backend with
        | #OpamUrl.version_control as vc ->
          let module VCS =
            (val match vc with
               | `git -> (module OpamGit.VCS: OpamVCS.VCS)
               | `hg -> (module OpamHg.VCS: OpamVCS.VCS)
               | `darcs -> (module OpamDarcs.VCS: OpamVCS.VCS)
               : OpamVCS.VCS)
          in
          let open OpamProcess.Job.Op in
          let versioned_files =
            OpamProcess.Job.run @@
            VCS.versioned_files dir @@| fun files -> files
          in
          let opamfile =
            OpamFilename.remove_prefix dir
              (OpamFile.filename name_and_file.pin.pin_file)
          in
          if List.mem opamfile versioned_files
          || not (OpamStd.String.contains opamfile ~sub:Filename.dir_sep) then
            url
          else
            { url with
              transport = "file";
              hash = None;
              backend = `rsync }
        | _ -> url
      in
      if same_kind pin_url then
        Some { name_and_file with pin = { name_and_file.pin with pin_url }}
      else None)
    (files_in_source ?locked ?recurse ?subpath dir)

let orig_opam_file st name opam =
  OpamFile.OPAM.get_metadata_dir
    ~repos_roots:(OpamRepositoryState.get_root st.switch_repos)
    opam >>= fun dir ->
  let opam_files = [
    dir // (OpamPackage.Name.to_string name ^ ".opam");
    dir // "opam"
  ] in
  let locked_files =
    match OpamFile.OPAM.locked opam with
    | Some locked ->
      List.map (fun f -> OpamFilename.add_extension f locked) opam_files
    | None -> []
  in
  OpamStd.List.find_opt OpamFilename.exists locked_files
  ++ OpamStd.List.find_opt OpamFilename.exists opam_files
  >>| OpamFile.make

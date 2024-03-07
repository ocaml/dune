open Import

type extra_files =
  | Inside_files_dir of Path.t option
  | Git_files of Path.Local.t option * Rev_store.At_rev.t

type nonrec t =
  { opam_file : OpamFile.OPAM.t
  ; package : OpamPackage.t
  ; extra_files : extra_files
  ; loc : Loc.t
  ; dune_build : bool
  }

let dune_build t = t.dune_build
let loc t = t.loc
let package t = t.package
let opam_file t = t.opam_file

let set_url t url =
  let opam_file = OpamFile.OPAM.with_url (OpamFile.URL.create url) t.opam_file in
  { t with opam_file }
;;

let add_opam_package_to_opam_file package opam_file =
  opam_file
  |> OpamFile.OPAM.with_version (OpamPackage.version package)
  |> OpamFile.OPAM.with_name (OpamPackage.name package)
;;

let read_opam_file package ~opam_file_path ~opam_file_contents =
  Opam_file.read_from_string_exn ~contents:opam_file_contents opam_file_path
  |> add_opam_package_to_opam_file package
;;

let git_repo package ~opam_file ~opam_file_contents rev ~files_dir =
  let opam_file_path = Path.of_local opam_file in
  let opam_file = read_opam_file package ~opam_file_path ~opam_file_contents in
  let loc = Loc.in_file opam_file_path in
  { dune_build = false
  ; loc
  ; package
  ; opam_file
  ; extra_files = Git_files (files_dir, rev)
  }
;;

let local_fs package ~dir ~opam_file_path ~files_dir =
  let opam_file_path = Path.append_local dir opam_file_path in
  let files_dir = Option.map files_dir ~f:(Path.append_local dir) in
  let opam_file =
    let opam_file_contents = Io.read_file ~binary:true opam_file_path in
    read_opam_file package ~opam_file_path ~opam_file_contents
  in
  let loc = Loc.in_file opam_file_path in
  { dune_build = false
  ; loc
  ; package
  ; extra_files = Inside_files_dir files_dir
  ; opam_file
  }
;;

(* Scan a path recursively down retrieving a list of all files together with their
   relative path. *)
let scan_files_entries path =
  match Path.stat path with
  | Error (Unix.ENOENT, _, _) -> []
  | Error e -> Unix_error.Detailed.raise e
  | _ ->
    (try
       Fpath.traverse_files
         ~dir:(Path.to_string path)
         ~init:[]
         ~f:(fun ~dir filename acc ->
           let local_path = Path.Local.relative (Path.Local.of_string dir) filename in
           local_path :: acc)
     with
     | Unix.Unix_error (err, a, e) ->
       User_error.raise
         ~loc:(Loc.in_file path)
         [ Pp.text "Unable to read file in opam repository:"
         ; Unix_error.Detailed.pp (err, a, e)
         ])
;;

let dune_package loc opam_file opam_package =
  let opam_file = add_opam_package_to_opam_file opam_package opam_file in
  let package = OpamFile.OPAM.package opam_file in
  { dune_build = true; opam_file; package; loc; extra_files = Inside_files_dir None }
;;

open Fiber.O

let get_opam_package_files resolved_packages =
  let indexed = List.mapi resolved_packages ~f:(fun i w -> i, w) |> Int.Map.of_list_exn in
  let from_dirs, from_git =
    Int.Map.partition_map indexed ~f:(fun (resolved_package : t) ->
      match resolved_package.extra_files with
      | Git_files (files_dir, rev) -> Right (files_dir, rev)
      | Inside_files_dir dir -> Left dir)
  in
  let+ from_git =
    if Int.Map.is_empty from_git
    then Fiber.return []
    else (
      let files_with_idx =
        Int.Map.to_list from_git
        |> List.concat_map ~f:(fun (idx, (files_dir, rev)) ->
          match files_dir with
          | None -> []
          | Some files_dir ->
            Rev_store.At_rev.directory_entries rev ~recursive:true files_dir
            |> Rev_store.File.Set.to_list
            |> List.map ~f:(fun file -> idx, files_dir, file))
      in
      let* rev_store = Rev_store.get in
      List.map files_with_idx ~f:(fun (_, _, file) -> file)
      |> Rev_store.content_of_files rev_store
      >>| List.map2 files_with_idx ~f:(fun (idx, files_dir, file) content ->
        let entry =
          let local_file =
            Rev_store.File.path file
            |> Path.Local.descendant ~of_:files_dir
            |> Option.value_exn
          in
          { File_entry.local_file; original = Content content }
        in
        idx, entry))
  in
  let from_dirs =
    Int.Map.to_list from_dirs
    |> List.concat_map ~f:(fun (idx, files_dir) ->
      match files_dir with
      | None -> []
      | Some files_dir ->
        scan_files_entries files_dir
        |> List.map ~f:(fun local_file ->
          ( idx
          , { File_entry.original = Path (Path.append_local files_dir local_file)
            ; local_file
            } )))
  in
  List.rev_append from_git from_dirs
  |> Int.Map.of_list_multi
  |> Int.Map.merge indexed ~f:(fun _ pkg files ->
    match pkg with
    | None -> assert false
    | Some _ -> Some (Option.value files ~default:[]))
  |> Int.Map.values
;;

open Import

(* CR-someday amokhov: Most of these records will have [dir = empty]. We might
   want to somehow optimise for the common case, e.g. by switching to a sum type
   with the [Files_only] constructor. It's best not to expose the current
   representation so we can easily change it in future. *)
type t =
  { files : Path.Build.Set.t
  ; dirs : Path.Build.Set.t
  }

module File = struct
  let create file = { files = Path.Build.Set.singleton file; dirs = Path.Build.Set.empty }
end

module Files = struct
  let create files = { files; dirs = Path.Build.Set.empty }
end

let create ~files ~dirs = { files; dirs }
let empty = { files = Path.Build.Set.empty; dirs = Path.Build.Set.empty }

let combine x y =
  { files = Path.Build.Set.union x.files y.files
  ; dirs = Path.Build.Set.union x.dirs y.dirs
  }
;;

let diff t { files; dirs } =
  { files = Path.Build.Set.diff t.files files; dirs = Path.Build.Set.diff t.dirs dirs }
;;

let is_empty { files; dirs } =
  Path.Build.Set.is_empty files && Path.Build.Set.is_empty dirs
;;

let head { files; dirs } =
  match Path.Build.Set.choose files with
  | Some _ as target -> target
  | None -> Path.Build.Set.choose dirs
;;

let to_dyn { files; dirs } =
  Dyn.Record [ "files", Path.Build.Set.to_dyn files; "dirs", Path.Build.Set.to_dyn dirs ]
;;

let all { files; dirs } = Path.Build.Set.to_list files @ Path.Build.Set.to_list dirs

let iter { files; dirs } ~file ~dir =
  Path.Build.Set.iter files ~f:file;
  Path.Build.Set.iter dirs ~f:dir
;;

module Validated = struct
  type unvalidated = t

  (* CR-soon amokhov: Represent these path sets more efficiently, e.g., by a map from the
     parent directory to the corresponding [Filename.Set.t] so that [target_names_in_dir]
     could be implemented without traversing the whole set. *)
  type nonrec t =
    { root : Path.Build.t
    ; files : Filename.Set.t
    ; dirs : Filename.Set.t
    }

  let iter { root; files; dirs } ~file ~dir =
    Filename.Set.iter files ~f:(fun fn -> file (Path.Build.relative root fn));
    Filename.Set.iter dirs ~f:(fun dn -> dir (Path.Build.relative root dn))
  ;;

  let fold { root; files; dirs } ~init ~file ~dir =
    let acc =
      Filename.Set.fold files ~init ~f:(fun fn -> file (Path.Build.relative root fn))
    in
    Filename.Set.fold dirs ~init:acc ~f:(fun dn -> dir (Path.Build.relative root dn))
  ;;

  let head { root; files; dirs } =
    let name =
      match Filename.Set.choose files with
      | Some name -> name
      | None ->
        (match Filename.Set.choose dirs with
         | Some name -> name
         | None -> assert false)
    in
    Path.Build.relative root name
  ;;

  let unvalidate t : unvalidated =
    { files =
        Path.Build.Set.of_listing ~dir:t.root ~filenames:(Filename.Set.to_list t.files)
    ; dirs =
        Path.Build.Set.of_listing ~dir:t.root ~filenames:(Filename.Set.to_list t.dirs)
    }
  ;;

  let to_dyn { root; files; dirs } =
    Dyn.Record
      [ "root", Path.Build.to_dyn root
      ; "files", Filename.Set.to_dyn files
      ; "dirs", Filename.Set.to_dyn dirs
      ]
  ;;
end

module Validation_result = struct
  type t =
    | Valid of Validated.t
    | No_targets
    | Inconsistent_parent_dir
    | File_and_directory_target_with_the_same_name of Path.Build.t
end

let validate { files; dirs } =
  let add_file (t : Validated.t) name =
    Validation_result.Valid { t with files = Filename.Set.add t.files name }
  in
  let add_dir (t : Validated.t) name =
    if Filename.Set.mem t.files name
    then
      Validation_result.File_and_directory_target_with_the_same_name
        (Path.Build.relative t.root name)
    else Valid { t with dirs = Filename.Set.add t.dirs name }
  in
  let build (init : Validation_result.t) ~paths ~f =
    Path.Build.Set.fold paths ~init ~f:(fun path res ->
      let parent = Path.Build.parent_exn path in
      let name = Path.Build.basename path in
      match res with
      | No_targets ->
        let t =
          { Validated.root = parent
          ; files = Filename.Set.empty
          ; dirs = Filename.Set.empty
          }
        in
        f t name
      | Valid t when Path.Build.equal t.root parent -> f t name
      | Valid _ -> Inconsistent_parent_dir
      | (Inconsistent_parent_dir | File_and_directory_target_with_the_same_name _) as res
        -> res)
  in
  build No_targets ~paths:files ~f:add_file |> build ~paths:dirs ~f:add_dir
;;

module Produced = struct
  (** All file and directory names are relative to the root (['a t]). *)
  type 'a dir_contents =
    { files : 'a Filename.Map.t (* mapping file name -> 'a *)
    ; subdirs : 'a dir_contents Filename.Map.t
      (* mapping directory name -> 'a dir_contents *)
    }

  let is_empty_dir_conts { files; subdirs } =
    Filename.Map.is_empty files && Filename.Map.is_empty subdirs
  ;;

  type 'a t =
    { root : Path.Build.t
    ; contents : 'a dir_contents
    }

  let equal
        { root = root1; contents = contents1 }
        { root = root2; contents = contents2 }
        ~equal
    =
    let rec eq_aux { files = files1; subdirs = dirs1 } { files = files2; subdirs = dirs2 }
      =
      Filename.Map.equal files1 files2 ~equal
      && Filename.Map.equal dirs1 dirs2 ~equal:eq_aux
    in
    Path.Build.equal root1 root2 && eq_aux contents1 contents2
  ;;

  module Error = struct
    type t =
      | Missing_dir of Path.Build.t
      | Empty_dir of Path.Build.t
      | Unreadable_dir of Path.Build.t * Unix_error.Detailed.t
      | Unsupported_file of Path.Build.t * File_kind.t

    let message = function
      | Missing_dir dir ->
        [ Pp.textf
            "Rule failed to produce directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
             |> Path.Source.to_string_maybe_quoted)
        ]
      | Empty_dir dir ->
        [ Pp.textf
            "Rule produced directory %S that contains no files nor non-empty \
             subdirectories"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
             |> Path.Source.to_string_maybe_quoted)
        ]
      | Unreadable_dir (dir, (unix_error, _, _)) ->
        [ Pp.textf
            "Rule produced unreadable directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
             |> Path.Source.to_string_maybe_quoted)
        ; Pp.verbatim (Unix.error_message unix_error)
        ]
      | Unsupported_file (file, kind) ->
        [ Pp.textf
            "Rule produced file %S with unrecognised kind %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn file
             |> Path.Source.to_string_maybe_quoted)
            (File_kind.to_string kind)
        ]
    ;;

    let to_string_hum = function
      | Missing_dir _ -> "missing directory"
      | Empty_dir _ -> "empty directory"
      | Unreadable_dir (_, unix_error) -> Unix_error.Detailed.to_string_hum unix_error
      | Unsupported_file _ -> "unsupported file kind"
    ;;
  end

  let empty = { files = Filename.Map.empty; subdirs = Filename.Map.empty }

  (** The call sites ensure that [dir = Path.Build.append_local validated.root local].
      No need for [local] actually... *)
  let rec contents_of_dir ~file_f (dir : Path.Build.t) : ('a dir_contents, Error.t) result
    =
    let open Result.O in
    let init = empty in
    match Path.readdir_unsorted_with_kinds (Path.build dir) with
    | Error (Unix.ENOENT, _, _) -> Error (Missing_dir dir)
    | Error e -> Error (Unreadable_dir (dir, e))
    | Ok dir_contents ->
      Result.List.fold_left dir_contents ~init ~f:(fun dir_contents (name, kind) ->
        match (kind : File_kind.t) with
        | S_LNK | S_REG ->
          let files =
            match file_f (Path.Local.relative (Path.Build.local dir) name) with
            | Some payload -> Filename.Map.add_exn dir_contents.files name payload
            | None -> dir_contents.files
          in
          Ok { dir_contents with files }
        | S_DIR ->
          let+ subdirs_contents =
            contents_of_dir ~file_f (Path.Build.relative dir name)
          in
          { dir_contents with
            subdirs = Filename.Map.add_exn dir_contents.subdirs name subdirs_contents
          }
        | _ -> Error (Unsupported_file (Path.Build.relative dir name, kind)))
  ;;

  let of_validated (validated : Validated.t) =
    let open Result.O in
    (* We assume here that [dir_name] is either a child of [root], or that we're ok with having [root/a/b] but not [root/a]. *)
    let aggregate_dir { root; contents } dir_name =
      let dir = Path.Build.relative root dir_name in
      let* new_contents = contents_of_dir ~file_f:(fun _ -> Some ()) dir in
      if is_empty_dir_conts new_contents
      then Error (Empty_dir dir)
      else (
        let contents =
          { contents with
            subdirs = Filename.Map.add_exn contents.subdirs dir_name new_contents
          }
        in
        Ok { root; contents })
    in
    let rooted_files = Filename.Set.to_map validated.files ~f:(Fun.const ()) in
    Filename.Set.to_list validated.dirs
    |> Result.List.fold_left
         ~init:
           { root = validated.root
           ; contents = { files = rooted_files; subdirs = Filename.Map.empty }
           }
         ~f:aggregate_dir
  ;;

  let of_files root (files : 'a option Path.Local.Map.t) : 'a t =
    let rec aux mb_payload contents path =
      match path, mb_payload with
      | [], _ ->
        Code_error.raise
          "Targets.Produced.of_files: path explosion failed on root"
          [ "files", Path.Local.Map.to_dyn Dyn.opaque files ]
      | [ final ], Some payload ->
        { contents with files = Filename.Map.add_exn contents.files final payload }
      | [ final ], None ->
        { contents with subdirs = Filename.Map.add_exn contents.subdirs final empty }
      | parent :: rest, _ ->
        let subdirs =
          Filename.Map.update contents.subdirs parent ~f:(fun contents_opt ->
            Some (aux mb_payload (Option.value contents_opt ~default:empty) rest))
        in
        { contents with subdirs }
    in
    let init = empty in
    let contents =
      Path.Local.Map.foldi files ~init ~f:(fun file mb_payload contents ->
        let parent = Path.Local.parent_exn file in
        if Path.Local.is_root parent
        then (
          let file = Path.Local.to_string file in
          match mb_payload with
          | Some payload ->
            { contents with files = Filename.Map.add_exn contents.files file payload }
          | None ->
            { contents with subdirs = Filename.Map.add_exn contents.subdirs file empty })
        else aux mb_payload contents (Path.Local.explode file))
    in
    { root; contents }
  ;;

  let find_any { root; contents } name =
    let open Option.O in
    let rec aux path { files; subdirs } = function
      | [] ->
        Code_error.raise
          "Targets.Produced.find_any: path explosion failed on root"
          [ "name", Path.Build.to_dyn name ]
      | [ final ] ->
        (match Filename.Map.find files final with
         | Some payload -> Some (Left payload)
         | None ->
           (* The order shouldn't matter, it's not possible to have both a file
              and a directory with the exact same path and name. *)
           let+ contents = Filename.Map.find subdirs final in
           Right contents.files)
      | parent :: rest ->
        let path = Path.Local.relative path parent in
        let* subdir = Filename.Map.find subdirs parent in
        aux path subdir rest
    in
    let root = Path.Build.local root in
    let* path = Path.Local.descendant (Path.Build.local name) ~of_:root in
    aux root contents (Path.Local.explode path)
  ;;

  let mem_any t name = Option.is_some (find_any t name)

  let find t name =
    match find_any t name with
    | Some (Left found) -> Some found
    | Some (Right _) | None -> None
  ;;

  let mem t name = Option.is_some (find t name)

  let find_dir t name =
    match find_any t name with
    | Some (Right found) -> Some found
    | Some (Left _) | None -> None
  ;;

  let mem_dir t name = Option.is_some (find_dir t name)

  let exists { contents; root = _ } ~f =
    let rec aux { files; subdirs } =
      Filename.Map.exists files ~f || Filename.Map.exists subdirs ~f:aux
    in
    aux contents
  ;;

  let all_files_seq { contents; root = _ } =
    let rec aux path { files; subdirs } =
      Seq.append
        (Filename.Map.to_seq files
         |> Seq.map ~f:(fun (file_name, payload) ->
           Path.Local.relative path file_name, payload))
        (Seq.concat
           (Filename.Map.to_seq subdirs
            |> Seq.map ~f:(fun (dir_name, subdir_contents) ->
              aux (Path.Local.relative path dir_name) subdir_contents)))
    in
    aux Path.Local.root contents
  ;;

  let all_dirs_seq { contents; root = _ } =
    let rec aux path { subdirs; files = _ } =
      Seq.concat
        (Filename.Map.to_seq subdirs
         |> Seq.map ~f:(fun (dir_name, dir_contents) ->
           let dir = Path.Local.relative path dir_name in
           Seq.cons dir (aux dir dir_contents)))
    in
    aux Path.Local.root contents
  ;;

  (* All traversal functions in this module follow the same order:
     - top-level files are processed.
     - top-level directories are processed, if applicable.
     - the content of the directories is then processed recursively.

     This explains why [root] is usually ignored and replaced by [Path.Local.root = .]:
     we don't want to process the root directory itself.
  *)

  let foldi { contents; root = _ } ~init ~f =
    let rec aux path { files; subdirs } acc =
      let acc' =
        Filename.Map.foldi files ~init:acc ~f:(fun file_name payload acc ->
          let file = Path.Local.relative path file_name in
          f file (Some payload) acc)
      in
      Filename.Map.foldi subdirs ~init:acc' ~f:(fun dir_name dir_contents acc ->
        let dir = Path.Local.relative path dir_name in
        let acc' = f dir None acc in
        aux dir dir_contents acc')
    in
    aux Path.Local.root contents init
  ;;

  let iteri { contents; root = _ } ~f ~d =
    let rec aux path { files; subdirs } =
      Filename.Map.iteri files ~f:(fun file_name payload ->
        let file = Path.Local.relative path file_name in
        f file payload);
      Filename.Map.iteri subdirs ~f:(fun dir_name dir_contents ->
        let dir = Path.Local.relative path dir_name in
        d dir;
        aux dir dir_contents)
    in
    aux Path.Local.root contents
  ;;

  let to_list_map { contents; root = _ } ~f =
    let rec aux path { files; subdirs } =
      let file_list =
        Filename.Map.to_list_map files ~f:(fun file_name payload ->
          f (Path.Local.relative path file_name) (Some payload))
      in
      let dir_list =
        Filename.Map.to_list_map subdirs ~f:(fun dir_name dir_contents ->
          let dir = Path.Local.relative path dir_name in
          let d = f dir None in
          d :: aux dir dir_contents)
        |> List.concat
      in
      file_list @ dir_list
    in
    aux Path.Local.root contents
  ;;

  let iter_files t ~f = iteri t ~f ~d:(fun _ -> ())

  (* Slightly more efficient to not even look at the files. *)
  let iter_dirs { contents; root = _ } ~f =
    let rec aux path { subdirs; files = _ } =
      Filename.Map.iteri subdirs ~f:(fun dir_name dir_contents ->
        let dir = Path.Local.relative path dir_name in
        f dir;
        aux dir dir_contents)
    in
    aux Path.Local.root contents
  ;;

  module Path_traversal = Fiber.Make_parallel_map (Path.Local.Map)
  module Filename_traversal = Fiber.Make_parallel_map (String.Map)

  let parallel_map { root; contents } ~f =
    let open Fiber.O in
    let rec aux path { files; subdirs } =
      let+ files, subdirs =
        Fiber.fork_and_join
          (fun () ->
             Filename_traversal.parallel_map files ~f:(fun file_name ->
               let file = Path.Local.relative path file_name in
               f file))
          (fun () ->
             Filename_traversal.parallel_map subdirs ~f:(fun dir_name ->
               let dir = Path.Local.relative path dir_name in
               aux dir))
      in
      { files; subdirs }
    in
    let+ contents = aux Path.Local.root contents in
    { root; contents }
  ;;

  let digest { contents; root = _ } =
    let rec all_digests _ { files; subdirs } =
      let ffiles = Filename.Map.values files in
      List.concat (ffiles :: Filename.Map.to_list_map subdirs ~f:all_digests)
    in
    Digest.generic (all_digests "ignored" contents)
  ;;

  exception Short_circuit

  (* The odd type of [d] and [f] is due to the fact that [map_with_errors]
     is used for a variety of things, not all "map-like". *)
  let map_with_errors
        ?(d : (Path.Build.t -> (unit, 'e) result) option)
        ~(f : Path.Build.t -> ('b, 'e) result)
        ~all_errors
        { root; contents }
    =
    let errors = ref [] in
    let f path =
      match f path with
      | Ok s -> Some s
      | Error e ->
        errors := (path, e) :: !errors;
        if all_errors then None else raise_notrace Short_circuit
    in
    let rec aux path { files; subdirs } =
      let files =
        Filename.Map.filter_mapi files ~f:(fun file _ ->
          f (Path.Build.relative path file))
      in
      let subdirs =
        Filename.Map.mapi subdirs ~f:(fun dir subdirs_contents ->
          let dir = Path.Build.relative path dir in
          aux dir subdirs_contents)
      in
      (match d with
       | None -> ()
       | Some f ->
         (match f path with
          | Ok () -> ()
          | Error e ->
            errors := (path, e) :: !errors;
            if all_errors then () else raise_notrace Short_circuit));
      { files; subdirs }
    in
    let result =
      try { root; contents = aux root contents } with
      | Short_circuit -> { root; contents = empty }
    in
    match Nonempty_list.of_list !errors with
    | None -> Ok result
    | Some list -> Error list
  ;;

  let to_dyn { root; contents } =
    let rec aux { files; subdirs } =
      Dyn.record
        [ "files", Filename.Map.to_dyn Dyn.opaque files
        ; "dirs", Filename.Map.to_dyn aux subdirs
        ]
    in
    Dyn.record [ "root", Path.Build.to_dyn root; "contents", aux contents ]
  ;;
end

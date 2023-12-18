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

  let to_trace_args { root; files; dirs } =
    let mkset s xs =
      if Filename.Set.is_empty xs
      then []
      else
        [ ( s
          , `List
              (Filename.Set.to_list_map xs ~f:(fun x ->
                 `String (Path.Build.relative root x |> Path.Build.to_string))) )
        ]
    in
    mkset "target_files" files @ mkset "target_dirs" dirs
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
  (* CR-someday amokhov: A hierarchical representation of the produced file
     trees may be better. It would allow for hierarchical traversals and reduce
     the number of internal invariants. *)
  type 'a t =
    { files : 'a Path.Build.Map.t
    ; dirs : 'a Filename.Map.t Path.Build.Map.t
    }

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
        (* CR-soon amokhov: This case is untested. *)
        [ Pp.textf
            "Rule produced unreadable directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
             |> Path.Source.to_string_maybe_quoted)
        ; Pp.verbatim (Unix.error_message unix_error)
        ]
      | Unsupported_file (file, kind) ->
        (* CR-soon amokhov: This case is untested. *)
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

  let of_validated =
    let rec collect dir : (unit Filename.Map.t Path.Build.Map.t, Error.t) result =
      match Path.readdir_unsorted_with_kinds (Path.build dir) with
      | Error (Unix.ENOENT, _, _) -> Error (Missing_dir dir)
      | Error e -> Error (Unreadable_dir (dir, e))
      | Ok dir_contents ->
        let open Result.O in
        let+ filenames, dirs =
          Result.List.fold_left
            dir_contents
            ~init:(Filename.Map.empty, Path.Build.Map.empty)
            ~f:(fun (acc_filenames, acc_dirs) (filename, kind) ->
              match (kind : File_kind.t) with
              (* CR-someday rleshchinskiy: Make semantics of symlinks more consistent. *)
              | S_LNK | S_REG ->
                Ok (String.Map.add_exn acc_filenames filename (), acc_dirs)
              | S_DIR ->
                let+ dir = collect (Path.Build.relative dir filename) in
                acc_filenames, Path.Build.Map.union_exn acc_dirs dir
              | _ -> Error (Unsupported_file (Path.Build.relative dir filename, kind)))
        in
        if not (String.Map.is_empty filenames)
        then Path.Build.Map.add_exn dirs dir filenames
        else dirs
    in
    let directory root dir =
      let open Result.O in
      let dir = Path.Build.relative root dir in
      let* files = collect dir in
      if Path.Build.Map.is_empty files then Error (Empty_dir dir) else Ok files
    in
    fun (validated : Validated.t) ->
      match
        Filename.Set.to_list validated.dirs
        |> Result.List.map ~f:(directory validated.root)
      with
      | Error _ as error -> error
      | Ok dirs ->
        let files =
          (* CR-someday rleshchinskiy: Check if the files actually exist here. Currently,
             we check this here for directory targets but for files, the check is done by
             the cache. *)
          Filename.Set.to_list validated.files
          |> Path.Build.Map.of_list_map_exn ~f:(fun file ->
            Path.Build.relative validated.root file, ())
        in
        (* The [union_exn] below can't raise because each map in [dirs] contains
           unique keys, which are paths rooted at the corresponding [dir]s. *)
        let dirs =
          List.fold_left dirs ~init:Path.Build.Map.empty ~f:Path.Build.Map.union_exn
        in
        Ok { files; dirs }
  ;;

  let of_files dir files =
    { files =
        Filename.Map.foldi files ~init:Path.Build.Map.empty ~f:(fun file value files ->
          Path.Build.Map.add_exn files (Path.Build.relative dir file) value)
    ; dirs = Path.Build.Map.empty
    }
  ;;

  let drop_dirs { files; dirs = _ } = { files; dirs = Path.Build.Map.empty }

  let all_files_seq t =
    Seq.append
      (Path.Build.Map.to_seq t.files)
      (Seq.concat
         (Path.Build.Map.to_seq t.dirs
          |> Seq.map ~f:(fun (dir, filenames) ->
            Filename.Map.to_seq filenames
            |> Seq.map ~f:(fun (filename, payload) ->
              Path.Build.relative dir filename, payload))))
  ;;

  let find { files; dirs } path =
    match Path.Build.Map.find files path with
    | Some _ as result -> result
    | None ->
      (match Path.Build.Map.find dirs (Path.Build.parent_exn path) with
       | Some files -> String.Map.find files (Path.Build.basename path)
       | None -> None)
  ;;

  let mem t path = Option.is_some (find t path)
  let find_dir { files = _; dirs } path = Path.Build.Map.find dirs path

  let equal { files = files1; dirs = dirs1 } { files = files2; dirs = dirs2 } ~equal =
    Path.Build.Map.equal files1 files2 ~equal
    && Path.Build.Map.equal dirs1 dirs2 ~equal:(String.Map.equal ~equal)
  ;;

  let exists { files; dirs } ~f =
    Path.Build.Map.exists files ~f || Path.Build.Map.exists dirs ~f:(String.Map.exists ~f)
  ;;

  let foldi { files; dirs } ~init ~f =
    let acc = Path.Build.Map.foldi files ~init ~f in
    Path.Build.Map.foldi dirs ~init:acc ~f:(fun dir filenames acc ->
      String.Map.foldi filenames ~init:acc ~f:(fun filename payload acc ->
        f (Path.Build.relative dir filename) payload acc))
  ;;

  let iteri { files; dirs } ~f =
    Path.Build.Map.iteri files ~f;
    Path.Build.Map.iteri dirs ~f:(fun dir filenames ->
      String.Map.iteri filenames ~f:(fun filename payload ->
        f (Path.Build.relative dir filename) payload))
  ;;

  module Path_traversal = Fiber.Make_map_traversals (Path.Build.Map)
  module Filename_traversal = Fiber.Make_map_traversals (String.Map)

  let parallel_map { files; dirs } ~f =
    let open Fiber.O in
    let+ files, dirs =
      Fiber.fork_and_join
        (fun () -> Path_traversal.parallel_map files ~f)
        (fun () ->
          Path_traversal.parallel_map dirs ~f:(fun dir files ->
            Filename_traversal.parallel_map files ~f:(fun file payload ->
              f (Path.Build.relative dir file) payload)))
    in
    { files; dirs }
  ;;

  let digest { files; dirs } =
    let all_digests =
      Path.Build.Map.values files
      :: Path.Build.Map.to_list_map dirs ~f:(fun _ -> Filename.Map.values)
    in
    Digest.generic (List.concat all_digests)
  ;;

  exception Short_circuit

  let map_with_errors
    { files; dirs }
    ~all_errors
    ~(f : Path.Build.t -> 'a -> ('b, 'e) result)
    =
    let errors = ref [] in
    let f path a =
      match f path a with
      | Ok s -> Some s
      | Error e ->
        errors := (path, e) :: !errors;
        if all_errors then None else raise_notrace Short_circuit
    in
    let result =
      try
        let files = Path.Build.Map.filter_mapi files ~f in
        let dirs =
          Path.Build.Map.mapi dirs ~f:(fun dir ->
            Filename.Map.filter_mapi ~f:(fun filename ->
              f (Path.Build.relative dir filename)))
        in
        { files; dirs }
      with
      | Short_circuit -> { files = Path.Build.Map.empty; dirs = Path.Build.Map.empty }
    in
    match Nonempty_list.of_list !errors with
    | None -> Ok result
    | Some list -> Error list
  ;;

  let to_dyn { files; dirs } =
    Dyn.record
      [ "files", Path.Build.Map.to_dyn Dyn.opaque files
      ; "dirs", Path.Build.Map.to_dyn (Filename.Map.to_dyn Dyn.opaque) dirs
      ]
  ;;
end

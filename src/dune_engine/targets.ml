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
  let create file =
    { files = Path.Build.Set.singleton file; dirs = Path.Build.Set.empty }
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

let is_empty { files; dirs } =
  Path.Build.Set.is_empty files && Path.Build.Set.is_empty dirs

let head { files; dirs } =
  match Path.Build.Set.choose files with
  | Some _ as target -> target
  | None -> Path.Build.Set.choose dirs

let head_exn t =
  match head t with
  | Some target -> target
  | None ->
    Code_error.raise "Targets.head_exn applied to empty set of targets" []

let to_dyn { files; dirs } =
  Dyn.Record
    [ ("files", Path.Build.Set.to_dyn files)
    ; ("dirs", Path.Build.Set.to_dyn dirs)
    ]

let pp { files; dirs } =
  Pp.enumerate
    (Path.Build.Set.to_list files @ Path.Build.Set.to_list dirs)
    ~f:(fun target -> Pp.text (Dpath.describe_target target))

let exists { files; dirs } ~f =
  Path.Build.Set.exists files ~f || Path.Build.Set.exists dirs ~f

module Validated = struct
  type nonrec t = t =
    { files : Path.Build.Set.t
    ; dirs : Path.Build.Set.t
    }

  let to_dyn = to_dyn

  let head = head_exn
end

module Validation_result = struct
  type t =
    | Valid of
        { parent_dir : Path.Build.t
        ; targets : Validated.t
        }
    | No_targets
    | Inconsistent_parent_dir
    | File_and_directory_target_with_the_same_name of Path.Build.t
end

let validate t =
  match is_empty t with
  | true -> Validation_result.No_targets
  | false -> (
    match Path.Build.Set.inter t.files t.dirs |> Path.Build.Set.choose with
    | Some path -> File_and_directory_target_with_the_same_name path
    | None -> (
      let parent_dir = Path.Build.parent_exn (head_exn t) in
      match
        exists t ~f:(fun path -> Path.Build.(parent_exn path <> parent_dir))
      with
      | true -> Inconsistent_parent_dir
      | false -> Valid { parent_dir; targets = t }))

module Produced = struct
  (* CR-someday amokhov: A hierarchical representation of the produced file
     trees may be better. It would allow for hierarchical traversals and reduce
     the number of internal invariants. *)
  type 'a t =
    { files : 'a Path.Build.Map.t
    ; dirs : 'a String.Map.t Path.Build.Map.t
    }

  let of_validated =
    let rec collect dir : (unit String.Map.t Path.Build.Map.t, _) result =
      match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
      | Error e -> Error (`Directory dir, e)
      | Ok dir_contents ->
        let open Result.O in
        let+ filenames, dirs =
          Result.List.fold_left dir_contents
            ~init:(String.Map.empty, Path.Build.Map.empty)
            ~f:(fun (acc_filenames, acc_dirs) (filename, kind) ->
              match (kind : File_kind.t) with
              | S_REG ->
                Ok (String.Map.add_exn acc_filenames filename (), acc_dirs)
              | S_DIR ->
                let+ dir = collect (Path.Build.relative dir filename) in
                (acc_filenames, Path.Build.Map.union_exn acc_dirs dir)
              | _ -> Ok (acc_filenames, acc_dirs))
        in
        Path.Build.Map.add_exn dirs dir filenames
    in
    fun (validated : Validated.t) ->
      match
        Path.Build.Set.to_list_map validated.dirs ~f:collect |> Result.List.all
      with
      | Error _ as error -> error
      | Ok dirs ->
        let files =
          Path.Build.Set.to_map validated.files ~f:(fun (_ : Path.Build.t) ->
              ())
        in
        (* The [union_exn] below can't raise because each map in [dirs] contains
           unique keys, which are paths rooted at the corresponding [dir]s. *)
        let dirs =
          List.fold_left dirs ~init:Path.Build.Map.empty
            ~f:Path.Build.Map.union_exn
        in
        Ok { files; dirs }

  let produced_after_rule_executed_exn ~loc targets =
    match of_validated targets with
    | Ok t -> t
    | Error (`Directory dir, (Unix.ENOENT, _, _)) ->
      User_error.raise ~loc
        [ Pp.textf "Rule failed to produce directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
            |> Path.Source.to_string_maybe_quoted)
        ]
    | Error (`Directory dir, (unix_error, _, _)) ->
      User_error.raise ~loc
        [ Pp.textf "Rule produced unreadable directory %S"
            (Path.Build.drop_build_context_maybe_sandboxed_exn dir
            |> Path.Source.to_string_maybe_quoted)
        ; Pp.verbatim (Unix.error_message unix_error)
        ]

  let of_file_list_exn list =
    { files = Path.Build.Map.of_list_exn list; dirs = Path.Build.Map.empty }

  let expand_validated_exn (validated : Validated.t) dir_filename_pairs =
    let files =
      Path.Build.Set.to_map validated.files ~f:(fun (_ : Path.Build.t) -> ())
    in
    let dirs =
      Path.Build.Map.of_list_multi dir_filename_pairs
      |> Path.Build.Map.map
           ~f:(String.Map.of_list_map_exn ~f:(fun file -> (file, ())))
    in
    let is_unexpected dir =
      not
        (Path.Build.Set.exists validated.dirs ~f:(fun validated_dir ->
             Path.Build.is_descendant dir ~of_:validated_dir))
    in
    Path.Build.Map.iteri dirs ~f:(fun dir _ ->
        if is_unexpected dir then
          Code_error.raise
            "Targets.Produced.expand_validated_exn: Unexpected directory."
            [ ("validated", Validated.to_dyn validated)
            ; ("dir", Path.Build.to_dyn dir)
            ]);
    { files; dirs }

  let all_files { files; dirs } =
    let disallow_duplicates file _payload1 _payload2 =
      Code_error.raise
        (sprintf "Targets.Produced.all_files: duplicate file %S"
           (Path.Build.to_string file))
        [ ("files", Path.Build.Map.to_dyn Dyn.opaque files)
        ; ("dirs", Path.Build.Map.to_dyn (String.Map.to_dyn Dyn.opaque) dirs)
        ]
    in
    let files_in_dirs =
      Path.Build.Map.foldi dirs ~init:Path.Build.Map.empty
        ~f:(fun dir filenames ->
          let paths =
            Path.Build.Map.of_list_exn
              (String.Map.to_list_map filenames ~f:(fun filename payload ->
                   (Path.Build.relative dir filename, payload)))
          in
          Path.Build.Map.union paths ~f:disallow_duplicates)
    in
    Path.Build.Map.union ~f:disallow_duplicates files files_in_dirs

  let all_files_seq t =
    Seq.append
      (Path.Build.Map.to_seq t.files)
      (Seq.concat
         (Path.Build.Map.to_seq t.dirs
         |> Seq.map ~f:(fun (dir, filenames) ->
                String.Map.to_seq filenames
                |> Seq.map ~f:(fun (filename, payload) ->
                       (Path.Build.relative dir filename, payload)))))

  let digest { files; dirs } =
    let all_digests =
      Path.Build.Map.values files
      :: Path.Build.Map.to_list_map dirs ~f:(fun _ -> String.Map.values)
    in
    Digest.generic (List.concat all_digests)

  module Option = struct
    exception Short_circuit

    let mapi { files; dirs } ~(f : Path.Build.t -> 'a -> 'b option) =
      let f path a =
        match f path a with
        | Some b -> b
        | None -> raise_notrace Short_circuit
      in
      try
        let files = Path.Build.Map.mapi files ~f in
        let dirs =
          Path.Build.Map.mapi dirs ~f:(fun dir ->
              String.Map.mapi ~f:(fun filename ->
                  f (Path.Build.relative dir filename)))
        in
        Some { files; dirs }
      with Short_circuit -> None
  end

  let to_dyn { files; dirs } =
    Dyn.record
      [ ("files", Path.Build.Map.to_dyn Dyn.opaque files)
      ; ("dirs", Path.Build.Map.to_dyn (String.Map.to_dyn Dyn.opaque) dirs)
      ]
end

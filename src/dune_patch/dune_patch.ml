open Stdune

(* Wrapper around the vendored [Patch] library that raises
   [User_error.E] directly instead of letting raw exceptions escape. *)
module Patch = struct
  include Patch

  let parse ~loc ~p data =
    match parse ~p data with
    | exception Parse_error { msg; lines } ->
      User_error.raise
        ~loc
        (Pp.textf "Failed to parse patch: %s" msg
         :: List.map lines ~f:(fun line -> Pp.seq (Pp.text "| ") (Pp.verbatim line)))
    | exception Failure msg ->
      User_error.raise ~loc [ Pp.textf "Invalid patch file: %s" msg ]
    | patches -> patches
  ;;

  let apply ~file ~cleanly filedata diff =
    match patch ~cleanly filedata diff with
    | exception Invalid_argument _ ->
      User_error.raise
        [ Pp.textf
            "Patch could not be applied to %S: hunk does not match file contents"
            file
        ]
    | exception Failure msg ->
      User_error.raise [ Pp.textf "Patch could not be applied to %S: %s" file msg ]
    | result -> result
  ;;
end

let re =
  let line xs = Re.seq ((Re.bol :: xs) @ [ Re.eol ]) in
  let followed_by_line xs = Re.seq [ Re.str "\n"; line xs ] in
  (* CR-soon alizter: this truncates unquoted filenames at the first
     space. Fixing this properly requires parsing the [diff --git]
     header where filenames with spaces are unambiguous. *)
  let filename = Re.group (Re.rep1 (Re.compl [ Re.space ])) in
  (* We don't care about what's after the filename. (likely a timestamp) *)
  let junk = Re.rep Re.notnl in
  Re.compile
  @@ Re.seq
       [ line [ Re.str {|--- |}; filename; junk ]
       ; followed_by_line [ Re.str {|+++ |}; filename; junk ]
       ]
;;

let git_header_re = Re.compile @@ Re.seq [ Re.bol; Re.str "diff --git " ]

let prefix_of_patch ~patch_loc patch_string =
  Re.all re patch_string
  |> List.filter_map ~f:(fun group ->
    let open Option.O in
    (* A match failure means a file name couldn't be parsed. *)
    let* old_file = Re.Group.get_opt group 1 in
    let* new_file = Re.Group.get_opt group 2 in
    let validate_as_path file =
      if not (Filename.is_relative file)
      then
        User_error.raise
          ~loc:patch_loc
          [ Pp.textf "Absolute path %S in patch file is not allowed." file ]
      else (
        let path = Path.Local.parse_string_exn ~loc:patch_loc file in
        if Path.Local.is_root path
        then
          User_error.raise
            ~loc:patch_loc
            [ Pp.textf "Directory %S in patch file is invalid." file ];
        path)
    in
    let prefix file =
      match validate_as_path file |> Path.Local.split_first_component with
      | Some _ -> 1
      | None -> 0
    in
    match old_file = "/dev/null", new_file = "/dev/null" with
    (* when both files are /dev/null we don't care about the patch. *)
    | true, true -> None
    | true, false ->
      (* Create file *)
      Some (prefix new_file)
    | false, true ->
      (* Delete file *)
      Some (prefix old_file)
    | false, false ->
      let old_path = validate_as_path old_file in
      let new_path = validate_as_path new_file in
      let prefix =
        match
          ( Path.Local.split_first_component old_path
          , Path.Local.split_first_component new_path )
        with
        | Some (_, old_suffix), Some (_, new_suffix)
          when (not (Path.Local.is_root old_suffix))
               && not (Path.Local.is_root new_suffix) ->
          (* Both files have prefixes and suffixes are not empty *)
          1
        | _, _ -> 0
      in
      (* Replace file *)
      Some prefix)
  |> List.min ~f:Int.compare
  (* Git extension patches (e.g. rename-only) may lack ---/+++ lines;
     default to p=1 when a [diff --git] header is present. *)
  |> Option.value ~default:(if Re.execp git_header_re patch_string then 1 else 0)
;;

let parse_patches ~loc ~patch_file patch_contents =
  let patch_loc = Loc.in_file patch_file in
  match
    Patch.parse ~loc ~p:(prefix_of_patch ~patch_loc patch_contents) patch_contents
  with
  | [] ->
    User_error.raise
      ~loc
      [ Pp.text
          "Could not parse the patch file. Only unified diff format is supported. \
           Context diffs and ed commands are not supported."
      ]
  | patches -> patches
;;

let write_patch_result ~file target_path = function
  | Some contents -> Io.write_file target_path contents
  | None ->
    Code_error.raise
      "Patch.patch returned None for non-delete"
      [ "file", Dyn.string file ]
;;

let apply_patches ~dir patches =
  let resolve filename = Path.append_local dir (Path.Local.of_string filename) in
  (* CR-someday alizter: [cleanly] disables fuzzy offset matching.
     The old external [patch] command used fuzz by default. If we
     find opam packages with patches that need fuzz to apply, we
     may need to revisit this. *)
  let cleanly = true in
  List.iter patches ~f:(fun (patch : Patch.t) ->
    match patch.operation with
    | Delete filename | Git_ext (_, filename, Delete_only) ->
      Fpath.unlink_no_err (Path.to_string (resolve filename))
    | Create filename | Git_ext (_, filename, Create_only) ->
      let target = resolve filename in
      Path.mkdir_p (Path.parent_exn target);
      Patch.apply ~file:filename ~cleanly None patch
      |> write_patch_result ~file:filename target
    | Edit (old_file, new_file) ->
      (* CR-someday alizter: package rules currently use copy
         sandboxing. If we switch to symlink or hardlink sandboxing,
         we should apply the patch atomically to avoid modifying
         shared targets through symlinks/hardlinks. *)
      let source = resolve old_file in
      if Fpath.exists (Path.to_string source)
      then (
        let contents = Io.read_file source in
        Patch.apply ~file:old_file ~cleanly (Some contents) patch
        |> write_patch_result ~file:old_file (resolve new_file))
      else
        User_error.raise [ Pp.textf "Cannot edit file %S: file does not exist" old_file ]
    | Git_ext (old_file, new_file, Rename_only (_, _)) ->
      let source = resolve old_file in
      if Fpath.exists (Path.to_string source)
      then Fpath.rename_exn (Path.to_string source) (Path.to_string (resolve new_file))
      else
        User_error.raise
          [ Pp.textf "Cannot rename file %S: file does not exist" old_file ])
;;

let exec ~loc ~dir ~patch =
  let open Fiber.O in
  let+ () = Fiber.return () in
  Io.read_file patch |> parse_patches ~loc ~patch_file:patch |> apply_patches ~dir
;;

module Action = Action_ext.Make (struct
    open Dune_engine

    type ('path, 'target) t = 'path

    let name = "patch"
    let version = 3
    let bimap patch f _ = f patch
    let is_useful_to ~memoize = memoize
    let encode patch input _ : Sexp.t = input patch

    let action patch ~(ectx : Action.context) ~(eenv : Action.env) =
      exec ~loc:ectx.rule_loc ~dir:eenv.working_dir ~patch
    ;;
  end)

let action ~patch = Action.action patch

module For_tests = struct
  let prefix_of_patch = prefix_of_patch
  let parse_patches = parse_patches
  let apply_patches = apply_patches
  let exec = exec
end

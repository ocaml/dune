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

let disambiguate_git_header_filenames patch_string =
  let add_tab line =
    if String.contains line '\t'
    then line
    else if String.ends_with line ~suffix:"\r"
    then String.drop_suffix_if_exists line ~suffix:"\r" ^ "\t\r"
    else line ^ "\t"
  in
  let rec loop in_git_header acc = function
    | [] -> List.rev acc
    | line :: lines ->
      if String.starts_with line ~prefix:"diff --git "
      then loop true (line :: acc) lines
      else if in_git_header && String.starts_with line ~prefix:"--- "
      then loop true (add_tab line :: acc) lines
      else if in_git_header && String.starts_with line ~prefix:"+++ "
      then loop false (add_tab line :: acc) lines
      else if in_git_header && String.starts_with line ~prefix:"@@"
      then loop false (line :: acc) lines
      else loop in_git_header (line :: acc) lines
  in
  String.split patch_string ~on:'\n' |> loop false [] |> String.concat ~sep:"\n"
;;

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
  (* Empty patches are treated as a no-op *)
  if String.is_empty (String.trim patch_contents)
  then []
  else (
    let patch_loc = Loc.in_file patch_file in
    let p = prefix_of_patch ~patch_loc patch_contents in
    let patch_contents = disambiguate_git_header_filenames patch_contents in
    match Patch.parse ~loc ~p patch_contents with
    | [] ->
      User_error.raise
        ~loc
        [ Pp.text
            "Could not parse the patch file. Only unified diff format is supported. \
             Context diffs and ed commands are not supported."
        ]
    | patches -> patches)
;;

let write_patch_result ~file target_path = function
  | Some contents -> Io.write_file_exn target_path contents
  | None ->
    Code_error.raise
      "Patch.patch returned None for non-delete"
      [ "file", Dyn.string file ]
;;

let apply_patches ~dir patches =
  let resolve filename = Path.append_local dir (Path.Local.of_string filename) in
  (* Fuzzy offset matching [cleanly = false] is enabled for two reasons: (1) it
     matches opam's behaviour  and (2) it makes life easier for some packages,
     such as OxCaml.*)
  let cleanly = false in
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
        let contents = Io.read_file_exn source in
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

module File_patch = struct
  type t =
    { patch_file : Path.t
    ; patch : Patch.t
    ; index : int
    ; source : Path.Local.t option
    ; target : Path.Local.t option
    }

  let local path = Path.Local.of_string path

  let paths = function
    | Patch.Edit (source, target) -> Some (local source), Some (local target)
    | Delete source -> Some (local source), None
    | Create target -> None, Some (local target)
    | Git_ext (source, target, extension) ->
      (match extension with
       | Rename_only _ -> Some (local source), Some (local target)
       | Delete_only -> Some (local target), None
       | Create_only -> None, Some (local target))
  ;;

  let parse ~loc ~patch_file contents =
    parse_patches ~loc ~patch_file contents
    |> List.mapi ~f:(fun index patch ->
      let source, target = paths patch.Patch.operation in
      { patch_file; patch; index; source; target })
  ;;

  let source t = t.source
  let target t = t.target

  let removes_source t =
    match t.patch.Patch.operation with
    | Delete _ | Git_ext (_, _, Delete_only) | Git_ext (_, _, Rename_only _) -> true
    | Edit _ | Create _ | Git_ext (_, _, Create_only) -> false
  ;;

  let preserves_source_mode t =
    match t.patch.Patch.operation with
    | Git_ext (_, _, Rename_only _) -> true
    | Edit _ | Delete _ | Create _
    | Git_ext (_, _, Delete_only)
    | Git_ext (_, _, Create_only) -> false
  ;;

  let missing_file t path =
    User_error.raise
      [ Pp.textf
          "trying to apply patch %s, but file %s does not exist"
          (Path.to_string_maybe_quoted t.patch_file)
          (String.maybe_quoted (Path.Local.to_string path))
      ]
  ;;

  let apply t contents =
    let file =
      match t.target, t.source with
      | Some path, _ | None, Some path -> Path.Local.to_string path
      | None, None -> Path.to_string t.patch_file
    in
    match t.patch.Patch.operation with
    | Delete _ | Git_ext (_, _, Delete_only) -> None
    | Create _ | Git_ext (_, _, Create_only) ->
      Patch.apply ~file ~cleanly:false None t.patch
    | Edit _ ->
      let source = Option.value_exn t.source in
      let contents =
        match contents with
        | Some contents -> Some contents
        | None -> missing_file t source
      in
      Patch.apply ~file ~cleanly:false contents t.patch
    | Git_ext (_, _, Rename_only _) ->
      (match contents with
       | Some contents -> Some contents
       | None -> missing_file t (Option.value_exn t.source))
  ;;

  let repr =
    Repr.record
      "file-patch"
      [ Repr.field "patch_file" Path.repr ~get:(fun t -> t.patch_file)
      ; Repr.field "index" Repr.int ~get:(fun t -> t.index)
      ; Repr.field "source" (Repr.option Path.Local.repr) ~get:(fun t -> t.source)
      ; Repr.field "target" (Repr.option Path.Local.repr) ~get:(fun t -> t.target)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
end

let exec ~loc ~dir ~patch =
  let open Fiber.O in
  let+ () = Fiber.return () in
  Io.read_file_exn patch |> parse_patches ~loc ~patch_file:patch |> apply_patches ~dir
;;

module Action = Dune_engine.Action_ext.Make (struct
    open Dune_engine

    type ('path, 'target) t = 'path

    let name = "patch"
    let version = 3
    let runs_process = false
    let can_run_in_action_runner = false
    let bimap patch f _ = f patch
    let is_useful_to_memoize = true
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

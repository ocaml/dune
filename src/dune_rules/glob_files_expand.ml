open! Import

(* Returns a list containing all descendant directories of the directory whose
   path is the concatenation of [relative_dir] onto [base_dir]. E.g., if
   [base_dir] is "foo/bar" and [relative_dir] is "baz/qux", then this function
   will return the list containing all descendants of the directory
   "foo/bar/baz/qux". The descendants of a directory are that directory's
   subdirectories, and each of of their subdirectories, and so on ad infinitum. *)
let get_descendants_of_relative_dir_relative_to_base_dir_local
  ~base_dir
  ~relative_dir
  ~prefix
  =
  let base_dir = Path.Build.drop_build_context_exn base_dir in
  let rec get_descendants_rec relative_dir prefix =
    let absolute_dir = Path.Source.relative base_dir relative_dir in
    let open Memo.O in
    let* children =
      Source_tree.find_dir absolute_dir
      >>| function
      | None -> []
      | Some dir -> Source_tree.Dir.sub_dirs dir |> String.Map.keys
    in
    let+ rest =
      Memo.List.concat_map children ~f:(fun child ->
        get_descendants_rec
          (Filename.concat relative_dir child)
          (Filename.concat prefix child))
    in
    (relative_dir, prefix) :: rest
  in
  get_descendants_rec relative_dir prefix
;;

let split_glob_string_into_parent_and_pattern glob_string =
  (* Extract the component of the string after the last path separator. This
     will be the entire string if it contains no path separators. *)
  let pattern_str = Filename.basename glob_string in
  (* Remove the pattern from the end of the string. This is done directly with
     string manipulation rather than [Filename.dirname] so that the result can
     be used to reconstruct strings representing paths to files matched by the
     glob in the style of the original glob. For example, the globs "*" and
     "./*" will match the same files, but we want the results of the latter to
     include the "./" prefix, but not the former. *)
  let prefix =
    match String.drop_suffix glob_string ~suffix:pattern_str with
    | Some x -> x
    | None ->
      Code_error.raise
        "Filename.basename did not return a suffix of the string"
        [ "glob_string", Dyn.string glob_string ]
  in
  let parent_str = Filename.dirname glob_string in
  prefix, parent_str, pattern_str
;;

module Glob_dir = struct
  (* The directory component of a glob. Globs can either be relative to some
     base dir (typically the directory containing the dune file which contains
     the glob) or absolute. *)
  type t =
    | Absolute of Path.External.t
    | Relative of
        { relative_dir : string
        ; base_dir : Path.Build.t
        }
end

module Without_vars = struct
  (* A glob whose [String_with_vars.t] has been expanded. A [Glob.t] is a
     wildcard for matching filenames only, not entire paths. The [dir] field
     holds the directory component of the original glob whereas [prefix] holds the
     directory prefix exactly as it was written. E.g., for the glob
     "foo/bar/*.txt", [dir] would be "foo/bar" and [prefix] would be "foo/bar/". *)
  type t =
    { glob : Glob.t
    ; dir : Glob_dir.t
    ; prefix : string
    ; recursive : bool
    }

  (* Returns a list of pairs (file_selector, prefix), which correspond
     to each directory that will be searched for files matching the glob. If the
     glob is not recursive, this list will be of length 1. The returned file
     selectors will expand globs relative to [base_dir], and the corresponding
     prefixes are the paths to each directory relative to [base_dir] exactly as written in
     the glob. The relative paths are required to construct relative paths to the files
     found by expanding the glob. *)
  let file_selectors_with_prefixes { glob; dir; prefix; recursive } ~loc =
    match (dir : Glob_dir.t) with
    | Relative { relative_dir; base_dir } ->
      let make_file_selector relative_dir =
        let dir = Path.Build.relative base_dir relative_dir in
        File_selector.of_glob ~dir:(Path.build dir) glob
      in
      if recursive
      then
        get_descendants_of_relative_dir_relative_to_base_dir_local
          ~base_dir
          ~relative_dir
          ~prefix
        |> Memo.map
             ~f:
               (List.map ~f:(fun (relative_dir, prefix) ->
                  make_file_selector relative_dir, prefix))
      else Memo.return [ make_file_selector relative_dir, prefix ]
    | Absolute dir ->
      if recursive
      then
        User_error.raise
          ~loc
          [ Pp.textf "Absolute paths in recursive globs are not supported." ]
      else Memo.return [ File_selector.of_glob ~dir:(Path.external_ dir) glob, prefix ]
  ;;
end

module Expanded = struct
  type t =
    { matches : string list
    ; prefix : string
    }

  let to_dyn { matches; prefix } =
    Dyn.record [ "matches", Dyn.list Dyn.string matches; "prefix", Dyn.string prefix ]
  ;;

  let matches { matches; _ } = matches
  let prefix { prefix; _ } = prefix
end

module Expand
    (M : Memo.S)
    (C : sig
       val collect_files : loc:Loc.t -> File_selector.t -> Filename_set.t M.t
     end) =
struct
  let expand_vars { Dep_conf.Glob_files.glob; recursive } ~f ~base_dir =
    let open M.O in
    let loc = String_with_vars.loc glob in
    let+ glob_str = f glob >>| Value.to_string ~dir:(Path.build base_dir) in
    let prefix, parent_str, pattern_str =
      split_glob_string_into_parent_and_pattern glob_str
    in
    let glob = Glob.of_string_exn loc pattern_str in
    let dir : Glob_dir.t =
      if Filename.is_relative parent_str
      then Relative { relative_dir = parent_str; base_dir }
      else Absolute (Path.External.of_string parent_str)
    in
    { Without_vars.glob; dir; prefix; recursive }
  ;;

  let expand (t : Dep_conf.Glob_files.t) ~f ~base_dir =
    let open M.O in
    let loc = String_with_vars.loc t.glob in
    let* without_vars = expand_vars t ~f ~base_dir in
    let+ matches =
      Without_vars.file_selectors_with_prefixes without_vars ~loc
      |> M.of_memo
      >>= M.List.concat_map ~f:(fun (file_selector, prefix) ->
        C.collect_files ~loc file_selector
        >>| Filename_set.filenames
        >>| Filename.Set.to_list_map ~f:(Filename.concat prefix))
      >>| List.sort ~compare:String.compare
    in
    { Expanded.matches; prefix = without_vars.prefix }
  ;;
end

let action_builder =
  let module Action_builder =
    Expand
      (Action_builder)
      (struct
        let collect_files = Action_builder.paths_matching
      end)
  in
  Action_builder.expand
;;

let memo =
  let module Memo =
    Expand
      (Memo)
      (struct
        let collect_files ~loc:_ = Build_system.eval_pred
      end)
  in
  Memo.expand
;;

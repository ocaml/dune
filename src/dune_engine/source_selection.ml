open Import

type t =
  { source_filenames : Filename.Array.Set.t
  ; source_dirs : Filename.Array.Set.t
  ; rules : Rule.t list
  }

let report_rule_internal_dir_conflict target_name loc =
  User_error.raise
    ~loc
    [ Pp.textf
        "This rule defines a target %S whose name conflicts with an internal directory \
         used by Dune. Please use a different name."
        (Filename.to_string target_name)
    ]
;;

type paths =
  { filenames : Filename.Array.Set.t
  ; dirnames : Filename.Array.Set.t
  }

let source_paths_to_ignore ~dir build_dir_only_sub_dirs rules =
  let of_filename_set set =
    Filename.Set.to_list set |> Filename.Array.Set.of_sorted_list
  in
  let rec iter ~filenames ~dirnames rules =
    match rules with
    | [] -> { filenames = of_filename_set filenames; dirnames = of_filename_set dirnames }
    | ({ Rule.targets; mode; _ } as rule) :: rules when Path.Build.equal dir targets.root
      ->
      let target_filenames = targets.files in
      let target_dirnames = targets.dirs in
      (match
         Filename.Set.find target_filenames ~f:(Subdir_set.mem build_dir_only_sub_dirs)
       with
       | None -> ()
       | Some target_name -> report_rule_internal_dir_conflict target_name (Rule.loc rule));
      (match mode with
       | Standard | Fallback -> iter ~filenames ~dirnames rules
       | Ignore_source_files ->
         iter
           ~filenames:(Filename.Set.union filenames target_filenames)
           ~dirnames:(Filename.Set.union dirnames target_dirnames)
           rules
       | Promote { only; _ } ->
         let target_filenames =
           match only with
           | None -> target_filenames
           | Some pred -> Filename.Set.filter target_filenames ~f:(Predicate.test pred)
         in
         iter
           ~filenames:(Filename.Set.union filenames target_filenames)
           ~dirnames:(Filename.Set.union dirnames target_dirnames)
           rules)
    | _ :: rules -> iter ~filenames ~dirnames rules
  in
  iter ~filenames:Filename.Set.empty ~dirnames:Filename.Set.empty rules
;;

let select_fallback_rules ~dir ~source_dir ~source_filenames rules =
  if Filename.Array.Set.is_empty source_filenames
  then rules
  else
    List.fold_left rules ~init:[] ~f:(fun acc (rule : Rule.t) ->
      match rule.mode with
      | Standard | Promote _ | Ignore_source_files -> rule :: acc
      | Fallback ->
        let source_filenames_for_targets =
          if not (Filename.Set.is_empty rule.targets.dirs)
          then
            Code_error.raise
              "Unexpected directory target in a Fallback rule"
              [ "targets", Targets.Validated.to_dyn rule.targets ];
          if Path.Build.equal dir rule.targets.root
          then
            rule.targets.files
            |> Filename.Set.to_list
            |> Filename.Array.Set.of_sorted_list
          else Filename.Array.Set.empty
        in
        if Filename.Array.Set.is_subset source_filenames_for_targets ~of_:source_filenames
        then acc
        else if
          Filename.Array.Set.are_disjoint source_filenames_for_targets source_filenames
        then rule :: acc
        else (
          let absent_targets =
            Filename.Array.Set.diff source_filenames_for_targets source_filenames
          in
          let present_targets =
            Filename.Array.Set.diff source_filenames_for_targets absent_targets
          in
          User_error.raise
            ~loc:(Rule.loc rule)
            [ Pp.text
                "Some of the targets of this fallback rule are present in the source \
                 tree, and some are not. This is not allowed. Either none of the targets \
                 must be present in the source tree, either they must all be."
            ; Pp.nop
            ; Pp.text "The following targets are present:"
            ; Pp.enumerate
                ~f:Path.pp
                (Filename.Array.Set.to_list_map
                   present_targets
                   ~f:(Path.relative_fname source_dir))
            ; Pp.nop
            ; Pp.text "The following targets are not:"
            ; Pp.enumerate
                ~f:Path.pp
                (Filename.Array.Set.to_list_map
                   absent_targets
                   ~f:(Path.relative_fname source_dir))
            ]))
;;

let select ~dir ~source_dir ~build_dir_only_sub_dirs ~source_filenames ~source_dirs rules =
  let ignored = source_paths_to_ignore ~dir build_dir_only_sub_dirs rules in
  let source_filenames = Filename.Array.Set.diff source_filenames ignored.filenames in
  let source_dirs = Filename.Array.Set.diff source_dirs ignored.dirnames in
  let rules = select_fallback_rules ~dir ~source_dir ~source_filenames rules in
  { source_filenames; source_dirs; rules }
;;

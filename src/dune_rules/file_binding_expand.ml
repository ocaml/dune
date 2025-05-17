open Import
open Memo.O

let relative_path_starts_with_parent relative_path =
  match String.lsplit2 relative_path ~on:'/' with
  | None -> Filename.(equal relative_path parent_dir_name)
  | Some (first, _) -> String.equal first Filename.parent_dir_name
;;

let escaping_paths_in_install_stanza =
  Warning.make
    ~default:(fun version -> if version >= (3, 11) then `Enabled else `Disabled)
    ~name:"escaping_paths_in_install_stanza"
    ~since:(3, 11)
;;

let validate_dst_for_install_stanza
      ~relative_dst_path_starts_with_parent_error_when
      ~loc
      ~dst
      ~dir
  =
  match relative_path_starts_with_parent dst with
  | false -> Memo.return ()
  | true ->
    (match relative_dst_path_starts_with_parent_error_when with
     | `Deprecation_warning_from_3_11 ->
       Warning_emit.emit
         escaping_paths_in_install_stanza
         (Warning_emit.Context.source_dir_or_enable dir)
         (fun () ->
            User_message.make
              ~loc
              [ Pp.textf
                  "The destination path %s begins with %s which will become an error in \
                   a future version of Dune. Destinations of files in install stanzas \
                   beginning with %s will be disallowed to prevent a package's installed \
                   files from escaping that package's install directories."
                  (String.maybe_quoted dst)
                  (String.maybe_quoted Filename.parent_dir_name)
                  (String.maybe_quoted Filename.parent_dir_name)
              ]
            |> Memo.return)
     | `Always_error ->
       User_error.raise
         ~loc
         [ Pp.textf
             "The destination path %s begins with %s which is not allowed. Destinations \
              in install stanzas may not begin with %s to prevent a package's installed \
              files from escaping that package's install directories."
             (String.maybe_quoted dst)
             (String.maybe_quoted Filename.parent_dir_name)
             (String.maybe_quoted Filename.parent_dir_name)
         ])
;;

let validate_for_install_stanza t ~relative_dst_path_starts_with_parent_error_when =
  let dst = File_binding.Expanded.dst_with_loc t in
  Memo.Option.iter dst ~f:(fun (loc, dst) ->
    let dir = File_binding.Expanded.dir t in
    validate_dst_for_install_stanza
      ~relative_dst_path_starts_with_parent_error_when
      ~loc
      ~dst
      ~dir)
;;

let expand_src t ~dir ~f = File_binding.Unexpanded.src t |> f >>| Path.Build.relative dir

let destination_relative_to_install_path t ~section ~expand ~expand_partial =
  let* src = expand_partial (File_binding.Unexpanded.src t)
  and+ dst_loc_opt =
    File_binding.Unexpanded.dst t
    |> Memo.Option.map ~f:(fun dst ->
      let loc = String_with_vars.loc dst in
      let+ dst = expand dst in
      dst, loc)
  in
  let+ () =
    Memo.Option.iter dst_loc_opt ~f:(fun (dst, loc) ->
      let dir = File_binding.Unexpanded.dir t in
      validate_dst_for_install_stanza
        ~relative_dst_path_starts_with_parent_error_when:`Deprecation_warning_from_3_11
        ~loc
        ~dst
        ~dir)
  in
  Install.Entry.adjust_dst ~section ~src ~dst:(Option.map dst_loc_opt ~f:fst)
;;

let expand t ~dir ~f =
  let f sw =
    let+ f = f sw in
    String_with_vars.loc sw, f
  in
  let* src =
    let+ loc, expanded = f (File_binding.Unexpanded.src t) in
    loc, Path.Build.relative dir expanded
  in
  let+ dst =
    match File_binding.Unexpanded.dst t with
    | None -> Memo.return None
    | Some dst ->
      let+ loc, p = f dst in
      Some (loc, p)
  in
  File_binding.Unexpanded.expand t ~dir:(Path.Build.drop_build_context_exn dir) ~src ~dst
;;

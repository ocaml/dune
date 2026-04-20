open Import
open Fiber.O

module Curl = struct
  let bin =
    lazy
      (match Bin.which ~path:(Env_path.path Env.initial) "curl" with
       | Some p -> p
       | None ->
         let curl = User_message.command "curl" in
         let sep = Pp.space in
         User_error.raise
           ~hints:
             [ Pp.concat
                 ~sep
                 [ Pp.text "Install"; curl; Pp.text "with your system package manager." ]
             ]
           [ Pp.concat
               ~sep
               [ Pp.text "The program"
               ; curl
               ; Pp.text "does not appear to be installed. Dune uses"
               ; curl
               ; Pp.text "to download packages. Dune requires that the"
               ; curl
               ; Pp.text
                   "executable be located in one of the directories listed in the PATH \
                    variable."
               ]
           ])
  ;;

  let user_agent =
    lazy
      (let base = "dune" in
       match Build_info.V1.version () with
       | None -> base
       | Some v -> base ^ "." ^ Build_info.V1.Version.to_string v)
  ;;

  let curl_features_regex =
    (* If these features are present, then --compressed is supported *)
    let features = [ "libz"; "brotli"; "zstd" ] in
    Re.compile
      Re.(seq [ bol; str "Features:"; rep (first (alt (List.map ~f:str features))) ])
  ;;

  let compressed_supported =
    (* We check if curl supports --compressed by running curl -V and checking if
       the output contains the features we need. *)
    Fiber.Lazy.create (fun () ->
      let+ lines, _ =
        let stderr_to =
          Process.Io.make_stderr
            ~output_on_success:Swallow
            ~output_limit:Dune_engine.Execution_parameters.Action_output_limit.default
        in
        Process.run_capture_lines
          Return
          ~stderr_to
          ~display:Quiet
          (Lazy.force bin)
          [ "-V" ]
      in
      match List.find_map lines ~f:(Re.exec_opt curl_features_regex) with
      | Some group -> Re.Group.test group 0
      | None -> false)
  ;;

  let run ~url ~temp_dir ~output =
    let* compressed_supported = Fiber.Lazy.force compressed_supported in
    let args =
      List.flatten
        [ [ "-L"
          ; "-s"
          ; "--user-agent"
          ; Lazy.force user_agent
          ; "--write-out"
          ; "\"%{http_code}\"" (* This arg must be quoted to work on windows *)
          ; "-o"
          ; Path.to_string output
          ]
        ; (if compressed_supported then [ "--compressed" ] else [])
        ; [ "--"; url ]
        ]
    in
    let stderr = Path.relative temp_dir "curl.stderr" in
    let+ http_code, exit_code =
      let stderr_to = Process.Io.file stderr Out in
      Process.run_capture_line
        Return
        ~stderr_to
        ~display:!Dune_engine.Clflags.display
        (Lazy.force bin)
        args
    in
    if exit_code <> 0
    then (
      let stderr =
        match Io.read_file stderr with
        | s ->
          Fpath.unlink_no_err (Path.to_string stderr);
          [ Pp.text s ]
        | exception s ->
          [ Pp.textf
              "Failed to read stderr from file %s"
              (Path.to_string_maybe_quoted stderr)
          ; Exn.pp s
          ]
      in
      Error
        (User_message.make
           ([ Pp.concat
                ~sep:Pp.space
                [ User_message.command "curl"
                ; Pp.textf "returned an invalid error code %d" exit_code
                ]
            ]
            @ stderr)))
    else (
      Fpath.unlink_no_err (Path.to_string stderr);
      match
        let open Option.O in
        let suffix = {|"|} in
        let prefix = suffix in
        String.drop_prefix_and_suffix ~prefix ~suffix http_code >>= Int.of_string
      with
      | None ->
        Error
          (User_message.make
             [ Pp.concat
                 ~sep:Pp.space
                 [ User_message.command "curl"
                 ; Pp.textf "returned an HTTP code we don't understand: %S" http_code
                 ]
             ])
      | Some http_code ->
        if http_code = 200
        then Ok ()
        else
          Error (User_message.make [ Pp.textf "Download failed with code %d" http_code ]))
  ;;
end

type failure =
  | Checksum_mismatch of Checksum.t
  | Unavailable of User_message.t option

let unpack_archive ~archive_driver ~target ~archive =
  Archive_driver.extract archive_driver ~archive ~target
  >>| Result.map_error ~f:(fun () ->
    Pp.textf "Unable to extract %s" (Path.to_string_maybe_quoted archive))
;;

let check_checksum checksum path =
  let checksum_error =
    match checksum with
    | None -> None
    | Some expected ->
      OpamHash.mismatch (Path.to_string path) (Checksum.to_opam_hash expected)
  in
  match checksum_error with
  | Some c -> Error (Checksum_mismatch (Checksum.of_opam_hash c))
  | None -> Ok ()
;;

let with_download url checksum ~target ~f =
  let url = OpamUrl.to_string url in
  let temp_dir =
    let prefix = "dune" in
    let suffix = Filename.basename url in
    Temp_dir.dir_for_target ~target ~prefix ~suffix
  in
  let output = Path.relative temp_dir "download" in
  Fiber.finalize ~finally:(fun () ->
    Temp.destroy Dir temp_dir;
    Fiber.return ())
  @@ fun () ->
  Curl.run ~temp_dir ~url ~output
  >>= function
  | Error message -> Fiber.return @@ Error (Unavailable (Some message))
  | Ok () ->
    (match check_checksum checksum output with
     | Ok () -> f output
     | Error _ as e -> Fiber.return e)
;;

let fetch_curl ~unpack:unpack_flag ~checksum ~target (url : OpamUrl.t) =
  with_download url checksum ~target ~f:(fun output ->
    match unpack_flag with
    | false ->
      Unix.rename (Path.to_string output) (Path.to_string target);
      Fiber.return @@ Ok ()
    | true ->
      unpack_archive
        ~archive_driver:
          (Archive_driver.choose_for_filename_default_to_tar (OpamUrl0.to_string url))
        ~target
        ~archive:output
      >>| (function
       | Ok () -> Ok ()
       | Error msg ->
         let exn =
           User_message.make
             [ Pp.textf
                 "Failed to unpack archive downloaded from %s"
                 (OpamUrl.to_string url)
             ; Pp.text "Reason:"
             ; msg
             ]
         in
         Error (Unavailable (Some exn))))
;;

let fetch_git rev_store ~target ~url:(url_loc, url) =
  OpamUrl.resolve url ~loc:url_loc rev_store
  >>= (function
   | Error _ as e -> Fiber.return e
   | Ok r -> OpamUrl.fetch_revision url ~loc:url_loc r rev_store)
  >>= function
  | Error msg -> Fiber.return @@ Error (Unavailable (Some msg))
  | Ok at_rev ->
    let+ res = Rev_store.At_rev.check_out at_rev ~target in
    Ok res
;;

let fetch_local ~checksum ~target (url, url_loc) =
  if not (OpamUrl.is_local url)
  then Code_error.raise "fetch_local: url should be file://" [ "url", OpamUrl.to_dyn url ];
  let path =
    match OpamUrl.classify url url_loc with
    | `Path p -> p
    | `Git | `Archive ->
      Code_error.raise "fetch_local: not a path" [ "url", OpamUrl.to_dyn url ]
  in
  match check_checksum checksum path with
  | Error _ as e -> Fiber.return e
  | Ok () ->
    let+ unpack_result =
      unpack_archive
        ~archive_driver:
          (Archive_driver.choose_for_filename_default_to_tar (OpamUrl0.to_string url))
        ~target
        ~archive:path
    in
    Result.map_error unpack_result ~f:(fun pp ->
      Unavailable (Some (User_message.make [ Pp.text "Could not unpack:"; pp ])))
;;

let resolve_directory_symlinks_in root =
  let follow_symlink_exn name =
    match Fpath.follow_symlink name with
    | Ok resolved -> Some resolved
    | Error (Unix_error _) -> None
    | Error Not_a_symlink ->
      Code_error.raise
        "resolve_directory_symlinks_in: not a symlink"
        [ "name", Dyn.string name ]
    | Error Max_depth_exceeded ->
      User_error.raise
        [ Pp.textf "Unable to resolve symlink %s: too many levels of symbolic links" name
        ]
  in
  let cycle_error name =
    User_error.raise
      [ Pp.textf "Unable to resolve symlink %s, it is part of a cycle." name ]
  in
  let rec resolve_rec (dir : Path.t) already_seen =
    match Readdir.read_directory_with_kinds (Path.to_string dir) with
    | Error e -> Unix_error.Detailed.raise e
    | Ok entries ->
      let sorted_entries =
        List.sort
          ~compare:(fun (name1, _k1) (name2, _k2) -> String.compare name1 name2)
          entries
      in
      List.fold_left sorted_entries ~init:already_seen ~f:(fun seen (name, kind) ->
        let relative = Path.relative dir name in
        let full_name = Path.to_string relative in
        match (kind : Unix.file_kind) with
        | S_DIR -> resolve_rec relative seen
        | S_LNK ->
          if String.Set.mem already_seen full_name then cycle_error full_name;
          let seen = String.Set.add seen full_name in
          (match follow_symlink_exn full_name with
           | None ->
             (* Delete any broken symlinks from the unpacked archive. Dune can't
                handle broken symlinks in the _build directory, but some opam
                package contain broken symlinks. The logic here is applied to the
                contents of package source archives but not to packages whose source
                is in a local directory (e.g. when a package is pinned from the
                filesystem). Broken symlinks are excluded while copying files from
                local directories into the build directory, and the logic for
                excluding them lives in [Pkg_rules.source_rules]. *)
             Fpath.unlink_no_err full_name;
             Log.info
               "Deleted broken symlink from fetched archive"
               [ "full_name", Dyn.string full_name ];
             seen
           | Some raw_resolved ->
             (* [raw_resolved] is a relative build path but it might contain indirections,
                something like _build/foo/../bar
                or _build/../outside *)
             let canon_resolved = Path.of_string raw_resolved in
             if Path.is_descendant relative ~of_:canon_resolved then cycle_error full_name;
             if not (Path.is_descendant canon_resolved ~of_:root)
             then
               User_error.raise
                 [ Pp.textf
                     "Unable to resolve symlink %s: its target %s is outside the source \
                      directory"
                     full_name
                     (Path.to_string canon_resolved)
                 ];
             (match Unix.stat raw_resolved with
              | { Unix.st_kind = S_DIR; _ } ->
                Fpath.unlink_exn full_name;
                (match Fpath.mkdir_p full_name with
                 | `Created -> ()
                 | `Already_exists ->
                   User_error.raise
                     [ Pp.textf
                         "Unable to resolve symlink %s: a directory with the same name \
                          already exists."
                         full_name
                     ]);
                (match
                   Readdir.read_directory_with_kinds (Path.to_string canon_resolved)
                 with
                 | Error e -> Unix_error.Detailed.raise e
                 | Ok children ->
                   let symlinks_in_children =
                     List.fold_left
                       children
                       ~init:false
                       ~f:(fun symlinks_in_children (child_name, child_kind) ->
                         let child_path = Filename.concat raw_resolved child_name in
                         let src = Path.of_string child_path in
                         let dst =
                           Path.of_string (Filename.concat full_name child_name)
                         in
                         match (child_kind : Unix.file_kind) with
                         | S_REG ->
                           Io.portable_hardlink ~src ~dst;
                           symlinks_in_children
                         | S_DIR ->
                           Io.portable_symlink ~src ~dst;
                           true
                         | S_LNK ->
                           follow_symlink_exn child_path
                           |> Option.iter ~f:(fun linked_path ->
                             let src = Path.of_string linked_path in
                             Io.portable_symlink ~src ~dst);
                           true
                         | _ -> symlinks_in_children)
                   in
                   if symlinks_in_children then resolve_rec relative seen else seen)
              | _ ->
                (* We do not care about symlinks pointing to anything but directories. *)
                seen))
        | _ -> seen)
  in
  let _symlinks_seen : String.Set.t = resolve_rec root String.Set.empty in
  ()
;;

let fetch ~unpack ~checksum ~target ~url:(url_loc, url) =
  let event =
    Dune_trace.(
      Out.start (global ()) (fun () ->
        Dune_trace.Event.Async.fetch
          ~url:(OpamUrl.to_string url)
          ~target
          ~checksum:(Option.map ~f:Checksum.to_string checksum)))
  in
  let unsupported_backend s =
    User_error.raise ~loc:url_loc [ Pp.textf "Unsupported backend: %s" s ]
  in
  Fiber.finalize
    ~finally:(fun () ->
      Option.iter (Dune_trace.global ()) ~f:(fun trace ->
        Dune_trace.Out.finish trace event);
      Fiber.return ())
    (fun () ->
       let+ fetch_result =
         match url.backend with
         | `git ->
           let* rev_store = Rev_store.get in
           fetch_git rev_store ~target ~url:(url_loc, url)
         | `http -> fetch_curl ~unpack ~checksum ~target url
         | `rsync ->
           if not unpack
           then
             Code_error.raise
               "fetch_local: unpack is not set"
               [ "url", OpamUrl.to_dyn url ];
           fetch_local ~checksum ~target (url, url_loc)
         | `hg -> unsupported_backend "mercurial"
         | `darcs -> unsupported_backend "darcs"
       in
       match fetch_result with
       | Ok () ->
         let target_str = Path.to_string target in
         if (Unix.lstat target_str).st_kind = S_DIR
         then resolve_directory_symlinks_in target;
         Ok ()
       | Error e -> Error e)
;;

let fetch_without_checksum ~unpack ~target ~url =
  fetch ~unpack ~checksum:None ~url ~target
  >>| function
  | Ok () -> Ok ()
  | Error (Checksum_mismatch _) -> assert false
  | Error (Unavailable message) -> Error message
;;

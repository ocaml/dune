open Import
open Fiber.O

module Curl = struct
  let bin =
    lazy
      (match Bin.which ~path:(Env_path.path Env.initial) "curl" with
       | Some p -> p
       | None ->
         User_error.raise
           ~hints:[ Pp.text "Install curl with your system package manager." ]
           [ Pp.text
               "The program \"curl\" does not appear to be installed. Dune uses curl to \
                download packages. Dune requires that the \"curl\" executable be located \
                in one of the directories listed in the PATH variable."
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
    Fiber_lazy.create (fun () ->
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
    let* compressed_supported = Fiber_lazy.force compressed_supported in
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
          Path.unlink_no_err stderr;
          [ Pp.text s ]
        | exception s ->
          [ Pp.textf
              "failed to read stderr form file %s"
              (Path.to_string_maybe_quoted stderr)
          ; Exn.pp s
          ]
      in
      Error
        (User_message.make
           ([ Pp.textf "curl returned an invalid error code %d" exit_code ] @ stderr)))
    else (
      Path.unlink_no_err stderr;
      match
        let open Option.O in
        let suffix = {|"|} in
        let prefix = suffix in
        String.drop_prefix_and_suffix ~prefix ~suffix http_code >>= Int.of_string
      with
      | None ->
        Error
          (User_message.make
             [ Pp.textf "curl returned an HTTP code we don't understand: %S" http_code ])
      | Some http_code ->
        if http_code = 200
        then Ok ()
        else
          Error (User_message.make [ Pp.textf "download failed with code %d" http_code ]))
  ;;
end

type failure =
  | Checksum_mismatch of Checksum.t
  | Unavailable of User_message.t option

let label = "dune-fetch"

let unpack_tarball ~target ~archive =
  Tar.extract ~archive ~target
  >>| Result.map_error ~f:(fun () ->
    Pp.textf "unable to extract %S" (Path.to_string archive))
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
      Path.rename output target;
      Fiber.return @@ Ok ()
    | true ->
      unpack_tarball ~target ~archive:output
      >>| (function
       | Ok () -> Ok ()
       | Error msg ->
         let exn =
           User_message.make
             [ Pp.textf
                 "failed to unpackage archive downloaded from %s"
                 (OpamUrl.to_string url)
             ; Pp.text "reason:"
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
    match OpamUrl.local_or_git_only url url_loc with
    | `Path p -> p
    | `Git -> Code_error.raise "fetch_local: not a path" [ "url", OpamUrl.to_dyn url ]
  in
  match check_checksum checksum path with
  | Error _ as e -> Fiber.return e
  | Ok () ->
    let+ unpack_result = unpack_tarball ~target ~archive:path in
    Result.map_error unpack_result ~f:(fun pp ->
      Unavailable (Some (User_message.make [ Pp.text "Could not unpack:"; pp ])))
;;

let fetch ~unpack ~checksum ~target ~url:(url_loc, url) =
  let event =
    Dune_stats.(
      start (global ()) (fun () ->
        { cat = None
        ; name = label
        ; args =
            (let args =
               [ "url", `String (OpamUrl.to_string url)
               ; "target", `String (Path.to_string target)
               ]
             in
             Some
               (match checksum with
                | None -> args
                | Some checksum ->
                  ("checksum", `String (Checksum.to_string checksum)) :: args))
        }))
  in
  let unsupported_backend s =
    User_error.raise ~loc:url_loc [ Pp.textf "Unsupported backend: %s" s ]
  in
  Fiber.finalize
    ~finally:(fun () ->
      Dune_stats.finish event;
      Fiber.return ())
    (fun () ->
      match url.backend with
      | `git ->
        let* rev_store = Rev_store.get in
        fetch_git rev_store ~target ~url:(url_loc, url)
      | `http -> fetch_curl ~unpack ~checksum ~target url
      | `rsync ->
        if not unpack
        then
          Code_error.raise "fetch_local: unpack is not set" [ "url", OpamUrl.to_dyn url ];
        fetch_local ~checksum ~target (url, url_loc)
      | `hg -> unsupported_backend "mercurial"
      | `darcs -> unsupported_backend "darcs")
;;

let fetch_without_checksum ~unpack ~target ~url =
  fetch ~unpack ~checksum:None ~url ~target
  >>| function
  | Ok () -> Ok ()
  | Error (Checksum_mismatch _) -> assert false
  | Error (Unavailable message) -> Error message
;;

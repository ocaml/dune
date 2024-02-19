open Import
open Fiber.O

module Curl = struct
  let bin =
    lazy
      (match Bin.which ~path:(Env_path.path Env.initial) "curl" with
       | Some p -> p
       | None ->
         User_error.raise
           [ Pp.concat
               ~sep:Pp.space
               [ User_message.command "curl"; Pp.text "not available in PATH" ]
             |> Pp.hovbox
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

module Fiber_job = struct
  let run (command : OpamProcess.command) =
    let prefix = "dune-source-fetch" in
    let stderr_file = Temp.create File ~prefix ~suffix:"stderr" in
    let stderr_to = Process.Io.file stderr_file Out in
    let stdout_file =
      match command.cmd_stdout with
      | None -> Temp.create File ~prefix ~suffix:"stdout"
      | Some path -> path |> Path.External.of_string |> Path.external_
    in
    let stdout_to = Process.Io.file stdout_file Out in
    let* times =
      let prog = command.cmd |> Path.External.of_string |> Path.external_ in
      let args = command.args in
      let stdin_from =
        match command.cmd_stdin with
        | Some true -> Some Process.Io.stdin
        | None | Some false -> None
      in
      let dir =
        command.cmd_dir
        |> Option.map ~f:(fun path ->
          path |> Path.Outside_build_dir.of_string |> Path.outside_build_dir)
      in
      let env = command.cmd_env |> Option.map ~f:Env.of_unix in
      Process.run_with_times
        ~display:Quiet
        ?dir
        ?env
        ?stdin_from
        ~stderr_to
        ~stdout_to
        Strict
        prog
        args
    in
    let r_stdout = Io.lines_of_file stdout_file in
    let r_stderr = Io.lines_of_file stderr_file in
    Temp.destroy File stderr_file;
    Temp.destroy File stdout_file;
    (* Process.run_with_times forces Strict failure-mode, so the return code is 0 *)
    let r_code = 0 in
    let r_duration = times.elapsed_time in
    Fiber.return
      { OpamProcess.r_code
      ; r_signal = None
      ; r_duration
      ; r_info = []
      ; r_stdout
      ; r_stderr
      ; r_cleanup = []
      }
  ;;

  let run =
    let rec run1 = function
      | OpamProcess.Job.Op.Done x -> Fiber.return x
      | Run (cmd, cont) ->
        let* r = run cmd in
        let k = cont r in
        run1 k
    in
    run1
  ;;
end

type failure =
  | Checksum_mismatch of Checksum.t
  | Unavailable of User_message.t option

let label = "dune-fetch"

let unpack ~target ~archive =
  let* () = Fiber.return () in
  Path.mkdir_p target;
  let+ (), ret =
    Process.run
      ~display:Quiet
      Return
      (Lazy.force Tar.bin)
      [ "xf"; Path.to_string archive; "-C"; Path.to_string target ]
  in
  match ret with
  | 0 -> Ok ()
  | _ -> Error (Pp.textf "unable to extract %S" (Path.to_string archive))
;;

let with_download url checksum ~f =
  let url = OpamUrl.to_string url in
  let temp_dir = Temp.create Dir ~prefix:"dune" ~suffix:(Filename.basename url) in
  let output = Path.relative temp_dir "download" in
  Fiber.finalize ~finally:(fun () ->
    Temp.destroy Dir temp_dir;
    Fiber.return ())
  @@ fun () ->
  Curl.run ~temp_dir ~url ~output
  >>= function
  | Error message -> Fiber.return @@ Error (Unavailable (Some message))
  | Ok () ->
    let checksum =
      let output = Path.to_string output in
      match checksum with
      | None -> `New (OpamHash.compute output)
      | Some checksum ->
        (match OpamHash.mismatch output (Checksum.to_opam_hash checksum) with
         | None -> `Match
         | Some s -> `Mismatch (Checksum.of_opam_hash s))
    in
    (match checksum with
     | `Mismatch m -> Fiber.return @@ Error (Checksum_mismatch m)
     | `New _ | `Match -> f output)
;;

let fetch_curl ~unpack:unpack_flag ~checksum ~target (url : OpamUrl.t) =
  with_download url checksum ~f:(fun output ->
    match unpack_flag with
    | false ->
      Path.mkdir_p (Path.parent_exn target);
      Path.rename output target;
      Fiber.return @@ Ok ()
    | true ->
      unpack ~target ~archive:output
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

let fetch_others ~unpack ~checksum ~target (url : OpamUrl.t) =
  (Fiber_job.run
   @@
   let hashes =
     match checksum with
     | None -> []
     | Some checksum -> [ Checksum.to_opam_hash checksum ]
   in
   let path = Path.to_string target in
   if OpamUrl.is_version_control url || unpack
   then (
     let dirname = OpamFilename.Dir.of_string path in
     let open OpamProcess.Job.Op in
     OpamRepository.pull_tree label dirname hashes [ url ]
     @@| function
     | Up_to_date _ -> OpamTypes.Up_to_date ()
     | Checksum_mismatch e -> Checksum_mismatch e
     | Result _ -> Result ()
     | Not_available (a, b) -> Not_available (a, b))
   else (
     let fname = OpamFilename.of_string path in
     OpamRepository.pull_file label fname hashes [ url ]))
  >>| function
  | Up_to_date () | Result () -> Ok ()
  | Not_available (None, _verbose) -> Error (Unavailable None)
  | Not_available (Some normal, verbose) ->
    let msg = User_message.make [ Pp.text normal; Pp.text verbose ] in
    Error (Unavailable (Some msg))
  | Checksum_mismatch expected ->
    Error (Checksum_mismatch (Checksum.of_opam_hash expected))
;;

let fetch_git rev_store ~target (url : OpamUrl.t) =
  OpamUrl.resolve url rev_store
  >>= (function
         | Error _ as e -> Fiber.return e
         | Ok r -> OpamUrl.fetch_revision url r rev_store)
  >>= function
  | Error msg -> Fiber.return @@ Error (Unavailable (Some msg))
  | Ok at_rev ->
    let+ res = Rev_store.At_rev.check_out at_rev ~target in
    Ok res
;;

let fetch ~unpack ~checksum ~target (url : OpamUrl.t) =
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
  Fiber.finalize
    ~finally:(fun () ->
      Dune_stats.finish event;
      Fiber.return ())
    (fun () ->
      match url.backend with
      | `git ->
        let* rev_store = Rev_store.get in
        fetch_git rev_store ~target url
      | `http -> fetch_curl ~unpack ~checksum ~target url
      | _ -> fetch_others ~unpack ~checksum ~target url)
;;

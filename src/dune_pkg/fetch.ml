open Import
open Fiber.O

module Curl = struct
  let bin = lazy (Bin.which ~path:(Env_path.path Env.initial) "curl")

  let user_agent =
    lazy
      (let base = "dune" in
       match Build_info.V1.version () with
       | None -> base
       | Some v -> base ^ "." ^ Build_info.V1.Version.to_string v)
  ;;

  let run ~url ~temp_dir ~output =
    let bin =
      match Lazy.force bin with
      | Some p -> p
      | None -> User_error.raise [ Pp.text "curl not available in PATH" ]
    in
    let args =
      [ "-L"
      ; "-s"
      ; "--compressed"
      ; "--user-agent"
      ; Lazy.force user_agent
      ; "--write-out"
      ; "%{http_code}\\n"
      ; "-o"
      ; Path.to_string output
      ; "--"
      ; url
      ]
    in
    let stderr = Path.relative temp_dir "curl.stderr" in
    let+ http_code, exit_code =
      let stderr_to = Process.Io.file stderr Out in
      Process.run_capture_line Return ~stderr_to ~display:Quiet bin args
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
      match Int.of_string http_code with
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
    let stdout_file =
      match command.cmd_stdout with
      | None -> Temp.create File ~prefix ~suffix:"stdout"
      | Some path -> path |> Path.External.of_string |> Path.external_
    in
    let stderr_to = Process.Io.file stderr_file Out in
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
    let result : OpamProcess.result =
      let r_stdout = Io.lines_of_file stdout_file in
      let r_stderr = Io.lines_of_file stderr_file in
      Temp.destroy File stderr_file;
      Temp.destroy File stdout_file;
      (* Process.run_with_times forces Strict failure-mode, so the return code is 0 *)
      let r_code = 0 in
      let r_duration = times.elapsed_time in
      { r_code
      ; r_signal = None
      ; r_duration
      ; r_info = []
      ; r_stdout
      ; r_stderr
      ; r_cleanup = []
      }
    in
    Fiber.return result
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

let fetch_curl ~unpack ~checksum ~target (url : OpamUrl.t) =
  let url = OpamUrl.to_string url in
  let temp_dir = Temp.create Dir ~prefix:"dune" ~suffix:(Filename.basename url) in
  let output = Path.relative temp_dir "download" in
  Fiber.finalize ~finally:(fun () ->
    Temp.destroy Dir temp_dir;
    Fiber.return ())
  @@ fun () ->
  let* res = Curl.run ~temp_dir ~url ~output in
  match res with
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
     | `New _ | `Match ->
       (match unpack with
        | false ->
          Io.copy_file ~src:output ~dst:target ();
          Fiber.return @@ Ok ()
        | true ->
          Fiber_job.run
            (OpamSystem.extract_job ~dir:(Path.to_string target) (Path.to_string output))
          >>| (function
           | None -> Ok ()
           | Some exn ->
             let exn =
               User_message.make
                 [ Pp.textf "failed to unpackage archive downloaded from %s" url
                 ; Pp.text "reason:"
                 ; Exn.pp exn
                 ]
             in
             Error (Unavailable (Some exn)))))
;;

let fetch_others ~unpack ~checksum ~target (url : OpamUrl.t) =
  let path = Path.to_string target in
  let+ downloaded =
    Fiber_job.run
    @@
    let hashes =
      match checksum with
      | None -> []
      | Some checksum -> [ Checksum.to_opam_hash checksum ]
    in
    match url.backend, unpack with
    | #OpamUrl.version_control, _ | _, true ->
      let dirname = OpamFilename.Dir.of_string path in
      let open OpamProcess.Job.Op in
      OpamRepository.pull_tree label dirname hashes [ url ]
      @@| (function
       | Up_to_date _ -> OpamTypes.Up_to_date ()
       | Checksum_mismatch e -> Checksum_mismatch e
       | Result _ -> Result ()
       | Not_available (a, b) -> Not_available (a, b))
    | _ ->
      let fname = OpamFilename.of_string path in
      OpamRepository.pull_file label fname hashes [ url ]
  in
  match downloaded with
  | Up_to_date () | Result () -> Ok ()
  | Not_available (None, _verbose) -> Error (Unavailable None)
  | Not_available (Some normal, verbose) ->
    let msg = User_message.make [ Pp.text normal; Pp.text verbose ] in
    Error (Unavailable (Some msg))
  | Checksum_mismatch expected ->
    Error (Checksum_mismatch (Checksum.of_opam_hash expected))
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
      (match url.backend with
       | `http -> fetch_curl
       | _ -> fetch_others)
        ~unpack
        ~checksum
        ~target
        url)
;;

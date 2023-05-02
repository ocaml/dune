open Stdune
module Process = Dune_engine.Process
module Display = Dune_engine.Display

module Fiber_job = struct
  let opam_style_read_file path =
    path |> Io.read_file
    |> String.drop_suffix_if_exists ~suffix:"\n"
    |> String.split ~on:'\n'

  let run command =
    let open Fiber.O in
    let prog =
      command.OpamProcess.cmd |> Path.External.of_string |> Path.external_
    in
    let args = command.args in
    let dir =
      command.cmd_dir
      |> Option.map ~f:(fun path ->
             path |> Path.Outside_build_dir.of_string |> Path.outside_build_dir)
    in
    let display = Display.Quiet in
    let env = command.cmd_env |> Option.map ~f:Env.of_unix in
    let stdin_from =
      match command.cmd_stdin with
      | Some true -> Some Process.Io.stdin
      | None | Some false -> None
    in
    let stderr_file =
      Temp.create Temp.File ~prefix:"source-fetch" ~suffix:"stderr"
    in
    let stdout_file =
      match command.cmd_stdout with
      | None -> Temp.create Temp.File ~prefix:"source-fetch" ~suffix:"stdout"
      | Some path -> path |> Path.External.of_string |> Path.external_
    in

    let stderr_to = Process.Io.file stderr_file Out in
    let stdout_to = Process.Io.file stdout_file Out in
    let* times =
      Process.run_with_times ~display ?dir ?env ?stdin_from ~stderr_to
        ~stdout_to prog args
    in

    let r_stdout = opam_style_read_file stdout_file in
    let r_stderr = opam_style_read_file stderr_file in
    (* Process.run_with_times forces Strict failure-mode, so the return code is 0 *)
    let r_code = 0 in
    let r_duration = times.elapsed_time in
    let result : OpamProcess.result =
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

  let run =
    let open Fiber.O in
    let rec run1 = function
      | OpamProcess.Job.Op.Done x -> Fiber.return x
      | Run (cmd, cont) ->
        let* r = run cmd in
        let k = cont r in
        run1 k
    in
    run1
end

let fetch url ~target =
  let open Fiber.O in
  let path = Path.to_string target in
  let fname = OpamFilename.of_string path in
  let hashes = [] in
  let label = "dune-fetch" in
  let job = OpamRepository.pull_file label fname hashes [ url ] in

  let* downloaded = Fiber_job.run job in
  match downloaded with
  | Up_to_date () | Result () -> Fiber.return ()
  | Not_available (normal, verbose) ->
    let n =
      match normal with
      | None -> "None"
      | Some n -> Printf.sprintf "Some %S" n
    in
    Format.eprintf "Not available: normal %s verbose %S\n" n verbose;
    Fiber.return ()

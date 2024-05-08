open Stdune
module Process = Dune_engine.Process
open Fiber.O

let bin =
  lazy
    (match Bin.which ~path:(Env_path.path Env.initial) "tar" with
     | Some x -> x
     | None -> Dune_engine.Utils.program_not_found "tar" ~loc:None)
;;

let output_limit = 1_000_000

let extract ~archive ~target =
  let* () = Fiber.return () in
  let tar = Lazy.force bin in
  let target_in_temp =
    let prefix = Path.basename target in
    let suffix = Path.basename archive in
    Temp_dir.dir_for_target ~target ~prefix ~suffix
  in
  Fiber.finalize ~finally:(fun () ->
    Temp.destroy Dir target_in_temp;
    Fiber.return ())
  @@ fun () ->
  Path.mkdir_p target_in_temp;
  let stdout_to = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit in
  let stderr_to = Process.Io.make_stderr ~output_on_success:Swallow ~output_limit in
  let args = [ "xf"; Path.to_string archive; "-C"; Path.to_string target_in_temp ] in
  let+ (), exit_code = Process.run ~display:Quiet ~stdout_to ~stderr_to Return tar args in
  match exit_code = 0 with
  | false -> Error ()
  | true ->
    let target_in_temp =
      match Path.readdir_unsorted_with_kinds target_in_temp with
      | Error e ->
        User_error.raise
          [ Pp.textf "failed to extract %s" (Path.to_string_maybe_quoted archive)
          ; Pp.text "reason:"
          ; Pp.text (Unix_error.Detailed.to_string_hum e)
          ]
      | Ok [ (fname, S_DIR) ] -> Path.relative target_in_temp fname
      | Ok _ -> target_in_temp
    in
    Path.mkdir_p (Path.parent_exn target);
    Path.rename target_in_temp target;
    Ok ()
;;

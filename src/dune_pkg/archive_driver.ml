open Stdune
module Process = Dune_engine.Process
open Fiber.O

module Command = struct
  type t =
    { bin : Path.t
    ; make_args : archive:Path.t -> target_in_temp:Path.t -> string list
    }
end

type t =
  { command : Command.t Lazy.t
  ; suffixes : string list
  }

let which bin_name = Bin.which ~path:(Env_path.path Env.initial) bin_name

let make_tar_args ~archive ~target_in_temp =
  [ "xf"; Path.to_string archive; "-C"; Path.to_string target_in_temp ]
;;

let make_zip_args ~archive ~target_in_temp =
  [ Path.to_string archive; "-d"; Path.to_string target_in_temp ]
;;

let tar =
  let command =
    lazy
      (match
         (* Test for tar before bsdtar as tar is more likely to be installed
            and both work equally well for tarballs. *)
         List.find_map [ "tar"; "bsdtar" ] ~f:which
       with
       | Some bin -> { Command.bin; make_args = make_tar_args }
       | None -> Dune_engine.Utils.program_not_found "tar" ~loc:None)
  in
  { command; suffixes = [ ".tar"; ".tar.gz"; ".tgz"; ".tar.bz2"; ".tbz" ] }
;;

let zip =
  let command =
    lazy
      (match which "unzip" with
       | Some bin -> { Command.bin; make_args = make_zip_args }
       | None ->
         (* Fall back to using tar to extract zip archives, which is possible in some cases. *)
         (match
            (* Test for bsdtar before tar, as if bsdtar is installed then it's
               likely that the tar binary is GNU tar which can't extract zip
               archives, whereas bsdtar can. If bsdtar is absent, try using the
               tar command anyway, as on MacOS, Windows, and some BSD systems,
               the tar command can extract zip archives. *)
            List.find_map [ "bsdtar"; "tar" ] ~f:which
          with
          | Some bin -> { Command.bin; make_args = make_tar_args }
          | None ->
            (* Still reference unzip in the error message, as installing it
               is the simplest way to fix the problem. *)
            Dune_engine.Utils.program_not_found "unzip" ~loc:None))
  in
  { command; suffixes = [ ".zip" ] }
;;

let all = [ tar; zip ]
let output_limit = 1_000_000
let check_suffix t filename = List.exists t.suffixes ~f:(Filename.check_suffix filename)
let choose_for_filename filename = List.find all ~f:(fun t -> check_suffix t filename)

let choose_for_filename_default_to_tar filename =
  choose_for_filename filename |> Option.value ~default:tar
;;

let extract t ~archive ~target =
  let* () = Fiber.return () in
  let command = Lazy.force t.command in
  let prefix = Path.basename target in
  let target_in_temp =
    let suffix = Path.basename archive in
    Temp_dir.dir_for_target ~target ~prefix ~suffix
  in
  let temp_stderr_path = Temp.create File ~prefix ~suffix:"stderr" in
  Fiber.finalize ~finally:(fun () ->
    Temp.destroy Dir target_in_temp;
    Temp.destroy File temp_stderr_path;
    Fiber.return ())
  @@ fun () ->
  Path.mkdir_p target_in_temp;
  let+ (), exit_code =
    let stdout_to = Process.Io.make_stdout ~output_on_success:Swallow ~output_limit in
    let args = command.make_args ~archive ~target_in_temp in
    let stderr_to = Process.Io.file temp_stderr_path Out in
    Process.run ~display:Quiet ~stdout_to ~stderr_to Return command.bin args
  in
  if exit_code = 0
  then (
    (* extract process sucess *)
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
    Ok ())
  else
    Io.with_file_in temp_stderr_path ~f:(fun err_channel ->
      let stderr_lines = Io.input_lines err_channel in
      User_error.raise
        [ Pp.textf "failed to extract '%s'" (Path.basename archive)
        ; Pp.concat
            ~sep:Pp.space
            [ Pp.text "Reason:"
            ; User_message.command @@ Path.basename command.bin
            ; Pp.textf "failed with non-zero exit code '%d' and output:" exit_code
            ]
        ; Pp.enumerate stderr_lines ~f:Pp.text
        ])
;;

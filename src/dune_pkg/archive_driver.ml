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
  { command : Command.t Fiber.Lazy.t
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
    Fiber.Lazy.create (fun () ->
      match
        (* Test for tar before bsdtar as tar is more likely to be installed
            and both work equally well for tarballs. *)
        List.find_map [ "tar"; "bsdtar" ] ~f:which
      with
      | Some bin -> Fiber.return { Command.bin; make_args = make_tar_args }
      | None ->
        Fiber.return
        @@ User_error.raise
             [ Pp.text "No program found to extract tar file. Tried:"
             ; Pp.enumerate [ "tar"; "bsdtar" ] ~f:Pp.verbatim
             ])
  in
  { command; suffixes = [ ".tar"; ".tar.gz"; ".tgz"; ".tar.bz2"; ".tbz" ] }
;;

let which_bsdtar (bin_name : string) =
  match which bin_name with
  | None -> Fiber.return None
  | Some bin ->
    let+ output, _error = Process.run_capture ~display:Quiet Return bin [ "--version" ] in
    let re = Re.compile (Re.str "bsdtar") in
    if Re.execp re output then Some bin else None
;;

let zip =
  let command =
    Fiber.Lazy.create (fun () ->
      match which "unzip" with
      | Some bin -> Fiber.return { Command.bin; make_args = make_zip_args }
      | None ->
        let rec find_tar programs =
          match programs with
          | [] -> Fiber.return None
          | x :: xs ->
            let* res = which_bsdtar x in
            (match res with
             | Some _ -> Fiber.return res
             | None -> find_tar xs)
        in
        let* program = find_tar [ "bsdtar"; "tar" ] in
        (match program with
         | Some bin -> Fiber.return { Command.bin; make_args = make_tar_args }
         | None ->
           Fiber.return
           @@ User_error.raise
                [ Pp.text "No program found to extract zip file. Tried:"
                ; Pp.enumerate [ "unzip"; "bsdtar"; "tar" ] ~f:Pp.verbatim
                ]))
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
  let* command = Fiber.Lazy.force t.command in
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

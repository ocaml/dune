open Stdune
module Process = Dune_engine.Process
open Fiber.O

type t =
  { bin : Path.t Lazy.t
  ; suffixes : string list
    (* Identify a file by its suffix rather than by its extension, since
       ".tar.gz" files only have an extension of ".gz". *)
  ; build_args : archive:Path.t -> target_in_temp:Path.t -> string list
  }

let make ~bin_name ~suffixes ~build_args =
  let bin =
    lazy
      (match Bin.which ~path:(Env_path.path Env.initial) bin_name with
       | Some x -> x
       | None -> Dune_engine.Utils.program_not_found bin_name ~loc:None)
  in
  { bin; suffixes; build_args }
;;

let tar =
  make
    ~bin_name:"tar"
    ~suffixes:[ ".tar"; ".tar.gz"; ".tgz"; ".tar.bz2"; ".tbz" ]
    ~build_args:(fun ~archive ~target_in_temp ->
      [ "xf"; Path.to_string archive; "-C"; Path.to_string target_in_temp ])
;;

let zip =
  make ~bin_name:"unzip" ~suffixes:[ ".zip" ] ~build_args:(fun ~archive ~target_in_temp ->
    [ Path.to_string archive; "-d"; Path.to_string target_in_temp ])
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
  let bin = Lazy.force t.bin in
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
  let args = t.build_args ~archive ~target_in_temp in
  let+ (), exit_code = Process.run ~display:Quiet ~stdout_to ~stderr_to Return bin args in
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

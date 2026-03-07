open Stdune
module Process = Dune_engine.Process
open Fiber.O

(** Tar implementation type, detected via --version output:
    - Libarchive: libarchive/bsdtar - auto-detects compression, can extract zip
    - Gnu: GNU tar - auto-detects compression, no zip support
    - Other: Unknown (e.g. OpenBSD) - needs explicit -z/-j flags *)
type tar_impl =
  | Libarchive
  | Gnu
  | Other

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

(* Regexes for detecting tar implementation from --version output *)
let libarchive_re = Re.compile (Re.alt [ Re.str "bsdtar"; Re.str "libarchive" ])
let gnu_tar_re = Re.compile (Re.str "GNU tar")

(** Detect tar implementation by running --version *)
let detect_tar_impl bin =
  let+ output, _ = Process.run_capture ~display:Quiet Return bin [ "--version" ] in
  if Re.execp libarchive_re output
  then Libarchive
  else if Re.execp gnu_tar_re output
  then Gnu
  else Other
;;

(** Generate tar arguments, adding -z/-j for non-auto-detecting tar *)
let make_tar_args ~tar_impl ~archive ~target_in_temp =
  let decompress_flag =
    match tar_impl with
    | Libarchive | Gnu -> [] (* auto-detect compression *)
    | Other ->
      let has_suffix = Filename.check_suffix (Path.to_string archive) in
      (* Need explicit flags for tar implementations that don't auto-detect *)
      if has_suffix ".tar.gz" || has_suffix ".tgz"
      then [ "-z" ]
      else if has_suffix ".tar.bz2" || has_suffix ".tbz"
      then [ "-j" ]
      else if has_suffix ".tar.xz" || has_suffix ".txz"
      then
        User_error.raise
          ~hints:[ Pp.text "Install GNU tar or bsdtar (libarchive) for XZ support." ]
          [ Pp.textf "Cannot extract '%s'" (Path.basename archive)
          ; Pp.text
              "The detected tar does not support XZ decompression. XZ archives require \
               GNU tar or libarchive."
          ]
      else if has_suffix ".tar.lzma" || has_suffix ".tlz"
      then
        User_error.raise
          ~hints:[ Pp.text "Install GNU tar or bsdtar (libarchive) for LZMA support." ]
          [ Pp.textf "Cannot extract '%s'" (Path.basename archive)
          ; Pp.text
              "The detected tar does not support LZMA decompression. LZMA archives \
               require GNU tar or libarchive."
          ]
      else []
  in
  [ "-x" ]
  @ decompress_flag
  @ [ "-f"; Path.to_string archive; "-C"; Path.to_string target_in_temp ]
;;

let make_zip_args ~archive ~target_in_temp =
  [ Path.to_string archive; "-d"; Path.to_string target_in_temp ]
;;

let tar =
  let command =
    Fiber.Lazy.create (fun () ->
      match List.find_map [ "tar"; "bsdtar" ] ~f:which with
      | Some bin ->
        let+ tar_impl = detect_tar_impl bin in
        { Command.bin; make_args = make_tar_args ~tar_impl }
      | None ->
        Fiber.return
        @@ User_error.raise
             [ Pp.text "No program found to extract tar file. Tried:"
             ; Pp.enumerate [ "tar"; "bsdtar" ] ~f:Pp.verbatim
             ])
  in
  { command
  ; suffixes =
      [ ".tar"
      ; ".tar.gz"
      ; ".tgz"
      ; ".tar.bz2"
      ; ".tbz"
      ; ".tar.xz"
      ; ".txz"
      ; ".tar.lzma"
      ; ".tlz"
      ]
  }
;;

let rec find_libarchive_tar = function
  | [] -> Fiber.return None
  | name :: rest ->
    (match which name with
     | None -> find_libarchive_tar rest
     | Some bin ->
       detect_tar_impl bin
       >>= (function
        | Libarchive -> Fiber.return (Some bin)
        | Gnu | Other -> find_libarchive_tar rest))
;;

let zip =
  let command =
    Fiber.Lazy.create (fun () ->
      match which "unzip" with
      | Some bin -> Fiber.return { Command.bin; make_args = make_zip_args }
      | None ->
        (* Only libarchive can extract zip, so find a tar that is libarchive *)
        let* program = find_libarchive_tar [ "bsdtar"; "tar" ] in
        (match program with
         | Some bin ->
           Fiber.return { Command.bin; make_args = make_tar_args ~tar_impl:Libarchive }
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
    Unix.rename (Path.to_string target_in_temp) (Path.to_string target);
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

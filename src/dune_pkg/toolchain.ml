open! Import
open! Stdune

module Make = struct
  let path =
    lazy
      (* TODO: prefer gmake over make if it's available for compatibility with the BSDs *)
      (match Bin.which ~path:(Env_path.path Env.initial) "make" with
       | Some p -> p
       | None ->
         User_error.raise
           ~hints:[ Pp.text "Install \"make\" with your system package manager." ]
           [ Pp.text
               "The program \"make\" does not appear to be installed. This program is \
                needed to compile the ocaml toolchain."
           ])
  ;;
end

module Dir = struct
  let make_and_check_dir path =
    if not (Path.exists path) then Path.mkdir_p path;
    if not (Path.is_directory path)
    then
      User_error.raise
        [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path) ]
  ;;

  let toolchain_base_dir () =
    let cache_dir =
      Xdg.create ~env:Sys.getenv_opt () |> Xdg.cache_dir |> Path.of_string
    in
    let d = Path.relative (Path.relative cache_dir "dune") "toolchains" in
    make_and_check_dir d;
    d
  ;;
end

module Version = struct
  type t = string

  let all = [ "4.14.2"; "5.1.1" ]
  let all_by_string = List.map all ~f:(fun t -> t, t)
  let to_string t = t

  let toolchain_dir t =
    let d = Path.relative (Dir.toolchain_base_dir ()) (to_string t) in
    Dir.make_and_check_dir d;
    d
  ;;

  let source_dir t = Path.relative (toolchain_dir t) "source"
  let target_dir t = Path.relative (toolchain_dir t) "target"

  (* A temporary directory where files will be installed before moving
     them into the target directory. This two stage installation means
     that we can guarantee that if the target directory exists then it
     contains the complete installation of the toolchain. *)
  let tmp_install_dir t = Path.relative (toolchain_dir t) "_install"

  (* When installing with the DESTDIR the full path from the root
     directory to the target directory is instantiated inside the
     directory passed as DESTDIR. This function returns the absolute
     path to the copy of the target directory inside DESTDIR. This
     assumes that the output of [tmp_install_dir] will be passed as
     DESTDIR when installing. *)
  let target_dir_within_tmp_install_dir t =
    let target_dir = target_dir t in
    let target_dir_without_root_prefix =
      (* Remove the root directory prefix from the target directory so
         it can be used to create a path relative to the temporary
         install dir. *)
      match
        String.drop_prefix
          (Path.to_string target_dir)
          ~prefix:(Path.External.to_string Path.External.root)
      with
      | Some x -> x
      | None ->
        Code_error.raise
          "Expected target dir to start with root"
          [ "target_dir", Path.to_dyn target_dir
          ; "root", Path.External.to_dyn Path.External.root
          ]
    in
    Path.relative (tmp_install_dir t) target_dir_without_root_prefix
  ;;

  let is_installed t = Path.exists (target_dir t)
end

module Compiler_package = struct
  type t =
    { version : Version.t
    ; url : OpamUrl0.t
    ; checksum : Checksum.t
    }

  let of_version version =
    (* TODO: read this information from the opam repo rather than
       hardcoding here *)
    match version with
    | "4.14.2" ->
      { version
      ; url = OpamUrl0.of_string "https://github.com/ocaml/ocaml/archive/4.14.2.tar.gz"
      ; checksum =
          Checksum.of_string
            "sha256=c2d706432f93ba85bd3383fa451d74543c32a4e84a1afaf3e8ace18f7f097b43"
      }
    | "5.1.1" ->
      { version
      ; url = OpamUrl0.of_string "https://github.com/ocaml/ocaml/archive/5.1.1.tar.gz"
      ; checksum =
          Checksum.of_string
            "sha256=57f7b382b3d71198413ede405d95ef3506f1cdc480cda1dca1e26b37cb090e17"
      }
    | other ->
      (* This is a code error as the [Version.t] type doesn't allow
         non-existant versions to be constructed. *)
      Code_error.raise
        "Invalid compiler toolchain version"
        [ "version", Dyn.string other ]
  ;;
end

let handle_checksum_mismatch { Compiler_package.version; url; checksum } ~got_checksum =
  User_error.raise
    [ Pp.textf
        "Checksum mismatch when downloading version %s of the compiler toolchain from %s."
        (Version.to_string version)
        (OpamUrl0.to_string url)
    ; Pp.textf "Expected checksum: %s" (Checksum.to_string checksum)
    ; Pp.textf "Got checksum: %s" (Checksum.to_string got_checksum)
    ]
;;

let handle_unavailable { Compiler_package.version; url; _ } ~msg_opt =
  let msg_context =
    Pp.textf
      "Unable to download version %s of the compiler toolchain from %s."
      (Version.to_string version)
      (OpamUrl0.to_string url)
  in
  let msg =
    match (msg_opt : User_message.t option) with
    | Some msg -> { msg with paragraphs = msg_context :: msg.paragraphs }
    | None -> User_message.make [ msg_context ]
  in
  raise (User_error.E msg)
;;

let fetch ({ Compiler_package.version; url; checksum } as t) =
  let open Fiber.O in
  let source_dir = Version.source_dir version in
  Fpath.rm_rf (Path.to_string source_dir);
  Path.mkdir_p source_dir;
  let+ result =
    Fetch.fetch
      ~unpack:true
      ~checksum:(Some checksum)
      ~target:source_dir
      ~url:(Loc.none, url)
  in
  match result with
  | Ok () -> ()
  | Error (Fetch.Checksum_mismatch got_checksum) ->
    handle_checksum_mismatch t ~got_checksum
  | Error (Fetch.Unavailable msg_opt) -> handle_unavailable t ~msg_opt
;;

let run_command ~dir prog args =
  let output_on_success =
    Dune_engine.Execution_parameters.Action_output_on_success.Swallow
  in
  let output_limit = Dune_engine.Execution_parameters.Action_output_limit.default in
  let stdout_to = Process.Io.make_stdout ~output_on_success ~output_limit in
  let stderr_to = Process.Io.make_stderr ~output_on_success ~output_limit in
  Process.run ~dir ~stdout_to ~stderr_to ~display:Quiet Strict prog args
;;

let configure version =
  let source_dir = Version.source_dir version in
  let configure_script = Path.relative source_dir "configure" in
  let prefix = Version.target_dir version in
  run_command ~dir:source_dir configure_script [ "--prefix"; Path.to_string prefix ]
;;

let make version args =
  let make = Lazy.force Make.path in
  let source_dir = Version.source_dir version in
  run_command ~dir:source_dir make args
;;

let build version =
  (* TODO: limit the amount of parallelism to a reasonable number of cores *)
  make version [ "-j" ]
;;

(* Installation happens in two steps. First, run `make install
   DESTDIR=...` to install the toolchain into a temporary directory. Then
   the target directory from the temporary directory is copied into the
   final installation directory. This allows us to use the fact that the
   final installation directory exists to check that the toolchain is
   installed. *)
let install version =
  let open Fiber.O in
  let dest_dir = Version.tmp_install_dir version in
  let target_dir = Version.target_dir version in
  Fpath.rm_rf (Path.to_string target_dir);
  Fpath.rm_rf (Path.to_string dest_dir);
  let+ () = make version [ "install"; sprintf "DESTDIR=%s" (Path.to_string dest_dir) ] in
  Path.rename
    (Version.target_dir_within_tmp_install_dir version)
    (Version.target_dir version);
  Fpath.rm_rf (Path.to_string dest_dir)
;;

let get ~log version =
  let open Fiber.O in
  let log_print style pp =
    match log with
    | `Never -> ()
    | _ -> User_message.print (User_message.make [ Pp.tag style pp ])
  in
  if Version.is_installed version
  then (
    (match log with
     | `Always ->
       log_print Success
       @@ Pp.textf
            "Version %s of the compiler toolchain is already installed in %s"
            (Version.to_string version)
            (Version.target_dir version |> Path.to_string)
     | _ -> ());
    Fiber.return ())
  else (
    let compiler_package = Compiler_package.of_version version in
    log_print Details
    @@ Pp.textf
         "Will install version %s of the compiler toolchain to %s"
         (Version.to_string version)
         (Version.target_dir version |> Path.to_string);
    log_print Details @@ Pp.text "Downloading...";
    let* () = fetch compiler_package in
    log_print Details @@ Pp.text "Configuring...";
    let* () = configure version in
    log_print Details @@ Pp.text "Building...";
    let* () = build version in
    log_print Details @@ Pp.text "Installing...";
    let+ () = install version in
    log_print Success
    @@ Pp.textf
         "Success! Compiler toolchain version %s installed to %s."
         (Version.to_string version)
         (Version.target_dir version |> Path.to_string))
;;

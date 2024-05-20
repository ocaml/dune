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
  let toolchain_base_dir () =
    let cache_dir =
      Xdg.create ~env:Sys.getenv_opt ()
      |> Xdg.cache_dir
      |> Path.Outside_build_dir.of_string
    in
    let path =
      Path.Outside_build_dir.relative
        (Path.Outside_build_dir.relative cache_dir "dune")
        "toolchains"
    in
    (let path = Path.outside_build_dir path in
     if not (Path.exists path) then Path.mkdir_p path;
     if not (Path.is_directory path)
     then
       User_error.raise
         [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path) ]);
    path
  ;;
end

module Compiler_package = struct
  type t =
    { version : Package_version.t
    ; url : OpamUrl0.t
    ; checksum : Checksum.t
    }

  let supported =
    let entry version_string url_string checksum_string =
      { version = Package_version.of_string version_string
      ; url = OpamUrl0.of_string url_string
      ; checksum = Checksum.of_string checksum_string
      }
    in
    [ entry
        "5.1.1"
        "https://github.com/ocaml/ocaml/archive/5.1.1.tar.gz"
        "sha256=57f7b382b3d71198413ede405d95ef3506f1cdc480cda1dca1e26b37cb090e17"
    ; entry
        "4.14.2"
        "https://github.com/ocaml/ocaml/archive/4.14.2.tar.gz"
        "sha256=c2d706432f93ba85bd3383fa451d74543c32a4e84a1afaf3e8ace18f7f097b43"
    ]
  ;;

  let latest = List.hd supported

  let of_version version =
    match List.find supported ~f:(fun t -> Package_version.equal t.version version) with
    | Some t -> t
    | None ->
      (* This is a code error as the [Version.t] type doesn't allow
         the construction of versions that don't appear in the
         supported list. *)
      Code_error.raise
        "Invalid compiler toolchain version"
        [ "version", Package_version.to_dyn version ]
  ;;

  (* Dune will require that this compiler package be chosen in
     solutions, with a version that's supported by dune toolchains.

     TODO: This will not work with packages that explicitly specify an
     alternative compiler. The ideal behaviour would be for dune to
     constrain whatever compiler package a package already depends on
     to the versions supported by dune toolchains. *)
  let preferred_compiler_package_name = Package_name.of_string "ocaml-base-compiler"

  (* The names of packages which dune will treat as compiler packages
     for the purposes of using dune toolchains instead.

     TODO: use package metadata to determine whether a package
     contains the compiler *)
  let package_names =
    preferred_compiler_package_name
    :: ([ "ocaml-system"; "ocaml-variants" ] |> List.map ~f:Package_name.of_string)
  ;;

  let is_compiler_package_by_name name =
    List.exists ~f:(Package_name.equal name) package_names
  ;;

  let constraint_ =
    let open Dune_lang in
    let constraint_ =
      Package_constraint.Or
        (List.map supported ~f:(fun { version; _ } ->
           let version_value =
             Package_constraint.Value.String_literal (Package_version.to_string version)
           in
           Package_constraint.Uop (Relop.Eq, version_value)))
    in
    { Package_dependency.name = preferred_compiler_package_name
    ; constraint_ = Some constraint_
    }
  ;;
end

module Version = struct
  type t = Package_version.t

  let latest = Compiler_package.latest.version
  let to_string = Package_version.to_string

  let all =
    List.map Compiler_package.supported ~f:(fun { Compiler_package.version; _ } ->
      version)
  ;;

  let all_by_string = List.map all ~f:(fun version -> to_string version, version)

  let of_string s =
    List.find_map all_by_string ~f:(fun (s', t) ->
      if String.equal s s' then Some t else None)
  ;;

  let of_package_version v = of_string (Package_version.to_string v)

  (* The path to a directory named after this version inside the
     toolchains directory. This directory will contain the source code
     and binary artifacts associated with a particular version of the
     compiler toolchain. *)
  let toolchain_dir t =
    Path.Outside_build_dir.relative (Dir.toolchain_base_dir ()) (to_string t)
  ;;

  let source_dir t = Path.Outside_build_dir.relative (toolchain_dir t) "source"
  let target_dir t = Path.Outside_build_dir.relative (toolchain_dir t) "target"

  (* A temporary directory within this version's toolchain directory
     where files will be installed before moving them into the target
     directory. This two stage installation means that we can
     guarantee that if the target directory exists then it contains
     the complete installation of the toolchain. *)
  let tmp_install_dir t = Path.Outside_build_dir.relative (toolchain_dir t) "_install"

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
          (Path.Outside_build_dir.to_string target_dir)
          ~prefix:(Path.External.to_string Path.External.root)
      with
      | Some x -> x
      | None ->
        Code_error.raise
          "Expected target dir to start with root"
          [ "target_dir", Path.Outside_build_dir.to_dyn target_dir
          ; "root", Path.External.to_dyn Path.External.root
          ]
    in
    Path.Outside_build_dir.relative (tmp_install_dir t) target_dir_without_root_prefix
  ;;

  let bin_dir t = Path.Outside_build_dir.relative (target_dir t) "bin"
  let is_installed t = Path.exists (Path.outside_build_dir (target_dir t))

  let flock_path t =
    Path.Outside_build_dir.relative (toolchain_dir t) "lock" |> Path.outside_build_dir
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
  let source_dir = Path.outside_build_dir (Version.source_dir version) in
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
  let source_dir = Path.outside_build_dir (Version.source_dir version) in
  let configure_script = Path.relative source_dir "configure" in
  let prefix = Version.target_dir version in
  run_command
    ~dir:source_dir
    configure_script
    [ "--prefix"; Path.Outside_build_dir.to_string prefix ]
;;

let make version args =
  let make = Lazy.force Make.path in
  let source_dir = Path.outside_build_dir (Version.source_dir version) in
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
  let dest_dir = Path.outside_build_dir (Version.tmp_install_dir version) in
  let target_dir = Path.outside_build_dir (Version.target_dir version) in
  Fpath.rm_rf (Path.to_string target_dir);
  Fpath.rm_rf (Path.to_string dest_dir);
  let+ () = make version [ "install"; sprintf "DESTDIR=%s" (Path.to_string dest_dir) ] in
  Path.rename
    (Path.outside_build_dir (Version.target_dir_within_tmp_install_dir version))
    (Path.outside_build_dir (Version.target_dir version));
  Fpath.rm_rf (Path.to_string dest_dir)
;;

let get ~log version =
  let open Fiber.O in
  let log_print style pp =
    match log with
    | `Never -> ()
    | _ -> User_message.print (User_message.make [ Pp.tag style pp ])
  in
  let print_already_installed_message () =
    match log with
    | `Always ->
      log_print Success
      @@ Pp.textf
           "Version %s of the compiler toolchain is already installed in %s"
           (Version.to_string version)
           (Version.target_dir version |> Path.Outside_build_dir.to_string)
    | _ -> ()
  in
  let download_build_install () =
    let compiler_package = Compiler_package.of_version version in
    log_print Details
    @@ Pp.textf
         "Will install version %s of the compiler toolchain to %s"
         (Version.to_string version)
         (Version.target_dir version |> Path.Outside_build_dir.to_string);
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
         (Version.target_dir version |> Path.Outside_build_dir.to_string)
  in
  if Version.is_installed version
  then (
    print_already_installed_message ();
    Fiber.return ())
  else
    Flock.with_flock
      (Version.flock_path version)
      ~name_for_messages:(sprintf "toolchain version %s" (Version.to_string version))
      ~timeout_s:infinity
      ~f:(fun () ->
        (* Note that we deliberately check if the toolchain is
           installed before and after taking the flock.

           The first check prevents us from trying to take the lock if
           the toolchain is installed. To build any package dune first
           checks if the necessary toolchain is installed, so to build
           a project with many dependencies, dune will check if the
           toolchain is installed many times. If this check required
           first taking a lock, multiple concurrent dune instances
           would sometimes contest the lock. This isn't too bad for
           performance as the lock is only held briefly, but when dune
           waits on a flock it prints a message, so freqeunt, brief
           lock acquisitions can lead to a lot of noise in the
           output.

           The second check is to handle the case where the toolchain
           was installed in between the first check, and the flock
           being acquired.
        *)
        if Version.is_installed version
        then (
          print_already_installed_message ();
          Fiber.return ())
        else download_build_install ())
;;

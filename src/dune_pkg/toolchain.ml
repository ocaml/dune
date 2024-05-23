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

module Names = struct
  let ocaml_base_compiler = Package_name.of_string "ocaml-base-compiler"
end

module Compiler = struct
  type t =
    { version : Package_version.t
    ; url : OpamUrl0.t
    ; checksum : Checksum.t option
    }

  let version_string t = Package_version.to_string t.version

  module Paths = struct
    (* The path to the directory that will contain all toolchain
       versions. Creates the directory if it doesn't already exist. *)
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
           [ Pp.textf "Expected %s to be a directory but it is not." (Path.to_string path)
           ]);
      path
    ;;

    (* The path to a directory named after the given version inside the
       toolchains directory. This directory will contain the source code
       and binary artifacts associated with a particular version of the
       compiler toolchain. *)
    let toolchain_dir t =
      Path.Outside_build_dir.relative (toolchain_base_dir ()) (version_string t)
    ;;

    let source_dir t = Path.Outside_build_dir.relative (toolchain_dir t) "source"
    let target_dir t = Path.Outside_build_dir.relative (toolchain_dir t) "target"

    (* A temporary directory within the given version's toolchain
       directory where files will be installed before moving them into
       the target directory. This two stage installation means that we
       can guarantee that if the target directory exists then it
       contains the complete installation of the toolchain. *)
    let tmp_install_dir t = Path.Outside_build_dir.relative (toolchain_dir t) "_install"

    (* When installing with the DESTDIR the full path from the root
       directory to the target directory is instantiated inside the
       directory passed as DESTDIR. This function returns the absolute
       path to the copy of the target directory inside DESTDIR. This
       assumes that the output of [tmp_install_dir_dir] will be passed as
       DESTDIR when installing. *)
    let target_dir_within_tmp_install_dir t =
      let target_dir = target_dir t in
      let target_without_root_prefix =
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
      Path.Outside_build_dir.relative (tmp_install_dir t) target_without_root_prefix
    ;;

    let flock t =
      Path.Outside_build_dir.relative (toolchain_dir t) "lock" |> Path.outside_build_dir
    ;;
  end

  module Command = struct
    let run ~dir prog args =
      let output_on_success =
        Dune_engine.Execution_parameters.Action_output_on_success.Swallow
      in
      let output_limit = Dune_engine.Execution_parameters.Action_output_limit.default in
      let stdout_to = Process.Io.make_stdout ~output_on_success ~output_limit in
      let stderr_to = Process.Io.make_stderr ~output_on_success ~output_limit in
      Process.run ~dir ~stdout_to ~stderr_to ~display:Quiet Strict prog args
    ;;

    let configure version =
      let source_dir = Path.outside_build_dir (Paths.source_dir version) in
      let configure_script = Path.relative source_dir "configure" in
      let prefix = Paths.target_dir version in
      run
        ~dir:source_dir
        configure_script
        [ "--prefix"; Path.Outside_build_dir.to_string prefix ]
    ;;

    let make version args =
      let make = Lazy.force Make.path in
      let source_dir = Path.outside_build_dir (Paths.source_dir version) in
      run ~dir:source_dir make args
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
      let dest_dir = Path.outside_build_dir (Paths.tmp_install_dir version) in
      let target_dir = Path.outside_build_dir (Paths.target_dir version) in
      Fpath.rm_rf (Path.to_string target_dir);
      Fpath.rm_rf (Path.to_string dest_dir);
      let+ () =
        make version [ "install"; sprintf "DESTDIR=%s" (Path.to_string dest_dir) ]
      in
      Path.rename
        (Path.outside_build_dir (Paths.target_dir_within_tmp_install_dir version))
        (Path.outside_build_dir (Paths.target_dir version));
      Fpath.rm_rf (Path.to_string dest_dir)
    ;;
  end

  let bin_dir t = Path.Outside_build_dir.relative (Paths.target_dir t) "bin"
  let is_version_installed t = Path.exists (Path.outside_build_dir (Paths.target_dir t))

  let of_version_and_resolved_package version resolved_package =
    let opam_file : OpamFile.OPAM.t = Resolved_package.opam_file resolved_package in
    Option.map opam_file.url ~f:(fun opam_file_url ->
      let url = OpamFile.URL.url opam_file_url in
      let checksum =
        OpamFile.URL.checksum opam_file_url
        |> List.hd_opt
        |> Option.map ~f:Checksum.of_opam_hash
      in
      { version; url; checksum })
  ;;

  let handle_checksum_mismatch t ~got_checksum =
    let checksum =
      match t.checksum with
      | Some checksum -> checksum
      | None ->
        Code_error.raise
          "checksum mismatch, but this packgae has no checksum"
          [ "got_checksum", Checksum.to_dyn got_checksum ]
    in
    User_error.raise
      [ Pp.textf
          "Checksum mismatch when downloading version %s of the compiler toolchain from \
           %s."
          (version_string t)
          (OpamUrl0.to_string t.url)
      ; Pp.textf "Expected checksum: %s" (Checksum.to_string checksum)
      ; Pp.textf "Got checksum: %s" (Checksum.to_string got_checksum)
      ]
  ;;

  let handle_unavailable t ~msg =
    let msg_context =
      Pp.textf
        "Unable to download version %s of the compiler toolchain from %s."
        (version_string t)
        (OpamUrl0.to_string t.url)
    in
    let msg =
      match (msg : User_message.t option) with
      | Some msg -> { msg with paragraphs = msg_context :: msg.paragraphs }
      | None -> User_message.make [ msg_context ]
    in
    raise (User_error.E msg)
  ;;

  let fetch ({ url; checksum; _ } as t) =
    let open Fiber.O in
    let source_dir = Path.outside_build_dir (Paths.source_dir t) in
    Fpath.rm_rf (Path.to_string source_dir);
    Path.mkdir_p source_dir;
    let+ result =
      Fetch.fetch ~unpack:true ~checksum ~target:source_dir ~url:(Loc.none, url)
    in
    match result with
    | Ok () -> ()
    | Error (Fetch.Checksum_mismatch got_checksum) ->
      handle_checksum_mismatch t ~got_checksum
    | Error (Fetch.Unavailable msg) -> handle_unavailable t ~msg
  ;;

  let log_print ~log_when style pp =
    match log_when with
    | `Never -> ()
    | _ -> User_message.print (User_message.make [ Pp.tag style pp ])
  ;;

  let print_already_installed_message t ~log_when =
    match log_when with
    | `Always ->
      log_print ~log_when User_message.Style.Success
      @@ Pp.textf
           "Version %s of the compiler toolchain is already installed in %s"
           (version_string t)
           (Paths.target_dir t |> Path.Outside_build_dir.to_string)
    | _ -> ()
  ;;

  let download_build_install t ~log_when =
    let open Fiber.O in
    let detail_log_style = User_message.Style.Details in
    log_print ~log_when detail_log_style
    @@ Pp.textf
         "Will install version %s of the compiler toolchain to %s"
         (version_string t)
         (Paths.target_dir t |> Path.Outside_build_dir.to_string);
    log_print ~log_when detail_log_style @@ Pp.text "Downloading...";
    let* () = fetch t in
    log_print ~log_when detail_log_style @@ Pp.text "Configuring...";
    let* () = Command.configure t in
    log_print ~log_when detail_log_style @@ Pp.text "Building...";
    let* () = Command.build t in
    log_print ~log_when detail_log_style @@ Pp.text "Installing...";
    let+ () = Command.install t in
    log_print ~log_when User_message.Style.Success
    @@ Pp.textf
         "Success! Compiler toolchain version %s installed to %s."
         (version_string t)
         (Paths.target_dir t |> Path.Outside_build_dir.to_string)
  ;;

  let with_flock t ~f =
    Flock.with_flock
      (Paths.flock t)
      ~name_for_messages:(sprintf "toolchain version %s" (version_string t))
      ~timeout_s:infinity
      ~f
  ;;

  let get t ~log_when =
    if is_version_installed t
    then (
      print_already_installed_message t ~log_when;
      Fiber.return ())
    else
      with_flock t ~f:(fun () ->
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
        if is_version_installed t
        then (
          print_already_installed_message t ~log_when;
          Fiber.return ())
        else download_build_install t ~log_when)
  ;;

  (* TODO: Currently we still need to add a constraint on the
     ocaml-base-compiler, as otherwise the solver tends to choose the
     ocaml-variants compiler instead. This won't be an issue once
     toolchains supports the ocaml-variants compiler at which point we
     can remove this constraint. We sholud also work out if the solver
     should be prefering ocaml-base-compiler over ocaml-variants. *)
  let constraint_ =
    { Package_dependency.name = Names.ocaml_base_compiler; constraint_ = None }
  ;;
end

module Available_compilers = struct
  type t = Resolved_package.t OpamPackage.Version.Map.t

  let equal = OpamPackage.Version.Map.equal Resolved_package.equal

  let upstream_opam_repo () =
    let loc, opam_url = Workspace.Repository.(opam_url upstream) in
    Opam_repo.of_git_repo loc opam_url
  ;;

  let load_upstream_opam_repo () =
    let open Fiber.O in
    let* repo = upstream_opam_repo () in
    Opam_repo.load_all_versions
      [ repo ]
      (Package_name.to_opam_package_name Names.ocaml_base_compiler)
  ;;

  let find_package t package_name version =
    if Package_name.equal package_name Names.ocaml_base_compiler
    then
      OpamPackage.Version.Map.find_opt (Package_version.to_opam_package_version version) t
      |> Option.bind ~f:(Compiler.of_version_and_resolved_package version)
    else None
  ;;
end

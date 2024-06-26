open! Import
open! Stdune

module Make = struct
  let path ~path =
    (* TODO: prefer gmake over make if it's available for compatibility with the BSDs *)
    match Bin.which ~path "make" with
    | Some p -> p
    | None ->
      User_error.raise
        ~hints:
          [ Pp.text "Install"
          ; User_message.command "make"
          ; Pp.text "with your system package manager."
          ]
        [ Pp.text "The program"
        ; User_message.command "make"
        ; Pp.text
            "does not appear to be installed. This program is needed to compile the \
             OCaml toolchain."
        ]
  ;;
end

module Compiler = struct
  (* Value of the PATH variable *)
  let env_path = Env_path.path Env.initial

  module Name = struct
    type t =
      { name : Package_name.t
      ; version : Package_version.t
      ; deps : Package_name.t list
      }

    (* A string identifying the compiler for use in paths and
       messages. In the case of ocaml-variants, the compiler that gets
       installed will have different features enabled depending on which
       packages the compiler depends on (the [deps] field of [t]). Thus it
       must be possible to install multiple instances of the same version
       of ocaml-variants at the same time, provided that they differ in
       their dependencies. This means that the list of dependencies must be
       present in the name of the directory where the compiler gets
       installed.

       An example identifier string for ocaml-variants with multiple
       dependencies is:
       ocaml-variants.5.2.0+options.with_deps.ocaml-option-flambda,ocaml-option-no-flat-float-array *)
    let identifier_string t =
      let deps_suffix =
        match t.deps with
        | [] -> ""
        | _ ->
          sprintf
            ".with_deps.%s"
            (String.concat
               ~sep:","
               (List.map t.deps ~f:Package_name.to_string
                |> List.sort ~compare:String.compare))
      in
      sprintf
        "%s.%s%s"
        (Package_name.to_string t.name)
        (Package_version.to_string t.version)
        deps_suffix
    ;;

    module Paths = struct
      (* The path to the directory that will contain all toolchain
         versions. Creates the directory if it doesn't already exist. *)
      let toolchain_base_dir () =
        let cache_dir =
          Lazy.force Dune_util.xdg |> Xdg.cache_dir |> Path.Outside_build_dir.of_string
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
             [ Pp.textf
                 "Expected %s to be a directory but it is not."
                 (Path.to_string path)
             ]);
        path
      ;;

      (* The path to a directory named after the given version inside the
         toolchains directory. This directory will contain the source code
         and binary artifacts associated with a particular version of the
         compiler toolchain. *)
      let toolchain_dir t =
        Path.Outside_build_dir.relative (toolchain_base_dir ()) (identifier_string t)
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

    let depends_on { deps; _ } dep = List.mem deps dep ~equal:Package_name.equal

    (* An opam filter environment for evaluating opam commands for
       building and installing packages. *)
    let opam_env t =
      let open Fiber.O in
      let make = Make.path ~path:env_path in
      let sys_poll = Sys_poll.make ~path:env_path in
      let prefix = Paths.target_dir t in
      let+ solver_env = Sys_poll.solver_env_from_current_system sys_poll in
      let solver_env =
        [ Package_variable_name.make, Path.to_string make
        ; (* set jobs to "" so that `make -j%{jobs}%` evaluates to `make -j` *)
          Package_variable_name.jobs, ""
        ; Package_variable_name.prefix, Path.Outside_build_dir.to_string prefix
        ; ( Package_variable_name.doc
          , Path.Outside_build_dir.relative prefix "doc"
            |> Path.Outside_build_dir.to_string )
        ]
        |> List.fold_left ~init:solver_env ~f:(fun acc (variable_name, string_value) ->
          Solver_env.set acc variable_name (Variable_value.string string_value))
      in
      let global_env = Solver_env.to_env solver_env in
      fun full_variable ->
        match OpamVariable.Full.scope full_variable with
        | Global -> global_env full_variable
        | Self -> None
        | Package package_name ->
          let package_name = Package_name.of_opam_package_name package_name in
          let variable =
            Package_variable_name.of_opam (OpamVariable.Full.variable full_variable)
          in
          if Package_variable_name.equal variable Package_variable_name.installed
          then (
            let value = depends_on t package_name in
            Some (B value))
          else None
    ;;
  end

  module Command = struct
    type t =
      { prog : [ `Relative_to_cwd of string | `Absolute of Path.t ]
      ; args : string list
      }

    let of_list ~path = function
      | [] -> None
      | prog :: args ->
        let prog =
          if Filename.is_implicit prog
          then (
            match Bin.which ~path prog with
            | Some prog -> `Absolute prog
            | None ->
              User_error.raise
                [ Pp.concat
                    ~sep:Pp.space
                    [ Pp.text "The program"
                    ; User_message.command prog
                    ; Pp.text "does not appear to be installed."
                    ]
                ])
          else if Filename.is_relative prog
          then `Relative_to_cwd prog
          else `Absolute (Path.of_string prog)
        in
        Some { prog; args }
    ;;

    let list_of_opam_command_list env opam_commands =
      OpamFilter.commands env opam_commands |> List.filter_map ~f:(of_list ~path:env_path)
    ;;

    let run ~dir { prog; args } =
      let output_on_success =
        Dune_engine.Execution_parameters.Action_output_on_success.Swallow
      in
      let output_limit = Dune_engine.Execution_parameters.Action_output_limit.default in
      let stdout_to = Process.Io.make_stdout ~output_on_success ~output_limit in
      let stderr_to = Process.Io.make_stderr ~output_on_success ~output_limit in
      let prog =
        match prog with
        | `Relative_to_cwd relative_path -> Path.relative dir relative_path
        | `Absolute path -> path
      in
      Process.run ~dir ~stdout_to ~stderr_to ~display:Quiet Strict prog args
    ;;
  end

  type t =
    { name : Name.t
    ; url : OpamUrl0.t
    ; opam_file : OpamFile.OPAM.t
    ; opam_env : OpamFilter.env
    }

  (* A witness that compiler has been installed *)
  type installed = t

  let build_commands t = Command.list_of_opam_command_list t.opam_env t.opam_file.build

  (* Run the build commands for the compiler. The build commands are
     taken from the compiler's opam file. *)
  let build t =
    let dir = Path.outside_build_dir (Name.Paths.source_dir t.name) in
    Fiber.sequential_iter (build_commands t) ~f:(Command.run ~dir)
  ;;

  (* Append "DESTDIR=..." to the install commands from the package's
     opam file so that when the install command is run it will install
     the compiler into a temporary directory. This also validates that
     the install command from the package's opam file begins with "make
     install", as otherwise appending "DESTDIR=..." to its argument list
     will have an unintentional effect. Raises user errors if the
     install command doesn't look as expected. *)
  let validate_and_add_destdir_to_install_commands t =
    let expectation_message =
      [ Pp.text "Expected a single unfiltered install command of the form "
      ; User_message.command "make install ..."
      ]
    in
    match t.opam_file.install with
    | [ (((CIdent "make", None) :: (CString "install", None) :: _ as args), None) ] ->
      let destdir_arg =
        sprintf
          "DESTDIR=%s"
          (Name.Paths.tmp_install_dir t.name |> Path.Outside_build_dir.to_string)
      in
      [ args @ [ CString destdir_arg, None ], None ]
    | [] ->
      User_error.raise
        (Pp.textf
           "Toolchain compiler %s has no install commands."
           (Name.identifier_string t.name)
         :: expectation_message)
    | [ (_, Some filter) ] ->
      User_error.raise
        (Pp.textf
           "Toolchain compiler %s has a filter on its install command:  %s"
           (Name.identifier_string t.name)
           (OpamFilter.to_string filter)
         :: expectation_message)
    | [ (_, None) ] ->
      User_error.raise
        (Pp.textf
           "Toolchain compiler %s is not of the form "
           (Name.identifier_string t.name)
         :: User_message.command "make install..."
         :: expectation_message)
    | _many ->
      User_error.raise
        (Pp.textf
           "Toolchain compiler %s has multiple install commands."
           (Name.identifier_string t.name)
         :: expectation_message)
  ;;

  (* The install commands from the package's opam file, modified to
     install the package into a temporary directory. *)
  let install_commands_with_destdir t =
    Command.list_of_opam_command_list
      t.opam_env
      (validate_and_add_destdir_to_install_commands t)
  ;;

  (* Run the install commands from the opam file, modified to install
     the package into a temporary directory. *)
  let install_tmp t =
    let dir = Path.outside_build_dir (Name.Paths.source_dir t.name) in
    Fiber.sequential_iter (install_commands_with_destdir t) ~f:(Command.run ~dir)
  ;;

  (* Install the compiler in the appropriate toolchain directory.

     Installation happens in two steps. First, run the package's
     install command passing "compilerIR=..." to install the toolchain
     into a temporary directory. Then the target directory from the
     temporary directory is copied into the final installation
     directory. This allows us to use the fact that the final
     installation directory exists to check that the toolchain is
     installed. *)
  let install t =
    let open Fiber.O in
    let dest_dir = Path.outside_build_dir (Name.Paths.tmp_install_dir t.name) in
    let target_dir = Path.outside_build_dir (Name.Paths.target_dir t.name) in
    Fpath.rm_rf (Path.to_string target_dir);
    Fpath.rm_rf (Path.to_string dest_dir);
    let+ () = install_tmp t in
    Path.rename
      (Path.outside_build_dir (Name.Paths.target_dir_within_tmp_install_dir t.name))
      (Path.outside_build_dir (Name.Paths.target_dir t.name));
    Fpath.rm_rf (Path.to_string dest_dir)
  ;;

  let bin_dir t = Path.Outside_build_dir.relative (Name.Paths.target_dir t.name) "bin"
  let is_installed t = Path.exists (Path.outside_build_dir (Name.Paths.target_dir t.name))

  (* The first checksum appearing in the compiler's opam file. This will
     be used to validate the complier's source archive. Note that ideally
     we would validate the compiler source archive against all the
     checksums from the compiler's opam file however [Fetch.fetch]
     currently only accepts a single checksum for validation. *)
  let first_checksum { opam_file; _ } =
    Option.bind opam_file.url ~f:(fun opam_file_url ->
      OpamFile.URL.checksum opam_file_url
      |> List.hd_opt
      |> Option.map ~f:Checksum.of_opam_hash)
  ;;

  let of_resolved_package resolved_package ~deps =
    let open Fiber.O in
    let package = Resolved_package.package resolved_package in
    let opam_file : OpamFile.OPAM.t = Resolved_package.opam_file resolved_package in
    match opam_file.url with
    | Some opam_file_url ->
      let url = OpamFile.URL.url opam_file_url in
      let name =
        { Name.name = Package_name.of_opam_package_name package.name
        ; version = Package_version.of_opam_package_version package.version
        ; deps
        }
      in
      let+ opam_env = Name.opam_env name in
      Some { name; url; opam_file; opam_env }
    | None -> Fiber.return None
  ;;

  let handle_checksum_mismatch t ~got_checksum =
    let checksum =
      match first_checksum t with
      | Some checksum -> checksum
      | None ->
        Code_error.raise
          "checksum mismatch, but this packgae has no checksum"
          [ "got_checksum", Checksum.to_dyn got_checksum ]
    in
    User_error.raise
      [ Pp.textf
          "Checksum mismatch when downloading compiler toolchain %s from %s."
          (Name.identifier_string t.name)
          (OpamUrl0.to_string t.url)
      ; Pp.textf "Expected checksum: %s" (Checksum.to_string checksum)
      ; Pp.textf "Got checksum: %s" (Checksum.to_string got_checksum)
      ]
  ;;

  let handle_unavailable t ~msg =
    let msg_context =
      Pp.textf
        "Unable to download compiler toolchain %s from %s."
        (Name.identifier_string t.name)
        (OpamUrl0.to_string t.url)
    in
    let msg =
      match (msg : User_message.t option) with
      | Some msg -> { msg with paragraphs = msg_context :: msg.paragraphs }
      | None -> User_message.make [ msg_context ]
    in
    raise (User_error.E msg)
  ;;

  let fetch t =
    let open Fiber.O in
    let source_dir = Path.outside_build_dir (Name.Paths.source_dir t.name) in
    Fpath.rm_rf (Path.to_string source_dir);
    Path.mkdir_p source_dir;
    let+ result =
      Fetch.fetch
        ~unpack:true
        ~checksum:(first_checksum t)
        ~target:source_dir
        ~url:(Loc.none, t.url)
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
           "Compiler toolchain %s is already installed in %s"
           (Name.identifier_string t.name)
           (Name.Paths.target_dir t.name |> Path.Outside_build_dir.to_string)
    | _ -> ()
  ;;

  let download_build_install t ~log_when =
    let open Fiber.O in
    let detail_log_style = User_message.Style.Details in
    log_print ~log_when detail_log_style
    @@ Pp.textf
         "Will install compiler toolchain %s to %s"
         (Name.identifier_string t.name)
         (Name.Paths.target_dir t.name |> Path.Outside_build_dir.to_string);
    log_print ~log_when detail_log_style @@ Pp.text "Downloading...";
    let* () = fetch t in
    log_print ~log_when detail_log_style @@ Pp.text "Building...";
    let* () = build t in
    log_print ~log_when detail_log_style @@ Pp.text "Installing...";
    let+ () = install t in
    log_print ~log_when User_message.Style.Success
    @@ Pp.textf
         "Success! Compiler toolchain %s installed to %s."
         (Name.identifier_string t.name)
         (Name.Paths.target_dir t.name |> Path.Outside_build_dir.to_string)
  ;;

  let with_flock t ~f =
    Flock.with_flock
      (Name.Paths.flock t.name)
      ~name_for_messages:(sprintf "compiler toolchain %s" (Name.identifier_string t.name))
      ~timeout_s:infinity
      ~f
  ;;

  let ensure_installed t ~log_when =
    if is_installed t
    then (
      print_already_installed_message t ~log_when;
      Fiber.return t)
    else
      with_flock t ~f:(fun () ->
        let open Fiber.O in
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
        if is_installed t
        then (
          print_already_installed_message t ~log_when;
          Fiber.return t)
        else
          let+ () = download_build_install t ~log_when in
          t)
  ;;
end

module Available_compilers = struct
  type t = Opam_repo.t

  let equal = Opam_repo.equal

  let load_upstream_opam_repo () =
    (* CR-later: load the compiler from all repos in the workspace *)
    let loc, opam_url = Workspace.Repository.(opam_url upstream) in
    Opam_repo.of_git_repo loc opam_url
  ;;

  (* The names of packages that can be installed by the toolchains
     mechanism. Note that for a package to be compatible with the
     toolchains mechanism, it is necessary that the package can be
     installed by running `make install` in its source directory after
     running whatever build commands are specified in its opam
     file. Additionally, the package's makefile must implement the
     DESTDIR protocol. *)
  let compiler_package_names =
    List.map ~f:Package_name.of_string [ "ocaml-base-compiler"; "ocaml-variants" ]
  ;;

  let is_compiler_package_name = List.mem compiler_package_names ~equal:Package_name.equal

  let find t package_name package_version ~deps =
    if is_compiler_package_name package_name
    then
      let open Fiber.O in
      let* resolved_packages_by_version =
        Opam_repo.load_all_versions [ t ] (Package_name.to_opam_package_name package_name)
      in
      match
        OpamPackage.Version.Map.find_opt
          (Package_version.to_opam_package_version package_version)
          resolved_packages_by_version
      with
      | Some resolved_package -> Compiler.of_resolved_package resolved_package ~deps
      | None -> Fiber.return None
    else Fiber.return None
  ;;
end

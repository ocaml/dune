open Import
module Artifact_substitution = Dune_rules.Artifact_substitution

let synopsis =
  [ `P "The installation directories used are defined by priority:"
  ; `Noblank
  ; `P
      "- directories set on the command line of $(i,dune install), or corresponding \
       environment variables"
  ; `Noblank
  ; `P
      "- directories set in dune binary. They are setup before the compilation of dune \
       with $(i,./configure)"
  ; `Noblank
  ; `P "- inferred from the environment variable $(i,OPAM_SWITCH_PREFIX) if present"
  ]
;;

let print_line ~(verbosity : Dune_engine.Display.t) fmt =
  Printf.ksprintf
    (fun s ->
      match verbosity with
      | Quiet -> ()
      | _ -> Console.print [ Pp.verbatim s ])
    fmt
;;

let interpret_destdir ~destdir path =
  match destdir with
  | None -> path
  | Some destdir -> Path.append_local destdir (Path.local_part path)
;;

let get_dirs context ~prefix_from_command_line ~from_command_line =
  let open Fiber.O in
  let module Roots = Install.Roots in
  let prefix_from_command_line = Option.map ~f:Path.of_string prefix_from_command_line in
  let+ roots =
    match prefix_from_command_line with
    | None -> Memo.run (Context.roots context)
    | Some prefix ->
      Roots.opam_from_prefix prefix ~relative:Path.relative
      |> Roots.map ~f:(fun s -> Some s)
      |> Fiber.return
  in
  let roots = Roots.first_has_priority from_command_line roots in
  let must_be_defined name v =
    match v with
    | Some v -> v
    | None ->
      (* We suggest that the user sets --prefix first rather than the specific
         missing option, since this is the most common case. *)
      User_error.raise
        [ Pp.textf "The %s installation directory is unknown." name ]
        ~hints:
          [ Pp.concat
              ~sep:Pp.space
              [ Pp.text "It can be specified with"
              ; User_message.command "--prefix"
              ; Pp.textf "or by setting"
              ; User_message.command (sprintf "--%s" name)
              ]
            |> Pp.hovbox
          ]
  in
  { Roots.lib_root = must_be_defined "libdir" roots.lib_root
  ; libexec_root = must_be_defined "libexecdir" roots.libexec_root
  ; bin = must_be_defined "bindir" roots.bin
  ; sbin = must_be_defined "sbindir" roots.sbin
  ; etc_root = must_be_defined "etcdir" roots.etc_root
  ; doc_root = must_be_defined "docdir" roots.doc_root
  ; share_root = must_be_defined "datadir" roots.share_root
  ; man = must_be_defined "mandir" roots.man
  }
;;

module Workspace = struct
  type t =
    { packages : Package.t Package.Name.Map.t
    ; contexts : Context.t list
    }

  let get () =
    let open Memo.O in
    Memo.run
      (let+ packages = Dune_rules.Dune_load.packages ()
       and+ contexts = Context.DB.all () in
       { packages; contexts })
  ;;

  let package_install_file t ~findlib_toolchain pkg =
    match Package.Name.Map.find t.packages pkg with
    | None -> Error ()
    | Some p ->
      let name = Package.name p in
      let dir = Package.dir p in
      Ok
        (Path.Source.relative
           dir
           (Dune_rules.Install_rules.install_file ~package:name ~findlib_toolchain))
  ;;
end

let resolve_package_install workspace ~findlib_toolchain pkg =
  match Workspace.package_install_file workspace ~findlib_toolchain pkg with
  | Ok path -> path
  | Error () ->
    let pkg = Package.Name.to_string pkg in
    User_error.raise
      [ Pp.textf "Unknown package %s!" pkg ]
      ~hints:
        (User_message.did_you_mean
           pkg
           ~candidates:
             (Package.Name.Map.keys workspace.packages
              |> List.map ~f:Package.Name.to_string))
;;

let print_unix_error f =
  try f () with
  | Unix.Unix_error (error, syscall, arg) ->
    let error = Unix_error.Detailed.create error ~syscall ~arg in
    User_message.prerr (User_error.make [ Unix_error.Detailed.pp error ])
;;

module Special_file = struct
  type t =
    | META
    | Dune_package

  let of_entry (e : _ Install.Entry.t) =
    match e.section with
    | Lib ->
      let dst = Install.Entry.Dst.to_string e.dst in
      if dst = Dune_findlib.Findlib.Package.meta_fn
      then Some META
      else if dst = Dune_package.fn
      then Some Dune_package
      else None
    | _ -> None
  ;;
end

type copy_kind =
  | Substitute (** Use [Artifact_substitution.copy_file]. Will scan all bytes. *)
  | Special of Special_file.t (** Hooks to add version numbers, replace sections, etc *)

type rmdir_mode =
  | Fail
  | Warn

(** Operations that act on real files or just pretend to (for --dry-run) *)
module type File_operations = sig
  val copy_file
    :  src:Path.t
    -> dst:Path.t
    -> executable:bool
    -> kind:copy_kind
    -> package:Package.Name.t
    -> conf:Artifact_substitution.Conf.t
    -> unit Fiber.t

  val mkdir_p : Path.t -> unit
  val remove_file_if_exists : Path.t -> unit
  val remove_dir_if_exists : if_non_empty:rmdir_mode -> Path.t -> unit
end

module File_ops_dry_run (Verbosity : sig
    val verbosity : Dune_engine.Display.t
  end) : File_operations = struct
  open Verbosity

  let print_line fmt = print_line ~verbosity fmt

  let copy_file ~src ~dst ~executable ~kind:_ ~package:_ ~conf:_ =
    print_line
      "Copying %s to %s (executable: %b)"
      (Path.to_string_maybe_quoted src)
      (Path.to_string_maybe_quoted dst)
      executable;
    Fiber.return ()
  ;;

  let mkdir_p path = print_line "Creating directory %s" (Path.to_string_maybe_quoted path)

  let remove_file_if_exists path =
    print_line "Removing (if it exists) %s" (Path.to_string_maybe_quoted path)
  ;;

  let remove_dir_if_exists ~if_non_empty path =
    print_line
      "Removing directory (%s if not empty) %s"
      (match if_non_empty with
       | Fail -> "fail"
       | Warn -> "warn")
      (Path.to_string_maybe_quoted path)
  ;;
end

module File_ops_real (W : sig
    val verbosity : Dune_engine.Display.t
    val workspace : Workspace.t
  end) : File_operations = struct
  open W

  let print_line = print_line ~verbosity
  let get_vcs p = Source_tree.nearest_vcs p

  type copy_special_file_status =
    | Done
    | Use_plain_copy

  let with_ppf oc ~f =
    let ppf = Format.formatter_of_out_channel oc in
    f ppf;
    Format.pp_print_flush ppf ()
  ;;

  let copy_special_file ~src ~package ~ic ~oc ~f =
    let open Fiber.O in
    let get_version () =
      let* packages =
        match Package.Name.Map.find workspace.packages package with
        | None -> Fiber.return None
        | Some package -> Memo.run (get_vcs (Package.dir package))
      in
      match packages with
      | None -> Fiber.return None
      | Some vcs -> Memo.run (Vcs.describe vcs)
    in
    try f ~get_version ic ~src oc with
    | _ (* XXX should we really be catching everything here? *) ->
      User_warning.emit
        ~loc:(Loc.in_file src)
        [ Pp.text "Failed to parse file, not adding version and locations information." ];
      Fiber.return Use_plain_copy
  ;;

  let process_meta ~get_version ic ~src:_ oc =
    let module Meta = Dune_findlib.Findlib.Meta in
    let lb = Lexing.from_channel ic in
    let meta : Meta.t = { name = None; entries = Meta.parse_entries lb } in
    let need_more_versions =
      try
        let (_ : Meta.t) =
          Meta.add_versions meta ~get_version:(fun _ -> raise_notrace Exit)
        in
        false
      with
      | Exit -> true
    in
    if not need_more_versions
    then Fiber.return Use_plain_copy
    else
      let open Fiber.O in
      let+ version = get_version () in
      with_ppf oc ~f:(fun ppf ->
        let meta = Meta.add_versions meta ~get_version:(fun _ -> version) in
        Pp.to_fmt ppf (Meta.pp meta.entries));
      Done
  ;;

  let process_dune_package ~get_version ~get_location ic ~src oc =
    let lb = Lexing.from_channel ic in
    let dune_version = Dune_lang.Syntax.greatest_supported_version_exn Stanza.syntax in
    match Dune_package.Or_meta.parse src lb |> User_error.ok_exn with
    | Use_meta ->
      with_ppf oc ~f:(Dune_package.Or_meta.pp_use_meta ~dune_version);
      Fiber.return Done
    | Dune_package dp ->
      let open Fiber.O in
      (* replace sites with external path in the file *)
      let dp, replace_info = Dune_package.replace_site_sections ~get_location dp in
      (* replace version if needed in the file *)
      let need_version = Option.is_none dp.version in
      let+ dp =
        if need_version
        then
          let+ version_opt = get_version () in
          match version_opt with
          | Some version -> { dp with version = Some (Package_version.of_string version) }
          | None -> dp
        else Fiber.return dp
      in
      with_ppf oc ~f:(fun ppf ->
        (* CR-emillon: we should write absolute paths only if necessary *)
        Dune_package.Or_meta.pp
          ~dune_version
          ppf
          (Dune_package dp)
          ~encoding:(Absolute replace_info));
      Done
  ;;

  let copy_file
    ~src
    ~dst
    ~executable
    ~kind
    ~package
    ~(conf : Artifact_substitution.Conf.t)
    =
    let chmod = if executable then fun _ -> 0o755 else fun _ -> 0o644 in
    let plain_copy () = Io.copy_file ~chmod ~src ~dst () in
    match kind with
    | Substitute -> Artifact_substitution.copy_file ~conf ~src ~dst ~chmod ()
    | Special sf ->
      let open Fiber.O in
      let ic, oc = Io.setup_copy ~chmod ~src ~dst () in
      let+ status =
        Fiber.finalize
          ~finally:(fun () ->
            Io.close_both (ic, oc);
            Fiber.return ())
          (fun () ->
            let f =
              match sf with
              | META -> process_meta
              | Dune_package ->
                process_dune_package
                  ~get_location:(Artifact_substitution.Conf.get_location conf)
            in
            copy_special_file ~src ~package ~ic ~oc ~f)
      in
      (match status with
       | Done -> ()
       | Use_plain_copy -> plain_copy ())
  ;;

  let remove_file_if_exists dst =
    if Path.exists dst
    then (
      print_line "Deleting %s" (Path.to_string_maybe_quoted dst);
      print_unix_error (fun () -> Path.unlink_exn dst))
  ;;

  let remove_dir_if_exists ~if_non_empty dir =
    match Path.readdir_unsorted dir with
    | Error (Unix.ENOENT, _, _) -> ()
    | Ok [] ->
      print_line "Deleting empty directory %s" (Path.to_string_maybe_quoted dir);
      print_unix_error (fun () -> Path.rmdir dir)
    | Error (e, _, _) ->
      User_message.prerr (User_error.make [ Pp.text (Unix.error_message e) ])
    | _ ->
      let dir = Path.to_string_maybe_quoted dir in
      (match if_non_empty with
       | Warn ->
         User_message.prerr
           (User_error.make
              [ Pp.textf "Directory %s is not empty, cannot delete (ignoring)." dir ])
       | Fail ->
         User_error.raise
           [ Pp.textf "Please delete non-empty directory %s manually." dir ])
  ;;

  let mkdir_p p =
    (* CR-someday amokhov: We should really change [Path.mkdir_p dir] to fail if
       it turns out that [dir] exists and is not a directory. Even better, make
       [Path.mkdir_p] return an explicit variant to deal with. *)
    match Fpath.mkdir_p (Path.to_string p) with
    | Created -> ()
    | Already_exists ->
      (match Path.is_directory p with
       | true -> ()
       | false ->
         User_error.raise
           [ Pp.textf "Please delete file %s manually." (Path.to_string_maybe_quoted p) ])
  ;;
end

module Sections = struct
  type t =
    | All
    | Only of Section.Set.t

  let sections_conv =
    let all =
      Section.all
      |> Section.Set.to_list
      |> List.map ~f:(fun section -> Section.to_string section, section)
    in
    Arg.list ~sep:',' (Arg.enum all)
  ;;

  let term =
    let doc = "sections that should be installed" in
    let open Cmdliner.Arg in
    let+ sections = value & opt (some sections_conv) None & info [ "sections" ] ~doc in
    match sections with
    | None -> All
    | Some sections -> Only (Section.Set.of_list sections)
  ;;

  let should_install t section =
    match t with
    | All -> true
    | Only set -> Section.Set.mem set section
  ;;
end

let file_operations ~verbosity ~dry_run ~workspace : (module File_operations) =
  if dry_run
  then
    (module File_ops_dry_run (struct
        let verbosity = verbosity
      end))
  else
    (module File_ops_real (struct
        let workspace = workspace
        let verbosity = verbosity
      end))
;;

let package_is_vendored (pkg : Package.t) =
  let dir = Package.dir pkg in
  Memo.run (Source_tree.is_vendored dir)
;;

type what =
  | Install
  | Uninstall

let pp_what fmt = function
  | Install -> Format.pp_print_string fmt "Install"
  | Uninstall -> Format.pp_print_string fmt "Uninstall"
;;

let cmd_what = function
  | Install -> "install"
  | Uninstall -> "uninstall"
;;

let install_entry
  ~ops
  ~conf
  ~package
  ~dir
  ~create_install_files
  (entry : Path.t Install.Entry.t)
  ~dst
  ~verbosity
  =
  let module Ops = (val ops : File_operations) in
  let open Fiber.O in
  let special_file = Special_file.of_entry entry in
  (match special_file with
   | _ when not create_install_files -> Fiber.return true
   | Some Special_file.META | Some Special_file.Dune_package -> Fiber.return true
   | None ->
     Artifact_substitution.test_file ~src:entry.src ()
     >>| (function
      | Some_substitution -> true
      | No_substitution -> false))
  >>= function
  | false -> Fiber.return entry
  | true ->
    let+ () =
      (match Path.is_directory dst with
       | true -> Ops.remove_dir_if_exists ~if_non_empty:Fail dst
       | false -> Ops.remove_file_if_exists dst);
      print_line
        ~verbosity
        "%s %s"
        (if create_install_files then "Copying to" else "Installing")
        (Path.to_string_maybe_quoted dst);
      Ops.mkdir_p dir;
      let executable = Section.should_set_executable_bit entry.section in
      let kind =
        match special_file with
        | Some special -> Special special
        | None ->
          (* CR-emillon: for most cases we could use a fast copy here, but some
             kinds of files do need artifact substitution(at least
             executable files and artifacts built from generated sites
             modules), but it's too late to know without reading the file. *)
          Substitute
      in
      Ops.copy_file ~src:entry.src ~dst ~executable ~kind ~package ~conf
    in
    Install.Entry.set_src entry dst
;;

let run
  what
  context
  common
  pkgs
  sections
  (config : Dune_config.t)
  ~dry_run
  ~destdir
  ~relocatable
  ~create_install_files
  ~prefix_from_command_line
  ~(from_command_line : _ Install.Roots.t)
  =
  let open Fiber.O in
  let* workspace = Workspace.get () in
  let contexts =
    match context with
    | None ->
      (match Common.x common with
       | Some findlib_toolchain ->
         let contexts =
           List.filter workspace.contexts ~f:(fun (ctx : Context.t) ->
             match Context.findlib_toolchain ctx with
             | None -> false
             | Some ctx_findlib_toolchain ->
               Dune_engine.Context_name.equal ctx_findlib_toolchain findlib_toolchain)
         in
         contexts
       | None -> workspace.contexts)
    | Some name ->
      (match
         List.find workspace.contexts ~f:(fun c ->
           Dune_engine.Context_name.equal (Context.name c) name)
       with
       | Some ctx -> [ ctx ]
       | None ->
         User_error.raise
           [ Pp.textf "Context %S not found!" (Dune_engine.Context_name.to_string name) ])
  in
  let* pkgs =
    match pkgs with
    | _ :: _ -> Fiber.return pkgs
    | [] ->
      Package.Name.Map.values workspace.packages
      |> Fiber.parallel_map ~f:(fun pkg ->
        package_is_vendored pkg
        >>| function
        | true -> None
        | false -> Some (Package.name pkg))
      >>| List.filter_opt
  in
  let install_files, missing_install_files =
    List.concat_map pkgs ~f:(fun pkg ->
      List.map contexts ~f:(fun (ctx : Context.t) ->
        let fn =
          let fn =
            resolve_package_install
              workspace
              ~findlib_toolchain:(Context.findlib_toolchain ctx)
              pkg
          in
          Path.append_source (Path.build (Context.build_dir ctx)) fn
        in
        if Path.exists fn then Left (ctx, (pkg, fn)) else Right fn))
    |> List.partition_map ~f:Fun.id
  in
  if missing_install_files <> []
  then
    User_error.raise
      [ Pp.textf "The following <package>.install are missing:"
      ; Pp.enumerate missing_install_files ~f:(fun p -> Pp.text (Path.to_string p))
      ]
      ~hints:
        [ Pp.concat
            ~sep:Pp.space
            [ Pp.text "try running"
            ; User_message.command "dune build [-p <pkg>] @install"
            ]
          |> Pp.hovbox
        ];
  (match contexts, prefix_from_command_line, from_command_line.lib_root with
   | _ :: _ :: _, Some _, _ | _ :: _ :: _, _, Some _ ->
     User_error.raise
       [ Pp.concat
           ~sep:Pp.space
           [ Pp.text "Cannot specify"
           ; User_message.command "--prefix"
           ; Pp.text "or"
           ; User_message.command "--libdir"
           ; Pp.text "when installing into multiple contexts!"
           ]
       ]
   | _ -> ());
  let install_files_by_context =
    let module CMap = Map.Make (Context) in
    CMap.of_list_multi install_files
    |> CMap.to_list_map ~f:(fun context install_files ->
      let entries_per_package =
        List.map install_files ~f:(fun (package, install_file) ->
          let entries =
            Install.Entry.load_install_file install_file Path.of_local
            |> List.filter ~f:(fun (entry : Path.t Install.Entry.t) ->
              Sections.should_install sections entry.section)
          in
          match
            List.filter_map entries ~f:(fun entry ->
              (* CR rgrinberg: this is ignoring optional entries *)
              Option.some_if (not (Path.exists entry.src)) entry.src)
          with
          | [] -> package, entries
          | missing_files ->
            User_error.raise
              [ Pp.textf
                  "The following files which are listed in %s cannot be installed \
                   because they do not exist:"
                  (Path.to_string_maybe_quoted install_file)
              ; Pp.enumerate missing_files ~f:(fun p ->
                  Pp.verbatim (Path.to_string_maybe_quoted p))
              ])
      in
      context, entries_per_package)
  in
  let destdir =
    Option.map
      ~f:Path.of_string
      (if create_install_files
       then
         (* CR-rgrinberg: why are we silently ignoring an argument instead
            of erroring given that they mutually exclusive? *)
         Some (Option.value ~default:"_destdir" destdir)
       else destdir)
  in
  let relocatable =
    if relocatable
    then (
      match prefix_from_command_line with
      | Some dir -> Some (Path.of_string dir)
      | None ->
        User_error.raise
          [ Pp.concat
              ~sep:Pp.space
              [ Pp.text "Option"
              ; User_message.command "--prefix"
              ; Pp.text "is needed with"
              ; User_message.command "--relocation"
              ]
            |> Pp.hovbox
          ])
    else None
  in
  let verbosity =
    match config.display with
    | Simple display -> display.verbosity
    | Tui -> Quiet
  in
  let open Fiber.O in
  let (module Ops) = file_operations ~verbosity ~dry_run ~workspace in
  let files_deleted_in = ref Path.Set.empty in
  let+ () =
    Fiber.sequential_iter
      install_files_by_context
      ~f:(fun (context, entries_per_package) ->
        let* roots = get_dirs context ~prefix_from_command_line ~from_command_line in
        let conf = Artifact_substitution.Conf.of_install ~relocatable ~roots ~context in
        Fiber.sequential_iter entries_per_package ~f:(fun (package, entries) ->
          let+ entries =
            (* CR rgrinberg: why don't we install things concurrently? *)
            Fiber.sequential_map entries ~f:(fun entry ->
              let dst =
                let paths = Install.Paths.make ~relative:Path.relative ~package ~roots in
                Install.Entry.relative_installed_path entry ~paths
                |> interpret_destdir ~destdir
              in
              let dir = Path.parent_exn dst in
              match what with
              | Uninstall ->
                Ops.remove_file_if_exists dst;
                files_deleted_in := Path.Set.add !files_deleted_in dir;
                Fiber.return entry
              | Install ->
                install_entry
                  ~ops:(module Ops)
                  ~conf
                  ~package
                  ~dir
                  ~create_install_files
                  ~dst
                  ~verbosity
                  entry)
          in
          if create_install_files
          then (
            let fn =
              resolve_package_install
                workspace
                ~findlib_toolchain:(Context.findlib_toolchain context)
                package
            in
            Install.Entry.gen_install_file entries |> Io.write_file (Path.source fn))))
  in
  Path.Set.to_list !files_deleted_in
  (* This [List.rev] is to ensure we process children directories before
     their parents *)
  |> List.rev
  |> List.iter ~f:(Ops.remove_dir_if_exists ~if_non_empty:Warn)
;;

let make ~what =
  let doc = Format.asprintf "%a packages defined in the workspace." pp_what what in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let absolute_path =
    Arg.conv'
      ( (fun path ->
          if Filename.is_relative path
          then Error "the path must be absolute to avoid ambiguity"
          else Ok path)
      , Arg.conv_printer Arg.string )
  in
  let term =
    let+ builder = Common.Builder.term
    and+ prefix_from_command_line =
      Arg.(
        value
        & opt (some string) None
        & info
            [ "prefix" ]
            ~env:(Cmd.Env.info "DUNE_INSTALL_PREFIX")
            ~docv:"PREFIX"
            ~doc:
              "Directory where files are copied. For instance binaries are copied into \
               $(i,\\$prefix/bin), library files into $(i,\\$prefix/lib), etc...")
    and+ destdir =
      Arg.(
        value
        & opt (some string) None
        & info
            [ "destdir" ]
            ~env:(Cmd.Env.info "DESTDIR")
            ~docv:"PATH"
            ~doc:"This directory is prepended to all installed paths.")
    and+ libdir_from_command_line =
      Arg.(
        value
        & opt (some absolute_path) None
        & info
            [ "libdir" ]
            ~docv:"PATH"
            ~doc:
              "Directory where library files are copied, relative to $(b,prefix) or \
               absolute. If $(b,--prefix) is specified the default is \
               $(i,\\$prefix/lib). Only absolute path accepted.")
    and+ mandir_from_command_line =
      let doc =
        "Manually override the directory to install man pages. Only absolute path \
         accepted."
      in
      Arg.(value & opt (some absolute_path) None & info [ "mandir" ] ~docv:"PATH" ~doc)
    and+ docdir_from_command_line =
      let doc =
        "Manually override the directory to install documentation files. Only absolute \
         path accepted."
      in
      Arg.(value & opt (some absolute_path) None & info [ "docdir" ] ~docv:"PATH" ~doc)
    and+ etcdir_from_command_line =
      let doc =
        "Manually override the directory to install configuration files. Only absolute \
         path accepted."
      in
      Arg.(value & opt (some absolute_path) None & info [ "etcdir" ] ~docv:"PATH" ~doc)
    and+ bindir_from_command_line =
      let doc =
        "Manually override the directory to install public binaries. Only absolute path \
         accepted."
      in
      Arg.(value & opt (some absolute_path) None & info [ "bindir" ] ~docv:"PATH" ~doc)
    and+ sbindir_from_command_line =
      let doc =
        "Manually override the directory to install files from sbin section. Only \
         absolute path accepted."
      in
      Arg.(value & opt (some absolute_path) None & info [ "sbindir" ] ~docv:"PATH" ~doc)
    and+ datadir_from_command_line =
      let doc =
        "Manually override the directory to install files from share section. Only \
         absolute path accepted."
      in
      Arg.(value & opt (some absolute_path) None & info [ "datadir" ] ~docv:"PATH" ~doc)
    and+ libexecdir_from_command_line =
      let doc =
        "Manually override the directory to install executable library files. Only \
         absolute path accepted."
      in
      Arg.(
        value & opt (some absolute_path) None & info [ "libexecdir" ] ~docv:"PATH" ~doc)
    and+ dry_run =
      Arg.(
        value
        & flag
        & info
            [ "dry-run" ]
            ~doc:"Only display the file operations that would be performed.")
    and+ relocatable =
      Arg.(
        value
        & flag
        & info
            [ "relocatable" ]
            ~doc:
              "Make the binaries relocatable (the installation directory can be moved). \
               The installation directory must be specified with --prefix")
    and+ create_install_files =
      Arg.(
        value
        & flag
        & info
            [ "create-install-files" ]
            ~doc:
              "Do not directly install, but create install files in the root directory \
               and create substituted files if needed in destdir (_destdir by default).")
    and+ pkgs = Arg.(value & pos_all package_name [] name_)
    and+ context =
      Arg.(
        value
        & opt (some Arg.context_name) None
        & info
            [ "context" ]
            ~docv:"CONTEXT"
            ~doc:
              "Select context to install from. By default, install files from all \
               defined contexts.")
    and+ sections = Sections.term in
    let builder = Common.Builder.forbid_builds builder in
    let builder = Common.Builder.disable_log_file builder in
    let common, config = Common.init builder in
    Scheduler.go ~common ~config (fun () ->
      let from_command_line =
        { Install.Roots.lib_root = libdir_from_command_line
        ; etc_root = etcdir_from_command_line
        ; doc_root = docdir_from_command_line
        ; man = mandir_from_command_line
        ; bin = bindir_from_command_line
        ; sbin = sbindir_from_command_line
        ; libexec_root = libexecdir_from_command_line
        ; share_root = datadir_from_command_line
        }
        |> Install.Roots.map ~f:(Option.map ~f:Path.of_string)
        |> Install.Roots.complete
      in
      run
        what
        context
        common
        pkgs
        sections
        config
        ~dry_run
        ~destdir
        ~relocatable
        ~create_install_files
        ~prefix_from_command_line
        ~from_command_line)
  in
  Cmd.v
    (Cmd.info
       (cmd_what what)
       ~doc
       ~man:Manpage.(`S s_synopsis :: (synopsis @ Common.help_secs)))
    term
;;

let install = make ~what:Install
let uninstall = make ~what:Uninstall

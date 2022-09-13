open Stdune
open Import

let synopsis =
  [ `P "The installation directories used are defined by priority:"
  ; `Noblank
  ; `P
      "- directories set on the command line of $(i,dune install), or \
       corresponding environment variables"
  ; `Noblank
  ; `P
      "- directories set in dune binary. They are setup before the compilation \
       of dune with $(i,./configure)"
  ; `Noblank
  ; `P
      "- inferred from the environment variable $(i,OPAM_SWITCH_PREFIX) if \
       present"
  ]

let print_line fmt =
  Printf.ksprintf (fun s -> Console.print [ Pp.verbatim s ]) fmt

let interpret_destdir ~destdir path =
  match destdir with
  | None -> path
  | Some destdir -> Path.append_local destdir (Path.local_part path)

let get_dirs context ~prefix_from_command_line ~from_command_line =
  let module Roots = Install.Section.Paths.Roots in
  let prefix_from_command_line =
    Option.map ~f:Path.of_string prefix_from_command_line
  in
  let roots =
    match prefix_from_command_line with
    | Some prefix ->
      Roots.opam_from_prefix prefix |> Roots.map ~f:(fun s -> Some s)
    | None -> Context.roots context
  in
  let roots = Roots.first_has_priority from_command_line roots in
  let must_be_defined name v =
    match v with
    | Some v -> v
    | None ->
      User_error.raise
        [ Pp.textf "The %s installation directory is unknown." name ]
        ~hints:[ Pp.textf "It could be specified with --%s" name ]
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

module Workspace = struct
  type t =
    { packages : Package.t Package.Name.Map.t
    ; contexts : Context.t list
    }

  let get () =
    let open Memo.O in
    Memo.run
      (let+ conf = Dune_rules.Dune_load.load ()
       and+ contexts = Context.DB.all () in
       { packages = conf.packages; contexts })

  let package_install_file t pkg =
    match Package.Name.Map.find t.packages pkg with
    | None -> Error ()
    | Some p ->
      let name = Package.name p in
      let dir = Package.dir p in
      Ok
        (Path.Source.relative dir
           (Dune_rules.Install_rules.install_file ~package:name
              ~findlib_toolchain:None))
end

let resolve_package_install workspace pkg =
  match Workspace.package_install_file workspace pkg with
  | Ok path -> path
  | Error () ->
    let pkg = Package.Name.to_string pkg in
    User_error.raise
      [ Pp.textf "Unknown package %s!" pkg ]
      ~hints:
        (User_message.did_you_mean pkg
           ~candidates:
             (Package.Name.Map.keys workspace.packages
             |> List.map ~f:Package.Name.to_string))

let print_unix_error f =
  try f ()
  with Unix.Unix_error (error, syscall, arg) ->
    let error = Unix_error.Detailed.create error ~syscall ~arg in
    User_message.prerr (User_error.make [ Unix_error.Detailed.pp error ])

module Special_file = struct
  type t =
    | META
    | Dune_package

  let of_entry (e : _ Install.Entry.t) =
    match e.section with
    | Lib ->
      let dst = Install.Dst.to_string e.dst in
      if dst = Findlib.meta_fn then Some META
      else if dst = Dune_package.fn then Some Dune_package
      else None
    | _ -> None
end

type rmdir_mode =
  | Fail
  | Warn

(** Operations that act on real files or just pretend to (for --dry-run) *)
module type File_operations = sig
  val copy_file :
       src:Path.t
    -> dst:Path.t
    -> executable:bool
    -> special_file:Special_file.t option
    -> package:Package.Name.t
    -> conf:Dune_rules.Artifact_substitution.conf
    -> unit Fiber.t

  val mkdir_p : Path.t -> unit

  val remove_file_if_exists : Path.t -> unit

  val remove_dir_if_exists : if_non_empty:rmdir_mode -> Path.t -> unit
end

module type Workspace = sig
  val workspace : Workspace.t
end

module File_ops_dry_run : File_operations = struct
  let copy_file ~src ~dst ~executable ~special_file:_ ~package:_ ~conf:_ =
    print_line "Copying %s to %s (executable: %b)"
      (Path.to_string_maybe_quoted src)
      (Path.to_string_maybe_quoted dst)
      executable;
    Fiber.return ()

  let mkdir_p path =
    print_line "Creating directory %s" (Path.to_string_maybe_quoted path)

  let remove_file_if_exists path =
    print_line "Removing (if it exists) %s" (Path.to_string_maybe_quoted path)

  let remove_dir_if_exists ~if_non_empty path =
    print_line "Removing directory (%s if not empty) %s"
      (match if_non_empty with
      | Fail -> "fail"
      | Warn -> "warn")
      (Path.to_string_maybe_quoted path)
end

module File_ops_real (W : Workspace) : File_operations = struct
  open W

  let get_vcs p = Dune_engine.Source_tree.nearest_vcs p

  type load_special_file_result =
    { need_version : bool
    ; callback : ?version:string -> Format.formatter -> unit
    }

  let copy_special_file ~src ~package ~ic ~oc ~f =
    let open Fiber.O in
    let plain_copy () =
      seek_in ic 0;
      Io.copy_channels ic oc;
      Fiber.return ()
    in

    match f ic with
    | Some { need_version; callback } ->
      let* version =
        if need_version then
          let* packages =
            match Package.Name.Map.find workspace.packages package with
            | None -> Fiber.return None
            | Some package -> Memo.run (get_vcs (Package.dir package))
          in
          match packages with
          | None -> Fiber.return None
          | Some vcs -> Memo.run (Dune_engine.Vcs.describe vcs)
        else Fiber.return None
      in
      let ppf = Format.formatter_of_out_channel oc in
      callback ppf ?version;
      Format.pp_print_flush ppf ();
      Fiber.return ()
    | None -> plain_copy ()
    (* XXX should we really be catching everything here? *)
    | exception _ ->
      User_warning.emit ~loc:(Loc.in_file src)
        [ Pp.text
            "Failed to parse file, not adding version and locations \
             information."
        ];
      plain_copy ()

  let process_meta ic =
    let lb = Lexing.from_channel ic in
    let meta : Dune_rules.Meta.t =
      { name = None; entries = Dune_rules.Meta.parse_entries lb }
    in
    let need_more_versions =
      try
        let (_ : Dune_rules.Meta.t) =
          Dune_rules.Meta.add_versions meta ~get_version:(fun _ ->
              raise_notrace Exit)
        in
        false
      with Exit -> true
    in
    if not need_more_versions then None
    else
      let callback ?version ppf =
        let meta =
          Dune_rules.Meta.add_versions meta ~get_version:(fun _ -> version)
        in
        Pp.to_fmt ppf (Dune_rules.Meta.pp meta.entries)
      in
      Some { need_version = true; callback }

  let replace_sites
      ~(get_location : Dune_engine.Section.t -> Package.Name.t -> Stdune.Path.t)
      dp =
    match
      List.find_map dp ~f:(function
        | Dune_lang.List [ Atom (A "name"); Atom (A name) ] -> Some name
        | _ -> None)
    with
    | None -> dp
    | Some name ->
      List.map dp ~f:(function
        | Dune_lang.List ((Atom (A "sections") as sexp_sections) :: sections) ->
          let sections =
            List.map sections ~f:(function
              | Dune_lang.List [ (Atom (A section) as section_sexp); _ ] ->
                let path =
                  get_location
                    (Option.value_exn (Section.of_string section))
                    (Package.Name.of_string name)
                in
                let open Dune_lang.Encoder in
                pair sexp string (section_sexp, Path.to_absolute_filename path)
              | _ -> assert false)
          in
          Dune_lang.List (sexp_sections :: sections)
        | x -> x)

  let process_dune_package ~get_location ic =
    let lb = Lexing.from_channel ic in
    let dp =
      Dune_lang.Parser.parse ~mode:Many lb
      |> List.map ~f:Dune_lang.Ast.remove_locs
    in
    (* replace sites with external path in the file *)
    let dp = replace_sites ~get_location dp in
    (* replace version if needed in the file *)
    let need_version =
      not
        (List.exists dp ~f:(function
          | Dune_lang.List (Atom (A "version") :: _)
          | Dune_lang.List [ Atom (A "use_meta"); Atom (A "true") ]
          | Dune_lang.List [ Atom (A "use_meta") ] -> true
          | _ -> false))
    in
    let callback ?version ppf =
      let dp =
        match version with
        | Some version -> (
          let version =
            Dune_lang.List
              [ Dune_lang.atom "version"
              ; Dune_lang.atom_or_quoted_string version
              ]
          in
          match dp with
          | lang :: name :: rest -> lang :: name :: version :: rest
          | [ lang ] -> [ lang; version ]
          | [] -> [ version ])
        | _ -> dp
      in
      Format.pp_open_vbox ppf 0;
      List.iter dp ~f:(fun x ->
          Dune_lang.Deprecated.pp ppf x;
          Format.pp_print_cut ppf ());
      Format.pp_close_box ppf ()
    in
    Some { need_version; callback }

  let copy_file ~src ~dst ~executable ~special_file ~package
      ~(conf : Dune_rules.Artifact_substitution.conf) =
    let chmod = if executable then fun _ -> 0o755 else fun _ -> 0o644 in
    match (special_file : Special_file.t option) with
    | Some sf ->
      let ic, oc = Io.setup_copy ~chmod ~src ~dst () in
      Fiber.finalize
        ~finally:(fun () ->
          Io.close_both (ic, oc);
          Fiber.return ())
        (fun () ->
          let f =
            match sf with
            | META -> process_meta
            | Dune_package ->
              process_dune_package ~get_location:conf.get_location
          in
          copy_special_file ~src ~package ~ic ~oc ~f)
    | None ->
      Dune_rules.Artifact_substitution.copy_file ~conf ~src ~dst ~chmod ()

  let remove_file_if_exists dst =
    if Path.exists dst then (
      print_line "Deleting %s" (Path.to_string_maybe_quoted dst);
      print_unix_error (fun () -> Path.unlink dst))

  let remove_dir_if_exists ~if_non_empty dir =
    if Path.exists dir then
      match Path.readdir_unsorted dir with
      | Ok [] ->
        print_line "Deleting empty directory %s"
          (Path.to_string_maybe_quoted dir);
        print_unix_error (fun () -> Path.rmdir dir)
      | Error (e, _, _) ->
        User_message.prerr (User_error.make [ Pp.text (Unix.error_message e) ])
      | _ -> (
        let dir = Path.to_string_maybe_quoted dir in
        match if_non_empty with
        | Warn ->
          User_message.prerr
            (User_error.make
               [ Pp.textf "Directory %s is not empty, cannot delete (ignoring)."
                   dir
               ])
        | Fail ->
          User_error.raise
            [ Pp.textf "Please delete non-empty directory %s manually." dir ])

  let mkdir_p p =
    (* CR-someday amokhov: We should really change [Path.mkdir_p dir] to fail if
       it turns out that [dir] exists and is not a directory. Even better, make
       [Path.mkdir_p] return an explicit variant to deal with. *)
    match Fpath.mkdir_p (Path.to_string p) with
    | Created -> ()
    | Already_exists -> (
      match Path.is_directory p with
      | true -> ()
      | false ->
        User_error.raise
          [ Pp.textf "Please delete file %s manually."
              (Path.to_string_maybe_quoted p)
          ])
end

module Sections = struct
  type t =
    | All
    | Only of Section.Set.t

  let sections_conv : Section.t list Cmdliner.Arg.converter =
    let all =
      Section.all |> Section.Set.to_list
      |> List.map ~f:(fun section -> (Section.to_string section, section))
    in
    Arg.list ~sep:',' (Arg.enum all)

  let term =
    let doc = "sections that should be installed" in
    let open Cmdliner.Arg in
    let+ sections =
      value & opt (some sections_conv) None & info [ "sections" ] ~doc
    in
    match sections with
    | None -> All
    | Some sections -> Only (Section.Set.of_list sections)

  let should_install t section =
    match t with
    | All -> true
    | Only set -> Section.Set.mem set section
end

let file_operations ~dry_run ~workspace : (module File_operations) =
  if dry_run then (module File_ops_dry_run)
  else
    (module File_ops_real (struct
      let workspace = workspace
    end))

let package_is_vendored (pkg : Dune_engine.Package.t) =
  let dir = Package.dir pkg in
  Memo.run (Dune_engine.Source_tree.is_vendored dir)

type what =
  | Install
  | Uninstall

let pp_what fmt = function
  | Install -> Format.pp_print_string fmt "Install"
  | Uninstall -> Format.pp_print_string fmt "Uninstall"

let cmd_what = function
  | Install -> "install"
  | Uninstall -> "uninstall"

let install_uninstall ~what =
  let doc = Format.asprintf "%a packages." pp_what what in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let absolute_path =
    ( (fun path ->
        if Filename.is_relative path then
          `Error "the path must be absolute to avoid ambiguity"
        else `Ok path)
    , snd Arg.string )
  in
  let term =
    let+ common = Common.term
    and+ prefix_from_command_line =
      Arg.(
        value
        & opt (some string) None
        & info [ "prefix" ]
            ~env:(env_var "DUNE_INSTALL_PREFIX")
            ~docv:"PREFIX"
            ~doc:
              "Directory where files are copied. For instance binaries are \
               copied into $(i,\\$prefix/bin), library files into \
               $(i,\\$prefix/lib), etc...")
    and+ destdir =
      Arg.(
        value
        & opt (some string) None
        & info [ "destdir" ] ~env:(env_var "DESTDIR") ~docv:"PATH"
            ~doc:"This directory is prepended to all installed paths.")
    and+ libdir_from_command_line =
      Arg.(
        value
        & opt (some absolute_path) None
        & info [ "libdir" ] ~docv:"PATH"
            ~doc:
              "Directory where library files are copied, relative to \
               $(b,prefix) or absolute. If $(b,--prefix) is specified the \
               default is $(i,\\$prefix/lib). Only absolute path accepted.")
    and+ mandir_from_command_line =
      let doc =
        "Manually override the directory to install man pages. Only absolute \
         path accepted."
      in
      Arg.(
        value
        & opt (some absolute_path) None
        & info [ "mandir" ] ~docv:"PATH" ~doc)
    and+ docdir_from_command_line =
      let doc =
        "Manually override the directory to install documentation files. Only \
         absolute path accepted."
      in
      Arg.(
        value
        & opt (some absolute_path) None
        & info [ "docdir" ] ~docv:"PATH" ~doc)
    and+ etcdir_from_command_line =
      let doc =
        "Manually override the directory to install configuration files. Only \
         absolute path accepted."
      in
      Arg.(
        value
        & opt (some absolute_path) None
        & info [ "etcdir" ] ~docv:"PATH" ~doc)
    and+ bindir_from_command_line =
      let doc =
        "Manually override the directory to install public binaries. Only \
         absolute path accepted."
      in
      Arg.(
        value
        & opt (some absolute_path) None
        & info [ "bindir" ] ~docv:"PATH" ~doc)
    and+ sbindir_from_command_line =
      let doc =
        "Manually override the directory to install files from sbin section. \
         Only absolute path accepted."
      in
      Arg.(
        value
        & opt (some absolute_path) None
        & info [ "sbindir" ] ~docv:"PATH" ~doc)
    and+ datadir_from_command_line =
      let doc =
        "Manually override the directory to install files from share section. \
         Only absolute path accepted."
      in
      Arg.(
        value
        & opt (some absolute_path) None
        & info [ "datadir" ] ~docv:"PATH" ~doc)
    and+ libexecdir_from_command_line =
      let doc =
        "Manually override the directory to install executable library files. \
         Only absolute path accepted."
      in
      Arg.(
        value
        & opt (some absolute_path) None
        & info [ "libexecdir" ] ~docv:"PATH" ~doc)
    and+ dry_run =
      Arg.(
        value & flag
        & info [ "dry-run" ]
            ~doc:"Only display the file operations that would be performed.")
    and+ relocatable =
      Arg.(
        value & flag
        & info [ "relocatable" ]
            ~doc:
              "Make the binaries relocatable (the installation directory can \
               be moved). The installation directory must be specified with \
               --prefix")
    and+ create_install_files =
      Arg.(
        value & flag
        & info [ "create-install-files" ]
            ~doc:
              "Do not directly install, but create install files in the root \
               directory and create substituted files if needed in destdir \
               (_destdir by default).")
    and+ pkgs = Arg.(value & pos_all package_name [] name_)
    and+ context =
      Arg.(
        value
        & opt (some Arg.context_name) None
        & info [ "context" ] ~docv:"CONTEXT"
            ~doc:
              "Select context to install from. By default, install files from \
               all defined contexts.")
    and+ sections = Sections.term in
    let config = Common.init ~log_file:No_log_file common in
    Scheduler.go ~common ~config (fun () ->
        let open Fiber.O in
        let* workspace = Workspace.get () in
        let contexts =
          match context with
          | None -> workspace.contexts
          | Some name -> (
            match
              List.find workspace.contexts ~f:(fun c ->
                  Dune_engine.Context_name.equal c.name name)
            with
            | Some ctx -> [ ctx ]
            | None ->
              User_error.raise
                [ Pp.textf "Context %S not found!"
                    (Dune_engine.Context_name.to_string name)
                ])
        in
        let* pkgs =
          match pkgs with
          | [] ->
            Fiber.parallel_map (Package.Name.Map.values workspace.packages)
              ~f:(fun pkg ->
                package_is_vendored pkg >>| function
                | true -> None
                | false -> Some (Package.name pkg))
            >>| List.filter_opt
          | l -> Fiber.return l
        in
        let install_files, missing_install_files =
          List.concat_map pkgs ~f:(fun pkg ->
              let fn = resolve_package_install workspace pkg in
              List.map contexts ~f:(fun ctx ->
                  let fn =
                    Path.append_source (Path.build ctx.Context.build_dir) fn
                  in
                  if Path.exists fn then Left (ctx, (pkg, fn)) else Right fn))
          |> List.partition_map ~f:Fun.id
        in
        if missing_install_files <> [] then
          User_error.raise
            [ Pp.textf "The following <package>.install are missing:"
            ; Pp.enumerate missing_install_files ~f:(fun p ->
                  Pp.text (Path.to_string p))
            ]
            ~hints:[ Pp.text "try running: dune build [-p <pkg>] @install" ];
        (match
           (contexts, prefix_from_command_line, libdir_from_command_line)
         with
        | _ :: _ :: _, Some _, _ | _ :: _ :: _, _, Some _ ->
          User_error.raise
            [ Pp.text
                "Cannot specify --prefix or --libdir when installing into \
                 multiple contexts!"
            ]
        | _ -> ());
        let install_files_by_context =
          let module CMap = Map.Make (Context) in
          CMap.of_list_multi install_files
          |> CMap.to_list_map ~f:(fun context install_files ->
                 let entries_per_package =
                   List.map install_files ~f:(fun (package, install_file) ->
                       let entries = Install.load_install_file install_file in
                       let entries =
                         List.filter entries
                           ~f:(fun (entry : Path.t Install.Entry.t) ->
                             Sections.should_install sections entry.section)
                       in
                       match
                         List.filter_map entries ~f:(fun entry ->
                             Option.some_if
                               (not (Path.exists entry.src))
                               entry.src)
                       with
                       | [] -> (package, entries)
                       | missing_files ->
                         User_error.raise
                           [ Pp.textf
                               "The following files which are listed in %s \
                                cannot be installed because they do not exist:"
                               (Path.to_string_maybe_quoted install_file)
                           ; Pp.enumerate missing_files ~f:(fun p ->
                                 Pp.verbatim (Path.to_string_maybe_quoted p))
                           ])
                 in
                 (context, entries_per_package))
        in
        let destdir =
          Option.map ~f:Path.of_string
            (if create_install_files then
             Some (Option.value ~default:"_destdir" destdir)
            else destdir)
        in
        let relocatable =
          if relocatable then
            match prefix_from_command_line with
            | Some dir -> Some (Path.of_string dir)
            | None ->
              User_error.raise
                [ Pp.text "Option --prefix is needed with --relocation" ]
          else None
        in

        let open Fiber.O in
        let (module Ops) = file_operations ~dry_run ~workspace in
        let files_deleted_in = ref Path.Set.empty in
        let from_command_line =
          let open Install.Section.Paths.Roots in
          { lib_root = libdir_from_command_line
          ; etc_root = etcdir_from_command_line
          ; doc_root = docdir_from_command_line
          ; man = mandir_from_command_line
          ; bin = bindir_from_command_line
          ; sbin = sbindir_from_command_line
          ; libexec_root = libexecdir_from_command_line
          ; share_root = datadir_from_command_line
          }
          |> map ~f:(Option.map ~f:Path.of_string)
          |> complete
        in
        let+ () =
          Fiber.sequential_iter install_files_by_context
            ~f:(fun (context, entries_per_package) ->
              let roots =
                get_dirs context ~prefix_from_command_line ~from_command_line
              in
              let conf =
                Dune_rules.Artifact_substitution.conf_for_install ~relocatable
                  ~default_ocamlpath:context.default_ocamlpath
                  ~stdlib_dir:context.stdlib_dir ~roots
              in
              Fiber.sequential_iter entries_per_package
                ~f:(fun (package, entries) ->
                  let paths = Install.Section.Paths.make ~package ~roots in
                  let+ entries =
                    Fiber.sequential_map entries ~f:(fun entry ->
                        let special_file = Special_file.of_entry entry in
                        let dst =
                          Install.Entry.relative_installed_path entry ~paths
                          |> interpret_destdir ~destdir
                        in
                        let dir = Path.parent_exn dst in
                        match what with
                        | Install ->
                          let* copy =
                            match special_file with
                            | _ when not create_install_files ->
                              Fiber.return true
                            | None ->
                              Dune_rules.Artifact_substitution.test_file
                                ~src:entry.src ()
                            | Some Special_file.META
                            | Some Special_file.Dune_package ->
                              Fiber.return true
                          in
                          let msg =
                            if create_install_files then "Copying to"
                            else "Installing"
                          in
                          if copy then
                            let* () =
                              (match Path.is_directory dst with
                              | true ->
                                Ops.remove_dir_if_exists ~if_non_empty:Fail dst
                              | false -> Ops.remove_file_if_exists dst);
                              print_line "%s %s" msg
                                (Path.to_string_maybe_quoted dst);
                              Ops.mkdir_p dir;
                              let executable =
                                Section.should_set_executable_bit entry.section
                              in
                              Ops.copy_file ~src:entry.src ~dst ~executable
                                ~special_file ~package ~conf
                            in
                            Fiber.return (Install.Entry.set_src entry dst)
                          else Fiber.return entry
                        | Uninstall ->
                          Ops.remove_file_if_exists dst;
                          files_deleted_in := Path.Set.add !files_deleted_in dir;
                          Fiber.return entry)
                  in
                  if create_install_files then
                    let fn = resolve_package_install workspace package in
                    Io.write_file (Path.source fn)
                      (Install.gen_install_file entries)))
        in
        Path.Set.to_list !files_deleted_in
        (* This [List.rev] is to ensure we process children directories before
           their parents *)
        |> List.rev
        |> List.iter ~f:(Ops.remove_dir_if_exists ~if_non_empty:Warn))
  in
  ( term
  , Cmdliner.Term.info (cmd_what what) ~doc
      ~man:Manpage.(`S s_synopsis :: (synopsis @ Common.help_secs)) )

let install = install_uninstall ~what:Install

let uninstall = install_uninstall ~what:Uninstall

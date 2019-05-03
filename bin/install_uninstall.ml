open Stdune
open Import

let interpret_destdir ~destdir path =
  match destdir with
  | None ->
    path
  | Some prefix ->
    Path.append_local
      (Path.of_string_exn prefix)
      (Path.local_part path)

let get_dirs context ~prefix_from_command_line ~libdir_from_command_line =
  match prefix_from_command_line with
  | Some p ->
    let prefix = Path.of_string_exn p in
    let dir = Option.value ~default:"lib" libdir_from_command_line in
    Fiber.return (prefix, Some (Path.relative_exn prefix dir))
  | None ->
    let open Fiber.O in
    let* prefix = Context.install_prefix context in
    let libdir =
      match libdir_from_command_line with
      | None -> Context.install_ocaml_libdir context
      | Some l -> Fiber.return (Some (Path.relative_exn prefix l))
    in
    let+ libdir = libdir in
    (prefix, libdir)

let resolve_package_install setup pkg =
  match Import.Main.package_install_file setup pkg with
  | Ok path -> path
  | Error () ->
    let pkg = Package.Name.to_string pkg in
    die "Unknown package %s!%s" pkg
      (hint pkg
         (Package.Name.Map.keys setup.conf.packages
          |> List.map ~f:Package.Name.to_string))

let print_unix_error f =
  try
    f ()
  with Unix.Unix_error (e, _, _) ->
    Format.eprintf "@{<error>Error@}: %s@."
      (Unix.error_message e)


let set_executable_bits   x = x lor  0o111
let clear_executable_bits x = x land (lnot 0o111)

(** Operations that act on real files or just pretend to (for --dry-run) *)
module type FILE_OPERATIONS = sig
  val copy_file : src:Path.t -> dst:Path.t -> executable:bool -> unit
  val mkdir_p : Path.t -> unit
  val remove_if_exists : Path.t -> unit
  val remove_dir_if_empty : Path.t -> unit
end

module File_ops_dry_run : FILE_OPERATIONS = struct
  let copy_file ~src ~dst ~executable =
    Format.printf
      "Copying %a to %a (executable: %b)\n"
      Path.pp src
      Path.pp dst
      executable

  let mkdir_p path =
    Format.printf
      "Creating directory %a\n"
      Path.pp
      path

  let remove_if_exists path =
    Format.printf
      "Removing (if it exists) %a\n"
      Path.pp
      path

  let remove_dir_if_empty path =
    Format.printf
      "Removing directory (if empty) %a\n"
      Path.pp
      path
end

module File_ops_real : FILE_OPERATIONS = struct
  let copy_file ~src ~dst ~executable =
    let chmod =
      if executable then
        set_executable_bits
      else
        clear_executable_bits
    in
    Io.copy_file ~src ~dst ~chmod ()

  let remove_if_exists dst =
    if Path.exists dst then begin
      Printf.eprintf
        "Deleting %s\n%!"
        (Path.to_string_maybe_quoted dst);
      print_unix_error (fun () -> Path.unlink dst)
    end

  let remove_dir_if_empty dir =
    if Path.exists dir then
      match Path.readdir_unsorted dir with
      | Ok [] ->
        Printf.eprintf "Deleting empty directory %s\n%!"
          (Path.to_string_maybe_quoted dir);
        print_unix_error (fun () -> Path.rmdir dir)
      | Error e ->
        Format.eprintf "@{<error>Error@}: %s@."
          (Unix.error_message e)
      | _  -> ()

  let mkdir_p = Path.mkdir_p
end

let file_operations ~dry_run : (module FILE_OPERATIONS) =
  if dry_run then
    (module File_ops_dry_run)
  else
    (module File_ops_real)

let install_uninstall ~what =
  let doc =
    sprintf "%s packages." (String.capitalize what)
  in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let term =
    let+ common = Common.term
    and+ prefix_from_command_line =
      Arg.(value
           & opt (some string) None
           & info ["prefix"]
               ~docv:"PREFIX"
               ~doc:"Directory where files are copied. For instance binaries \
                     are copied into $(i,\\$prefix/bin), library files into \
                     $(i,\\$prefix/lib), etc... It defaults to the current opam \
                     prefix if opam is available and configured, otherwise it uses \
                     the same prefix as the ocaml compiler.")
    and+ libdir_from_command_line =
      Arg.(value
           & opt (some string) None
           & info ["libdir"]
               ~docv:"PATH"
               ~doc:"Directory where library files are copied, relative to \
                     $(b,prefix) or absolute. If $(b,--prefix) \
                     is specified the default is $(i,\\$prefix/lib), otherwise \
                     it is the output of $(b,ocamlfind printconf destdir)"
          )
    and+ destdir =
      Arg.(value
           & opt (some string) None
           & info ["destdir"]
               ~env:(env_var "DESTDIR")
               ~docv:"PATH"
               ~doc:"When passed, this directory is prepended to all \
                     installed paths."
          )
    and+ dry_run =
      Arg.(value
           & flag
           & info ["dry-run"]
               ~doc:"Only display the file operations that would be performed."
          )
    and+ pkgs =
      Arg.(value & pos_all package_name [] name_)
    in
    Common.set_common common ~targets:[];
    let log = Log.create common in
    Scheduler.go ~log ~common (fun () ->
      let open Fiber.O in
      let* workspace = Import.Main.scan_workspace ~log common in
      let pkgs =
        match pkgs with
        | [] -> Package.Name.Map.keys workspace.conf.packages
        | l  -> l
      in
      let install_files, missing_install_files =
        List.concat_map pkgs ~f:(fun pkg ->
          let fn = resolve_package_install workspace pkg in
          List.map workspace.contexts ~f:(fun ctx ->
            let fn = Path.append_source ctx.Context.build_dir fn in
            if Path.exists fn then
              Left (ctx, (pkg, fn))
            else
              Right fn))
        |> List.partition_map ~f:Fn.id
      in
      if missing_install_files <> [] then begin
        die "The following <package>.install are missing:\n\
             %s\n\
             You need to run: dune build @install"
          (String.concat ~sep:"\n"
             (List.map missing_install_files
                ~f:(fun p -> sprintf "- %s" (Path.to_string p))))
      end;
      (match
         workspace.contexts,
         prefix_from_command_line,
         libdir_from_command_line
       with
       | _ :: _ :: _, Some _, _ | _ :: _ :: _, _, Some _ ->
         die "Cannot specify --prefix or --libdir when installing \
              into multiple contexts!"
       | _ -> ());
      let module CMap = Map.Make(Context) in
      let install_files_by_context =
        CMap.of_list_multi install_files |> CMap.to_list
      in
      let (module Ops) = file_operations ~dry_run in
      Fiber.parallel_iter install_files_by_context
        ~f:(fun (context, install_files) ->
          let+ (prefix, libdir) =
            get_dirs context ~prefix_from_command_line ~libdir_from_command_line
          in
          List.iter install_files ~f:(fun (package, path) ->
            let entries = Install.load_install_file path in
            let paths =
              Install.Section.Paths.make
                ~package
                ~destdir:prefix
                ?libdir
                ()
            in
            let files_deleted_in = ref Path.Set.empty in
            List.iter entries ~f:(fun entry ->
              let dst =
                Install.Entry.relative_installed_path entry ~paths
                |> interpret_destdir ~destdir
              in
              let dir = Path.parent_exn dst in
              if what = "install" then begin
                Printf.eprintf "Installing %s\n%!"
                  (Path.to_string_maybe_quoted dst);
                Ops.mkdir_p dir;
                let executable =
                  Install.Section.should_set_executable_bit entry.section
                in
                Ops.copy_file ~src:entry.src ~dst ~executable
              end else begin
                Ops.remove_if_exists dst;
                files_deleted_in := Path.Set.add !files_deleted_in dir;
              end;
              Path.Set.to_list !files_deleted_in
              (* This [List.rev] is to ensure we process children
                 directories before their parents *)
              |> List.rev
              |> List.iter ~f:Ops.remove_dir_if_empty))))
  in
  (term, Cmdliner.Term.info what ~doc ~man:Common.help_secs)

let install   = install_uninstall ~what:"install"
let uninstall = install_uninstall ~what:"uninstall"

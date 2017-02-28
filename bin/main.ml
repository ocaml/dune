open Jbuilder
open Import
open Jbuilder_cmdliner.Cmdliner

(* Things in src/ don't depend on cmdliner to speed up the bootstrap, so we set this
   reference here *)
let () = suggest_function := Jbuilder_cmdliner.Cmdliner_suggest.value

let (>>=) = Future.(>>=)

type common =
  { concurrency    : int
  ; debug_rules    : bool
  ; debug_dep_path : bool
  ; debug_findlib  : bool
  ; dev_mode       : bool
  ; workspace_file : string option
  ; root           : string
  ; target_prefix  : string
  }

let prefix_target common s = common.target_prefix ^ s

let set_common c =
  Clflags.concurrency := c.concurrency;
  Clflags.debug_rules := c.debug_rules;
  Clflags.debug_dep_path := c.debug_dep_path;
  Clflags.debug_findlib := c.debug_findlib;
  Clflags.dev_mode := c.dev_mode;
  Printf.eprintf "Workspace root: %s\n" c.root;
  if c.root <> Filename.current_dir_name then
    Sys.chdir c.root

module Main = struct
  include Jbuilder.Main

  let setup common =
    setup ?workspace_file:common.workspace_file ()
end

let create_log = Main.create_log

type ('a, 'b) walk_result =
  | Cont of 'a
  | Stop of 'b

let rec walk_parents dir ~init ~f =
  match f init dir with
  | Stop x -> Stop x
  | Cont x ->
    let parent = Filename.dirname dir in
    if parent = dir then
      Cont x
    else
      walk_parents parent ~init:x ~f

let find_root () =
  let cwd = Sys.getcwd () in
  let rec loop counter ~candidates ~to_cwd dir =
    let files = Sys.readdir dir |> Array.to_list |> String_set.of_list in
    if String_set.mem "jbuild-workspace" files then
      cont counter ~candidates:((0, dir, to_cwd) :: candidates) dir ~to_cwd
    else if String_set.exists files ~f:(fun fn ->
        String.is_suffix fn ~suffix:".install") then
      cont counter ~candidates:((1, dir, to_cwd) :: candidates) dir ~to_cwd
    else if String_set.mem ".git" files || String_set.mem ".hg" files then
      cont counter ~candidates:((2, dir, to_cwd) :: candidates) dir ~to_cwd
    else
      cont counter ~candidates dir ~to_cwd
  and cont counter ~candidates ~to_cwd dir =
    if counter > String.length cwd then
      candidates
    else
      let parent = Filename.dirname dir in
      if parent = dir then
        candidates
      else
        let base = Filename.basename dir in
        loop (counter + 1) parent ~candidates ~to_cwd:(base :: to_cwd)
  in
  match loop 0 ~candidates:[] ~to_cwd:[] cwd with
  | [] -> (cwd, [])
  | l ->
    let lowest_priority =
      List.fold_left l ~init:max_int ~f:(fun acc (prio, _, _) ->
        min acc prio)
    in
    match List.find l ~f:(fun (prio, _, _) -> prio = lowest_priority) with
    | None -> assert false
    | Some (_, dir, to_cwd) -> (dir, to_cwd)

let copts_sect = "COMMON OPTIONS"
let help_secs =
  [ `S copts_sect
  ; `P "These options are common to all commands."
  ; `S "MORE HELP"
  ; `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."
  ; `S "BUGS"
  ; `P "Check bug reports at https://github.com/janestreet/jbuilder/issues"
  ]

let common =
  let make concurrency debug_rules debug_dep_path debug_findlib dev_mode
        workspace_file root =
    let root, to_cwd =
      match root with
      | Some dn -> (dn, [])
      | None -> find_root ()
    in
    { concurrency
    ; debug_rules
    ; debug_dep_path
    ; debug_findlib
    ; dev_mode
    ; workspace_file
    ; root
    ; target_prefix = String.concat ~sep:"" (List.map to_cwd ~f:(sprintf "%s/"))
    }
  in
  let docs = copts_sect in
  let concurrency =
    Arg.(value
         & opt int !Clflags.concurrency
         & info ["j"] ~docs ~docv:"JOBS"
             ~doc:{|Run no more than $(i,JOBS) commands simultaneously.|}
        )
  in
  let drules =
    Arg.(value
         & flag
         & info ["debug-rules"] ~docs
             ~doc:"Print all internal rules."
        )
  in
  let ddep_path =
    Arg.(value
         & flag
         & info ["debug-depency-path"] ~docs
             ~doc:{|In case of error, print the dependency path from
                    the targets on the command line to the rule that failed.
                  |})
  in
  let dfindlib =
    Arg.(value
         & flag
         & info ["debug-findlib"] ~docs
             ~doc:{|Debug the findlib sub-system.|})
  in
  let dev =
    Arg.(value
         & flag
         & info ["dev"] ~docs
             ~doc:{|Use stricter compilation flags by default.|})
  in
  let workspace_file =
    Arg.(value
         & opt (some file) None
         & info ["workspace"] ~docs ~docv:"FILE"
             ~doc:"Use this specific workspace file instead of looking it up.")
  in
  let root =
    Arg.(value
         & opt (some dir) None
         & info ["root"] ~docs ~docv:"DIR"
             ~doc:{|Use this directory as workspace root instead of guessing it.
                   Note that this option doesn't change the interpretation of
                   targets given on the command line. It is only intended
                    for scripts.|})
  in
  Term.(const make
        $ concurrency
        $ drules
        $ ddep_path
        $ dfindlib
        $ dev
        $ workspace_file
        $ root
       )

let installed_libraries =
  let doc = "Print out libraries installed on the system." in
  let go common =
    set_common common;
    Future.Scheduler.go ~log:(create_log ())
      (Context.default () >>= fun ctx ->
       let findlib = ctx.findlib in
       let pkgs = Findlib.all_packages findlib in
       let max_len = List.longest_map pkgs ~f:(fun p -> p.name) in
       List.iter pkgs ~f:(fun pkg ->
         let ver =
           match pkg.Findlib.version with
           | "" -> "n/a"
           | v  -> v
         in
         Printf.printf "%-*s (version: %s)\n" max_len pkg.name ver);
       Future.return ())
  in
  ( Term.(const go
          $ common)
  , Term.info "installed-libraries" ~doc
  )

let resolve_package_install setup pkg =
  match Main.package_install_file setup pkg with
  | Ok path -> path
  | Error () ->
    die "Unknown package %s!%s" pkg (hint pkg (String_map.keys setup.packages))

let build_package common pkg =
  Future.Scheduler.go ~log:(create_log ())
    (Main.setup common >>= fun setup ->
     Build_system.do_build_exn setup.build_system
       [resolve_package_install setup pkg])

let build_package =
  let doc = "Build a single package in release mode." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This command is meant to be used in $(i,<package>.opam) files.
           It builds the given package as if all the definitions in
           the source tree that are for another package didn't existed.
         |}
    ; `P {|More precisely, this is what you should use in your $(i,<package>.opam) file:|}
    ; `Pre {|  build: ["jbuilder" "build-package" "<package>" "-j" jobs|}
    ; `Blocks help_secs
    ]
  in
  let name_ = Arg.info [] ~docv:"PACKAGE-NAME" in
  let go common pkg =
    set_common common;
    build_package common pkg
  in
  ( Term.(const go
          $ common
          $ Arg.(required & pos 0 (some string) None name_))
  , Term.info "build-package" ~doc ~man)

let external_lib_deps packages =
  let log = create_log () in
  let deps =
    Path.Map.fold (Main.external_lib_deps ~log ~packages ()) ~init:String_map.empty
      ~f:(fun ~key:_ ~data:deps acc -> Build.merge_lib_deps deps acc)
  in
  String_map.iter deps ~f:(fun ~key:n ~data ->
    match (data : Build.lib_dep_kind) with
    | Required -> Printf.printf "%s\n" n
    | Optional -> Printf.printf "%s (optional)\n" n)

let external_lib_deps =
  let doc = "Print out external library dependencies." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Print out the external libraries needed to build the given packages.|}
    ; `P {|The output should be included in what is written in
           your $(i,<package>.opam) file.|}
    ; `Blocks help_secs
    ]
  in
  let name_ = Arg.info [] ~docv:"PACKAGE-NAME" in
  let go common packages =
    set_common common;
    external_lib_deps packages
  in
  ( Term.(const go
          $ common
          $ Arg.(non_empty & pos_all string [] name_))
  , Term.info "external-lib-deps" ~doc ~man)

type target =
  | File  of Path.t
  | Alias of Path.t * Alias.t

let resolve_targets common (setup : Main.setup) user_targets =
  match user_targets with
  | [] -> []
  | _ ->
    let targets =
      List.concat_map user_targets ~f:(fun s ->
        if String.is_prefix s ~prefix:"@" then
          let s = String.sub s ~pos:1 ~len:(String.length s - 1) in
          let path = Path.relative Path.root (prefix_target common s) in
          if Path.is_root path then
            die "@ on the command line must be followed by a valid alias name"
          else
            let dir = Path.parent path in
            let name = Path.basename path in
            [Alias (path, Alias.make ~dir name)]
        else
          let path = Path.relative Path.root (prefix_target common s) in
          let can't_build path =
            die "Don't know how to build %s" (Path.to_string path)
          in
          if not (Path.is_local path) then
            [File path]
          else if Path.is_in_build_dir path then begin
            if Build_system.is_target setup.build_system path then
              [File path]
            else
              can't_build path
          end else
            match
              let l =
                List.filter_map setup.contexts ~f:(fun ctx ->
                    let path = Path.append ctx.Context.build_dir path in
                    if Build_system.is_target setup.build_system path then
                      Some (File path)
                    else
                      None)
              in
              if Build_system.is_target setup.build_system path ||
                 Path.exists path then
                File path :: l
              else
                l
            with
            | [] -> can't_build path
            | l  -> l
        )
    in
    Printf.printf "Building the following targets:\n";
    List.iter targets ~f:(function
      | File path ->
        Printf.printf "- %s\n" (Path.to_string path)
      | Alias (path, _) ->
        Printf.printf "- alias %s\n" (Path.to_string path));
    flush stdout;
    List.map targets ~f:(function
      | File path -> path
      | Alias (_, alias) -> Alias.file alias)

let build_targets =
  let doc = "Build the given targets." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|Targets starting with a $(b,@) are interpreted as aliases.|}
    ; `Blocks help_secs
    ]
  in
  let name_ = Arg.info [] ~docv:"TARGET" in
  let go common targets =
    set_common common;
    Future.Scheduler.go ~log:(create_log ())
      (Main.setup common >>= fun setup ->
       let targets = resolve_targets common setup targets in
       Build_system.do_build_exn setup.build_system targets) in
  ( Term.(const go
          $ common
          $ Arg.(non_empty & pos_all string [] name_))
  , Term.info "build" ~doc ~man)

let runtest =
  let doc = "Run tests." in
  let man =
    [ `S "DESCRIPTION"
    ; `P {|This is a short-hand for calling:|}
    ; `Pre {|  jbuilder build @runtest|}
    ; `Blocks help_secs
    ]
  in
  let name_ = Arg.info [] ~docv:"DIR" in
  let go common dirs =
    set_common common;
    Future.Scheduler.go ~log:(create_log ())
      (Main.setup common >>= fun setup ->
       let targets =
         List.map dirs ~f:(fun dir ->
           let dir = Path.(relative root) (prefix_target common dir) in
           Alias.file (Alias.runtest ~dir))
       in
       Build_system.do_build_exn setup.build_system targets) in
  ( Term.(const go
          $ common
          $ Arg.(value & pos_all string ["."] name_))
  , Term.info "runtest" ~doc ~man)

let opam_installer () =
  match Bin.which "opam-installer" with
  | None ->
    die "\
Sorry, you need the opam-installer tool to be able to install or
uninstall packages.

I couldn't find the opam-installer binary :-("
  | Some fn -> fn

let get_prefix context ~from_command_line =
  match from_command_line with
  | Some p -> Future.return (Path.of_string p)
  | None -> Context.install_prefix context

let install_uninstall ~what =
  let doc =
    sprintf "%s packages using opam-installer." (String.capitalize_ascii what)
  in
  let name_ = Arg.info [] ~docv:"PACKAGE" in
  let go common prefix pkgs =
    set_common common;
    let opam_installer = opam_installer () in
    Future.Scheduler.go ~log:(create_log ())
      (Main.setup common >>= fun setup ->
       let pkgs =
         match pkgs with
         | [] -> String_map.keys setup.packages
         | l  -> l
       in
       let install_files, missing_install_files =
         List.concat_map pkgs ~f:(fun pkg ->
           let fn = resolve_package_install setup pkg in
           List.map setup.contexts ~f:(fun ctx ->
             let fn = Path.append ctx.Context.build_dir fn in
             if Path.exists fn then
               Inl (ctx, fn)
             else
               Inr fn))
         |> List.partition_map ~f:(fun x -> x)
       in
       if missing_install_files <> [] then begin
         die "The following <package>.install are missing:\n\
              %s\n\
              You need to run: jbuilder build @install"
           (String.concat ~sep:"\n"
              (List.map missing_install_files
                 ~f:(fun p -> sprintf "- %s" (Path.to_string p))))
       end;
       (match setup.contexts, prefix with
        | _ :: _ :: _, Some _ ->
          die "Cannot specify --prefix when installing into multiple contexts!"
        | _ -> ());
       let module CMap = Map.Make(Context) in
       let install_files_by_context = CMap.of_alist_multi install_files |> CMap.bindings in
       Future.all_unit
         (List.map install_files_by_context ~f:(fun (context, install_files) ->
            get_prefix context ~from_command_line:prefix >>= fun prefix ->
            Future.all_unit
              (List.map install_files ~f:(fun path ->
                   Future.run Strict (Path.to_string opam_installer)
                     [ sprintf "-%c" what.[0]
                     ; "--prefix"
                     ; Path.to_string prefix
                     ; Path.to_string path
                     ])))))
  in
  ( Term.(const go
          $ common
          $ Arg.(value & opt (some dir) None & info ["prefix"])
          $ Arg.(value & pos_all string [] name_))
  , Term.info what ~doc ~man:help_secs)

let install   = install_uninstall ~what:"install"
let uninstall = install_uninstall ~what:"uninstall"

let all =
  [ installed_libraries
  ; build_package
  ; external_lib_deps
  ; build_targets
  ; runtest
  ; install
  ; uninstall
  ]

let default =
  let doc = "composable build system for OCaml" in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ common))
  , Term.info "jbuilder" ~doc
      ~man:
        [ `S "DESCRIPTION"
        ; `P {|Jbuilder is a build system designed for OCaml projects only. It
               focuses on providing the user with a consistent experience and takes
               care of most of the low-level details of OCaml compilation. All you
               have to do is provide a description of your project and Jbuilder will
               do the rest.
             |}
        ; `P {|The scheme it implements is inspired from the one used inside Jane
               Street and adapted to the open source world. It has matured over a
               long time and is used daily by hundred of developpers, which means
               that it is highly tested and productive.
             |}
        ; `Blocks help_secs
        ]
  )

let () =
  Ansi_color.setup_err_formatter_colors ();
  try
    match Term.eval_choice default all ~catch:false with
    | `Error _ -> exit 1
    | _ -> exit 0
  with exn ->
    Format.eprintf "%a@?" (Main.report_error ?map_fname:None) exn;
    exit 1

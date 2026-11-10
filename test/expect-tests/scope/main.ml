open Stdune
open Dune_engine
open Dune_rules
module Workspace = Source.Workspace
module Dune_config = Dune_config_file.Dune_config
module Scheduler = Dune_scheduler.Scheduler

let%expect_test "package library entries release obsolete cache results" =
  let cwd = Path.External.cwd () in
  Temp.with_temp_dir
    ~parent_dir:(Path.external_ cwd)
    ~prefix:"scope-cache"
    ~suffix:""
    ~f:(fun dir ->
      let dir = Result.ok_exn dir in
      Exn.protect
        ~finally:(fun () -> Sys.chdir (Path.External.to_string cwd))
        ~f:(fun () ->
          Sys.chdir (Path.to_string dir);
          Dune_tests_common.init ();
          Global.init ~capture_outputs:false;
          Workspace.Clflags.set
            { Workspace.Clflags.x = None
            ; profile = None
            ; instrument_with = None
            ; workspace_file = None
            ; config_from_command_line = Dune_config.Partial.empty
            ; config_from_config_file = Dune_config.Partial.empty
            };
          Source.Only_packages.Clflags.set No_restriction;
          Memo.reset (Fs_memo.init ~dune_file_watcher:None);
          Io.write_file_exn
            (Path.relative Path.root "dune-project")
            "(lang dune 3.21)\n\
            \ (package (name a) (deprecated_package_names old-a))\n\
            \ (package (name b))\n";
          let write_stanzas names =
            let redirects =
              List.map names ~f:(fun name ->
                sprintf
                  "(deprecated_library_name (old_public_name old-a.%s)\n\
                  \ (new_public_name b.missing))\n"
                  name)
              |> String.concat ~sep:""
            in
            Io.write_file_exn
              (Path.relative Path.root "dune")
              (redirects
               ^ "(install (package a) (section share) (files missing))\n"
               ^ "(library (name unowned))\n"
               ^ "(library (public_name b)\n (enabled_if (= %{read:missing} true)))\n")
          in
          let run computation =
            Scheduler.Run.go
              { Scheduler.Config.concurrency = 1
              ; print_ctrl_c_warning = false
              ; watch_exclusions = []
              }
              ~file_watcher:No_watcher
              (fun () -> Memo.run computation)
          in
          let entries () =
            Scope.DB.lib_entries_of_package
              Context_name.default
              (Dune_lang.Package.Name.of_string "a")
          in
          let package_stanzas name =
            Dune_load.stanzas_of_package
              Context_name.default
              (Dune_lang.Package.Name.of_string name)
          in
          let previous = Weak.create 1 in
          write_stanzas [ "old" ];
          run
            (let open Memo.O in
             let+ stanzas = package_stanzas "a"
             and+ other = package_stanzas "b"
             and+ deprecated = package_stanzas "old-a"
             and+ unknown = package_stanzas "unknown"
             and+ dune_files = Dune_load.dune_files Context_name.default in
             let expected =
               List.concat_map dune_files ~f:Dune_file.static_stanzas
               |> List.filter ~f:(fun stanza ->
                 match Dune_lang.Stanza.repr stanza with
                 | Library.T _ -> false
                 | _ -> true)
             in
             printfn "indexed a stanzas: %d" (List.length stanzas);
             printfn "indexed b stanzas: %d" (List.length other);
             printfn
               "source order preserved: %b"
               (List.equal Dune_lang.Stanza.equal (List.map stanzas ~f:snd) expected);
             printfn "deprecated package bucket: %d" (List.length deprecated);
             printfn "unknown package bucket: %d" (List.length unknown));
          run
            (let open Memo.O in
             let+ entries = entries () in
             printfn
               "initial deprecated names: %d"
               (List.length entries.deprecated_library_names);
             Weak.set previous 0 (Some entries));
          write_stanzas [ "old"; "new" ];
          Memo.reset
            (Memo.Invalidation.invalidate_caches ~reason:Memo.Invalidation.Reason.Test);
          run
            (let open Memo.O in
             let+ entries = entries () in
             printfn
               "updated deprecated names: %d"
               (List.length entries.deprecated_library_names));
          Gc.full_major ();
          printfn "obsolete entries retained: %b" (Weak.check previous 0)));
  [%expect
    {|
    indexed a stanzas: 2
    indexed b stanzas: 1
    source order preserved: true
    deprecated package bucket: 0
    unknown package bucket: 0
    initial deprecated names: 1
    updated deprecated names: 2
    obsolete entries retained: false
    |}]
;;

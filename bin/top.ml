open Stdune
open Import

let doc =
  "Print a list of toplevel directives for including directories and loading \
   cma files."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Print a list of toplevel directives for including directories and loading cma files.|}
  ; `P
      {|The output of $(b,dune toplevel-init-file) should be evaluated in a toplevel
          to make a library available there.|}
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "top" ~doc ~man

let link_deps sctx link =
  let open Memo.O in
  Memo.parallel_map link ~f:(fun t ->
      Dune_rules.Lib_flags.link_deps sctx t Dune_rules.Link_mode.Byte)
  >>| List.concat

let term =
  let+ common = Common.term
  and+ dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"DIR")
  and+ ctx_name =
    Common.context_arg ~doc:{|Select context where to build/run utop.|}
  in
  let config = Common.init common in
  Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      Build_system.run_exn (fun () ->
          let open Memo.O in
          let* setup = setup in
          let sctx =
            Dune_engine.Context_name.Map.find setup.scontexts ctx_name
            |> Option.value_exn
          in
          let* libs =
            let dir =
              let build_dir = (Super_context.context sctx).build_dir in
              Path.Build.relative build_dir (Common.prefix_target common dir)
            in
            let* db =
              let+ scope = Dune_rules.Scope.DB.find_by_dir dir in
              Dune_rules.Scope.libs scope
            in
            Dune_rules.Utop.libs_under_dir sctx ~db ~dir:(Path.build dir)
          in
          let* requires =
            Dune_rules.Resolve.Memo.read_memo
              (Dune_rules.Lib.closure ~linking:true libs)
          in
          let include_paths =
            Dune_rules.Lib_flags.L.toplevel_include_paths requires
          in
          let* files = link_deps sctx requires in
          let+ () = Memo.parallel_iter files ~f:Build_system.build_file in
          let files_to_load =
            List.filter files ~f:(fun p ->
                let ext = Path.extension p in
                ext = Ocaml.Mode.compiled_lib_ext Byte
                || ext = Ocaml.Cm_kind.ext Cmo)
          in
          Dune_rules.Toplevel.print_toplevel_init_file ~include_paths
            ~files_to_load))

let command = Cmd.v info term

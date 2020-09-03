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

let info = Term.info "top" ~doc ~man

let link_deps link =
  List.concat_map link ~f:(fun t ->
      Dune_rules.Lib.link_deps t Dune_rules.Link_mode.Byte)

let term =
  let+ common = Common.term
  and+ dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"DIR")
  and+ ctx_name =
    Common.context_arg ~doc:{|Select context where to build/run utop.|}
  in
  Common.set_common common ~targets:[];
  Scheduler.go ~common (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup common in
      let sctx =
        Dune_engine.Context_name.Map.find setup.scontexts ctx_name
        |> Option.value_exn
      in
      let dir =
        let build_dir = (Super_context.context sctx).build_dir in
        Path.Build.relative build_dir (Common.prefix_target common dir)
      in
      let scope = Super_context.find_scope_by_dir sctx dir in
      let db = Dune_rules.Scope.libs scope in
      let libs =
        Dune_rules.Utop.libs_under_dir sctx ~db ~dir:(Path.build dir)
      in
      let requires =
        Dune_rules.Lib.closure ~linking:true libs |> Result.ok_exn
      in
      let include_paths = Dune_rules.Lib.L.include_paths requires in
      let files = link_deps requires in
      let* () = do_build (List.map files ~f:(fun f -> Target.File f)) in
      let files_to_load =
        List.filter files ~f:(fun p ->
            let ext = Path.extension p in
            ext = Dune_rules.Mode.compiled_lib_ext Byte
            || ext = Dune_rules.Cm_kind.ext Cmo)
      in
      Dune_rules.Toplevel.print_toplevel_init_file ~include_paths ~files_to_load;
      Fiber.return ())

let command = (term, info)

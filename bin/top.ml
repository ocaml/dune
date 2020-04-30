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

let link_deps link ~lib_config =
  List.concat_map link ~f:(fun t ->
      Dune.Lib.link_deps t Dune.Link_mode.Byte lib_config)

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
        Dune.Context_name.Map.find setup.scontexts ctx_name |> Option.value_exn
      in
      let dir =
        Path.Build.relative
          (Super_context.build_dir sctx)
          (Common.prefix_target common dir)
      in
      let scope = Super_context.find_scope_by_dir sctx dir in
      let db = Dune.Scope.libs scope in
      let libs = Dune.Utop.libs_under_dir sctx ~db ~dir:(Path.build dir) in
      let requires = Dune.Lib.closure ~linking:true libs |> Result.ok_exn in
      let include_paths = Dune.Lib.L.include_paths requires in
      let lib_config = sctx |> Super_context.context |> Context.lib_config in
      let files = link_deps requires ~lib_config in
      let* () = do_build (List.map files ~f:(fun f -> Target.File f)) in
      let files_to_load =
        List.filter files ~f:(fun p ->
            let ext = Path.extension p in
            ext = Dune.Mode.compiled_lib_ext Byte || ext = Dune.Cm_kind.ext Cmo)
      in
      Dune.Toplevel.print_toplevel_init_file ~include_paths ~files_to_load;
      Fiber.return ())

let command = (term, info)

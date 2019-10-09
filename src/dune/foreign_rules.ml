open! Stdune

let build_c_file ~sctx ~dir ~expander ~include_flags (loc, src, dst) =
  let flags = Foreign.Source.flags src in
  let ctx = Super_context.context sctx in
  let c_flags =
    Super_context.foreign_flags sctx ~dir ~expander ~flags
      ~language:Foreign.Language.C
  in
  Super_context.add_rule sctx ~loc
    ~dir
      (* With sandboxing we get errors like: bar.c:2:19: fatal error: foo.cxx:
         No such file or directory #include "foo.cxx" *)
    ~sandbox:Sandbox_config.no_sandboxing
    (let src = Path.build (Foreign.Source.path src) in
     Command.run
     (* We have to execute the rule in the library directory as the .o is
        produced in the current directory *) ~dir:(Path.build dir)
       (Ok ctx.ocamlc)
       [ A "-g"
       ; include_flags
       ; Dyn (Build.map c_flags ~f:(fun x -> Command.quote_args "-ccopt" x))
       ; A "-o"
       ; Target dst
       ; Dep src
       ]);
  dst

let build_cxx_file ~sctx ~dir ~expander ~include_flags (loc, src, dst) =
  let flags = Foreign.Source.flags src in
  let ctx = Super_context.context sctx in
  let output_param =
    if ctx.ccomp_type = "msvc" then
      [ Command.Args.Concat ("", [ A "/Fo"; Target dst ]) ]
    else
      [ A "-o"; Target dst ]
  in
  let cxx_flags =
    Super_context.foreign_flags sctx ~dir ~expander ~flags
      ~language:Foreign.Language.Cxx
  in
  Super_context.add_rule sctx ~loc
    ~dir
      (* this seems to work with sandboxing, but for symmetry with
         [build_c_file] disabling that here too *)
    ~sandbox:Sandbox_config.no_sandboxing
    (let src = Path.build (Foreign.Source.path src) in
     Command.run
     (* We have to execute the rule in the library directory as the .o is
        produced in the current directory *) ~dir:(Path.build dir)
       (Super_context.resolve_program ~loc:None ~dir sctx ctx.c_compiler)
       ( [ Command.Args.S [ A "-I"; Path ctx.stdlib_dir ]
         ; include_flags
         ; Command.Args.dyn cxx_flags
         ]
       @ output_param @ [ A "-c"; Dep src ] ));
  dst

(* TODO: [requires] is a confusing name, probably because it's too general: it
   looks like it's a list of libraries we depend on. *)
let build_o_files ~sctx ~foreign_sources ~(dir : Path.Build.t) ~expander
    ~requires ~dir_contents ?extra_flags ~(extra_deps : Dep_conf.t list) =
  let extra_flags = Option.value ~default:Command.Args.empty extra_flags in
  let ctx = Super_context.context sctx in
  let all_dirs = Dir_contents.dirs dir_contents in
  let h_files =
    List.fold_left all_dirs ~init:[] ~f:(fun acc dc ->
        String.Set.fold (Dir_contents.text_files dc) ~init:acc
          ~f:(fun fn acc ->
            if String.is_suffix fn ~suffix:Foreign.header_ext then
              Path.relative (Path.build (Dir_contents.dir dc)) fn :: acc
            else
              acc))
  in
  let includes =
    Command.Args.S
      [ Hidden_deps (Dep.Set.of_files h_files)
      ; Command.of_result_map requires ~f:(fun libs ->
            S
              [ Lib.L.c_include_flags libs
              ; Hidden_deps
                  (Lib_file_deps.deps libs
                     ~groups:[ Lib_file_deps.Group.Header ])
              ])
      ]
  in
  let extra_deps =
    let open Build.O in
    let+ () = Super_context.Deps.interpret sctx extra_deps ~expander in
    Command.Args.empty
  in
  let include_flags =
    Command.Args.S [ includes; extra_flags; Dyn extra_deps ]
  in
  String.Map.to_list foreign_sources
  |> List.map ~f:(fun (obj, (loc, src)) ->
         let dst = Path.Build.relative dir (obj ^ ctx.lib_config.ext_obj) in
         let build_file =
           match Foreign.Source.language src with
           | C -> build_c_file
           | Cxx -> build_cxx_file
         in
         build_file ~sctx ~dir ~expander ~include_flags (loc, src, dst))

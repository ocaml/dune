open! Stdune
open Dune_file

let build_c_file (buildable : Buildable.t) ~sctx ~dir ~expander ~includes
  (loc, src, dst) =
  let ctx = Super_context.context sctx in
  let c_flags =
    (Super_context.c_flags sctx ~dir ~expander ~flags:buildable.c_flags).c
  in
  Super_context.add_rule sctx ~loc
    ~dir
      (* With sandboxing we get errors like: bar.c:2:19: fatal error: foo.cxx:
        No such file or directory #include "foo.cxx" *)
    ~sandbox:Sandbox_config.no_sandboxing
    (let src = Path.build (C.Source.path src) in
     Command.run
     (* We have to execute the rule in the library directory as the .o is
       produced in the current directory *) ~dir:(Path.build dir)
       (Ok ctx.ocamlc)
       [ A "-g"
       ; includes
       ; Dyn (Build.S.map c_flags ~f:(fun x -> Command.quote_args "-ccopt" x))
       ; A "-o"
       ; Target dst
       ; Dep src
       ]);
  dst

let build_cxx_file (buildable : Buildable.t) ~sctx ~dir ~expander ~includes
  (loc, src, dst) =
  let ctx = Super_context.context sctx in
  let output_param =
    if ctx.ccomp_type = "msvc" then
      [ Command.Args.Concat ("", [ A "/Fo"; Target dst ]) ]
    else
      [ A "-o"; Target dst ]
  in
  let cxx_flags =
    (Super_context.c_flags sctx ~dir ~expander ~flags:buildable.c_flags).cxx
  in
  Super_context.add_rule sctx ~loc
    ~dir
      (* this seems to work with sandboxing, but for symmetry with
        [build_c_file] disabling that here too *)
    ~sandbox:Sandbox_config.no_sandboxing
    (let src = Path.build (C.Source.path src) in
     Command.run
     (* We have to execute the rule in the library directory as the .o is
       produced in the current directory *) ~dir:(Path.build dir)
       (Super_context.resolve_program ~loc:None ~dir sctx ctx.c_compiler)
       ( [ Command.Args.S [ A "-I"; Path ctx.stdlib_dir ]
         ; includes
         ; Command.Args.dyn cxx_flags
         ]
       @ output_param @ [ A "-c"; Dep src ] ));
  dst

let build_o_files buildable ~sctx ~(c_sources : C.Sources.t) ~dir ~expander
  ~requires ~dir_contents =
  let ctx = Super_context.context sctx in
  let all_dirs = Dir_contents.dirs dir_contents in
  let h_files =
    List.fold_left all_dirs ~init:[] ~f:(fun acc dc ->
      String.Set.fold (Dir_contents.text_files dc) ~init:acc ~f:(fun fn acc ->
        if String.is_suffix fn ~suffix:C.header_ext then
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
            (Lib_file_deps.deps libs ~groups:[ Lib_file_deps.Group.Header ])
          ])
      ]
  in
  let build_x_files build_x files =
    String.Map.to_list files
    |> List.map ~f:(fun (obj, (loc, src)) ->
      let dst = Path.Build.relative dir (obj ^ ctx.lib_config.ext_obj) in
      build_x buildable ~sctx ~dir ~expander ~includes (loc, src, dst))
  in
  let { C.Kind.Dict.c; cxx } = C.Sources.split_by_kind c_sources in
  build_x_files build_c_file c @ build_x_files build_cxx_file cxx

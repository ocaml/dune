open! Stdune
open Import
open Build.O
open! No_io

module SC = Super_context

module Gen (P : Install_rules.Params) = struct

  let sctx = P.sctx
  let ctx = SC.context sctx

  let build_c_file (lib : Library.t) ~expander ~dir ~includes (loc, src, dst) =
    let src = C.Source.path src in
    SC.add_rule sctx ~loc ~dir
      (Expander.expand_and_eval_set expander lib.c_flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir
         (Ok ctx.ocamlc)
         [ A "-g"
         ; includes
         ; Dyn (fun c_flags -> Arg_spec.quote_args "-ccopt" c_flags)
         ; A "-o"; Target dst
         ; Dep src
         ]);
    dst

  let build_cxx_file (lib : Library.t) ~expander ~dir ~includes (loc, src, dst) =
    let src = C.Source.path src in
    let open Arg_spec in
    let output_param =
      if ctx.ccomp_type = "msvc" then
        [Concat ("", [A "/Fo"; Target dst])]
      else
        [A "-o"; Target dst]
    in
    SC.add_rule sctx ~loc ~dir
      (Expander.expand_and_eval_set expander lib.cxx_flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir
         (SC.resolve_program ~loc:None ~dir sctx ctx.c_compiler)
         ([ S [A "-I"; Path ctx.stdlib_dir]
          ; As (SC.cxx_flags sctx)
          ; includes
          ; Dyn (fun cxx_flags -> As cxx_flags)
          ] @ output_param @
          [ A "-c"; Dep src
          ]));
    dst

  let build_o_files lib ~(c_sources : C.Sources.t)
        ~dir ~expander ~requires ~dir_contents =
    let all_dirs = Dir_contents.dirs dir_contents in
    let h_files =
      List.fold_left all_dirs ~init:[] ~f:(fun acc dc ->
        String.Set.fold (Dir_contents.text_files dc) ~init:acc
          ~f:(fun fn acc ->
            if String.is_suffix fn ~suffix:".h" then
              Path.relative (Dir_contents.dir dc) fn :: acc
            else
              acc))
    in
    let includes =
      Arg_spec.S
        [ Hidden_deps h_files
        ; Arg_spec.of_result_map requires ~f:(fun libs ->
            S [ Lib.L.c_include_flags libs ~stdlib_dir:ctx.stdlib_dir
              ; Hidden_deps (Lib_file_deps.file_deps libs
                               ~groups:[Lib_file_deps.Group.Header])
              ])
        ]
    in
    let build_x_files build_x files =
      String.Map.to_list files
      |> List.map ~f:(fun (obj, (loc, src)) ->
        let dst = Path.relative dir (obj ^ ctx.ext_obj) in
        build_x lib ~expander ~dir ~includes (loc, src, dst)
      )
    in
    let { C.Kind.Dict. c; cxx } = C.Sources.split_by_kind c_sources in
    build_x_files build_c_file c
    @ build_x_files build_cxx_file cxx
end

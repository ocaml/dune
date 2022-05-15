open Import

let gen_select_rules t ~dir compile_info =
  let open Memo.O in
  Lib.Compile.resolved_selects compile_info
  |> Resolve.Memo.read_memo
  >>= Memo.parallel_iter
        ~f:(fun { Lib.Compile.Resolved_select.dst_fn; src_fn } ->
          let dst = Path.Build.relative dir dst_fn in
          Super_context.add_rule t ~dir
            (Action_builder.with_file_targets ~file_targets:[ dst ]
               (let open Action_builder.O in
               let* src_fn = Resolve.read src_fn in
               let src = Path.build (Path.Build.relative dir src_fn) in
               let+ () = Action_builder.path src in
               Action.Full.make (Action.Copy_and_add_line_directive (src, dst)))))

let with_lib_deps (t : Context.t) compile_info ~dir ~f =
  let prefix =
    if t.merlin then
      Lib.Compile.merlin_ident compile_info
      |> Merlin_ident.merlin_file_path dir
      |> Path.build |> Action_builder.path |> Action_builder.goal
    else Action_builder.return ()
  in
  Rules.prefix_rules prefix ~f

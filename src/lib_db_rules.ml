open Stdune
open Build.O

let gen_select_rules ctx ~dir compile_info =
  List.iter (Lib.Compile.resolved_selects compile_info) ~f:(fun rs ->
    let { Lib.Compile.Resolved_select.dst_fn; src_fn } = rs in
    let dst = Path.relative dir dst_fn in
    Rule_context.add_rule ctx
      (match src_fn with
       | Ok src_fn ->
         let src = Path.relative dir src_fn in
         Build.copy_and_add_line_directive ~src ~dst
       | Error e ->
         Build.fail ~targets:[dst]
           { fail = fun () ->
               raise (Lib.Error (No_solution_found_for_select e))
           }))

let with_lib_deps ctx compile_info ~dir ~f =
  let prefix =
    Lib.Compile.user_written_deps compile_info
    |> Dune_file.Lib_deps.info ~kind:(Lib.Compile.optional compile_info)
    |> Build.record_lib_deps
  in
  let prefix =
    let context = Rule_context.context ctx in
    if context.merlin then
      Build.path (Path.relative dir ".merlin-exists")
      >>>
      prefix
    else
      prefix
  in
  Rule_context.prefix_rules ctx prefix ~f

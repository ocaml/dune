open Import
open Jbuild
open Build.O
open! No_io

module SC = Super_context

module Ppx_info = struct
  exception Found_expect

  let uses_expect ~scope (lib : Jbuild.Library.t) =
    let user_ppx =
      Jbuild.Preprocess_map.pps lib.buildable.preprocess
      |> List.rev_map ~f:(fun pp -> Lib_dep.direct (Jbuild.Pp.to_string pp)) in
    match
      Lib_db.Scope.fold_transitive_closure
        scope
        user_ppx
        ~init:()
        ~f:(fun (lib : Lib.t) () ~required_by:_ ->
          if Lib.exists_name lib ~f:((=) "ppx_expect") then
            raise_notrace Found_expect
        ) with
    | () -> false
    | exception Found_expect -> true
    | exception _ -> false
end

let setup sctx ~dir ~(lib : Jbuild.Library.t) ~scope ~modules =
  Option.iter lib.inline_tests ~f:(fun inline_tests ->
    let uses_expect = Ppx_info.uses_expect lib ~scope in
    let name = lib.name ^ "_test_runner" in
    let module_filename = name ^ ".ml-gen" in
    let module_name = String.capitalize_ascii name in
    let main : Module.t =
      { name = module_name
      ; impl = Some { name   = module_filename
                    ; syntax = Module.Syntax.OCaml
                    }
      ; intf = None
      ; obj_name = ""
      }
    in

    SC.add_rule sctx (
      Build.write_file (Path.relative dir module_filename)
        "let () = Ppx_inline_test_lib.Runtime.exit ()");
    ignore (
      Exe.build_and_link sctx
        ~dir
        ~program:{ name; main }
        ~modules:(String_map.singleton module_name main)
        ~scope
        ~linkages:[Exe.Linkage.native_or_custom (SC.context sctx)]
        ~libraries:
          (List.map ~f:Lib_dep.direct (
             [lib.name]
             @ (if uses_expect then
                  ["ppx_expect.evaluator"]
                else
                  [])
             @ ["ppx_inline_test.runner.lib"]
           ))
        ~link_flags:(Build.return ["-linkall"])
      : _ * _);

    SC.add_alias_action sctx
      (Build_system.Alias.runtest ~dir)
      ~stamp:(List [Atom "ppx-runner"; Atom name])
      (let module A = Action in
       let exe = Path.relative dir (name ^ ".exe") in
       Build.path exe >>>
       Super_context.Deps.interpret sctx
         ~scope:scope.data
         ~dir
         inline_tests.deps
       >>^ fun _ ->
       A.chdir dir
         (A.progn
            (A.run (Ok exe)
               [ "inline-test-runner"
               ; lib.name
               ; "-source-tree-root"
               ; Path.reach (Super_context.context sctx).build_dir ~from:dir
               ; "-diff-cmd"; "-"
               ]
             ::
             (String_map.values modules
              |> List.concat_map ~f:(fun m ->
                [ Module.file m ~dir Impl
                ; Module.file m ~dir Intf
                ])
              |> List.filter_map ~f:(fun x -> x)
              |> List.map ~f:(fun fn ->
                A.diff ~optional:true
                  fn (Path.extend_basename fn ~suffix:".corrected")))))))

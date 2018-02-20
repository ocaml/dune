open Import
open Jbuild
open Build.O
open! No_io

module SC = Super_context

module Ppx_info = struct
  type t =
    { uses_ppx_expect      : bool
    ; uses_ppx_inline_test : bool
    }

  let default =
    { uses_ppx_expect      = false
    ; uses_ppx_inline_test = false
    }

  let make compile_info =
    Lib.Compile.pps compile_info
    |> function
    | Error _ -> default
    | Ok l ->
      List.fold_left l ~init:default ~f:(fun acc pp ->
        match Lib.name pp with
        | "ppx_expect"      -> { acc with uses_ppx_expect      = true }
        | "ppx_inline_test" -> { acc with uses_ppx_inline_test = true }
        | _                 -> acc)
end

let setup sctx ~dir ~(lib : Jbuild.Library.t) ~scope ~modules:lib_modules
      ~compile_info =
  Option.iter lib.inline_tests ~f:(fun inline_tests ->
    let ppx_info = Ppx_info.make compile_info in
    if not ppx_info.uses_ppx_inline_test then
      Loc.warn lib.buildable.loc
        "This library has an 'inline_tests' field but \
         'ppx_inline_test' is not used.";
    let name = lib.name ^ "_test_runner" in
    let main_module_filename = name ^ ".ml-gen" in
    let main_module_name = String.capitalize_ascii name in
    let modules =
      String_map.singleton main_module_name
        { Module.
          name = main_module_name
        ; impl = Some { name   = main_module_filename
                      ; syntax = OCaml
                      }
        ; intf = None
        ; obj_name = ""
        }
    in

    SC.add_rule sctx (
      Build.write_file (Path.relative dir main_module_filename)
        "let () = Ppx_inline_test_lib.Runtime.exit ()");
    ignore (
      Exe.build_and_link sctx
        ~loc:lib.buildable.loc
        ~dir
        ~program:{ name; main_module_name }
        ~modules
        ~scope
        ~linkages:[Exe.Linkage.native_or_custom (SC.context sctx)]
        ~libraries:
          (List.map ~f:Lib_dep.direct (
             [lib.name]
             @ (if ppx_info.uses_ppx_expect then
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
         ~scope
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
             (String_map.values lib_modules
              |> List.concat_map ~f:(fun m ->
                [ Module.file m ~dir Impl
                ; Module.file m ~dir Intf
                ])
              |> List.filter_map ~f:(fun x -> x)
              |> List.map ~f:(fun fn ->
                A.diff ~optional:true
                  fn (Path.extend_basename fn ~suffix:".corrected")))))))

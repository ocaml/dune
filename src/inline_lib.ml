open Import
open Jbuild
open Build.O
open! No_io

type rule =
  { exe: Jbuild.Executables.t
  ; alias_name: string
  ; alias_action: (unit, Action.t) Build.t
  ; alias_stamp: Sexp.t
  ; all_modules: Module.t String_map.t
  ; gen_source : (unit, Action.t) Build.t
  }

module Test_lib = struct
  type t =
    | Ppx_expect
    | Ppx_inline_test

  let of_lib_name = function
    | "ppx_inline_test" -> [Ppx_inline_test]
    | "ppx_expect" -> [Ppx_expect]
    | "ppx_jane" -> [Ppx_inline_test; Ppx_expect]
    | _ -> []

  module Set = Set.Make(struct
      type t' = t
      type t = t'
      let compare (x : t) (y : t) =
        match x, y with
        | Ppx_expect, Ppx_expect
        | Ppx_inline_test, Ppx_inline_test -> 0
        | Ppx_inline_test, Ppx_expect -> 1
        | Ppx_expect, Ppx_inline_test -> -1
    end)
end

let setup_rules test_libs ~sctx ~dir ~(lib : Jbuild.Library.t) ~scope =
  let name = lib.name ^ "_test_runner" in
  let module_filename = name ^ ".ml-gen" in
  let module_name = String.capitalize_ascii name in
  let exe_stanza =
    { Jbuild.Executables.names = [Loc.none, name]
    ; link_executables = true
    ; link_flags = Ordered_set_lang.Unexpanded.t (
        Sexp.add_loc ~loc:Loc.none (List [Atom "-linkall"])
      )
    ; modes = Mode.Dict.Set.all
    ; buildable =
        { Buildable.
          loc = Loc.none
        ; modules =
            Ordered_set_lang.t (List (Loc.none, [Atom (Loc.none, module_name)]))
        ; modules_without_implementation = Ordered_set_lang.standard
        ; libraries =
            List.map ~f:Lib_dep.direct (
              [lib.name]
              @ (if Test_lib.Set.mem Test_lib.Ppx_expect test_libs then
                   ["ppx_expect.evaluator"]
                 else
                   [])
              @ ["ppx_inline_test.runner.lib"]
          )
        ; preprocess = Preprocess_map.no_preprocessing
        ; preprocessor_deps = []
        ; flags = Ordered_set_lang.Unexpanded.standard
        ; ocamlc_flags = Ordered_set_lang.Unexpanded.standard
        ; ocamlopt_flags = Ordered_set_lang.Unexpanded.standard
        ; js_of_ocaml = Js_of_ocaml.default
        ; gen_dot_merlin = false
        ; lint = Jbuild.Lint.no_lint
        }
    } in
  { exe = exe_stanza
  ; alias_name = "runtest"
  ; alias_stamp = Sexp.List [Atom "ppx-runner"; Atom name]
  ; alias_action =
      (let module A = Action in
       let exe = Path.relative dir (name ^ ".exe") in
       Build.path exe >>>
       Super_context.Deps.interpret sctx ~scope ~dir lib.inline_tests.deps
       >>^ fun _ ->
       A.chdir dir
         (A.run (Ok exe) ["inline-test-runner"; lib.name]))
  ; gen_source = (
      Build.write_file (Path.relative dir module_filename)
        "let () = Ppx_inline_test_lib.Runtime.exit ()"
    )
  ; all_modules =
      (String_map.of_alist_exn
         [ module_name
         , { Module.
             name = module_name
           ; impl = Some { Module.File.
                           name = module_filename
                         ; syntax = Module.Syntax.OCaml
                         }
           ; intf = None
           ; obj_name = "" } ])
  }
;;

let rule sctx ~(lib : Jbuild.Library.t) ~dir ~scope =
  let test_config =
    Jbuild.Preprocess_map.pps lib.buildable.preprocess
    |> List.rev_map ~f:Jbuild.Pp.to_string
    |> List.concat_map ~f:Test_lib.of_lib_name
    |> Test_lib.Set.of_list in
  if Test_lib.Set.is_empty test_config then
    None
  else
    Some (setup_rules test_config ~sctx ~dir ~lib ~scope)

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

module Ppx_info = struct
  type t =
    { uses_expect: bool
    ; uses_inline_test: bool
    }

  let need_runner t = t.uses_inline_test || t.uses_expect

  let of_lib ~scope (lib : Jbuild.Library.t) =
    let user_ppx =
      Jbuild.Preprocess_map.pps lib.buildable.preprocess
      |> List.rev_map ~f:(fun pp -> Lib_dep.direct (Jbuild.Pp.to_string pp)) in
    (* we should early terminate once both uses_expect and uses_inline_test are
       true *)
    match
      Lib_db.Scope.fold_transitive_closure
        scope
        user_ppx
        ~init:{ uses_expect = false
              ; uses_inline_test = false
              }
        ~f:(fun (lib : Lib.t) acc ~required_by:_ ->
          let is_name name = Lib.exists_name lib ~f:((=) name) in
          { uses_expect
            = acc.uses_expect || is_name "ppx_expect"
          ; uses_inline_test
            = acc.uses_inline_test || is_name "ppx_inline_test"
          }
        ) with
    | res -> Some res
    | exception _ -> None
end

let setup_rules (config : Ppx_info.t) ~sctx ~dir ~(lib : Jbuild.Library.t)
      ~scope =
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
              @ (if config.uses_expect then
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
  let ppx_info = Ppx_info.of_lib ~scope lib in
  match ppx_info with
  | None -> None
  | Some ppx_info ->
    if Ppx_info.need_runner ppx_info then
      Some (setup_rules ppx_info ~sctx ~dir ~lib ~scope:scope.data)
    else
      None

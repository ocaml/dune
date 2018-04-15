open Import
open Build.O
open Jbuild

module SC = Super_context

let pp_fname fn =
  let fn, ext = Filename.split_extension fn in
  (* We need to to put the .pp before the .ml so that the compiler realises that
     [foo.pp.mli] is the interface for [foo.pp.ml] *)
  fn ^ ".pp" ^ ext

let pped_module ~dir (m : Module.t) ~f =
  let pped_file (kind : Ml_kind.t) (file : Module.File.t) =
    let pp_fname = pp_fname file.name in
    f kind (Path.relative dir file.name) (Path.relative dir pp_fname);
    {file with name = pp_fname}
  in
  { m with
    impl = Option.map m.impl ~f:(pped_file Impl)
  ; intf = Option.map m.intf ~f:(pped_file Intf)
  }

let migrate_driver_main = "ocaml-migrate-parsetree.driver-main"

let ppx_exe sctx ~key =
  Path.relative (SC.build_dir sctx) (".ppx/" ^ key ^ "/ppx.exe")

let build_ppx_driver sctx ~lib_db ~dep_kind ~target pps =
  let ctx = SC.context sctx in
  let mode = Context.best_mode ctx in
  let compiler = Option.value_exn (Context.compiler ctx mode) in
  let pps = pps @ [Pp.of_string migrate_driver_main] in
  let driver, libs =
    let resolved_pps =
      Lib.DB.resolve_pps lib_db
        (List.map pps ~f:(fun x -> (Loc.none, x)))
      (* Extend the dependency stack as we don't have locations at
         this point *)
      |> Result.map_error ~f:(fun e ->
        Dep_path.prepend_exn e
          (Preprocess (pps : Jbuild.Pp.t list :> string list)))
    in
    let driver =
      match resolved_pps with
      | Ok    l -> List.last l
      | Error _ -> None
    in
    (driver,
     Result.bind resolved_pps ~f:Lib.closure
     |> Result.map ~f:Build.return
     |> Build.of_result)
  in
  let libs =
    Build.record_lib_deps ~kind:dep_kind
      (List.map pps ~f:(fun pp -> Lib_dep.of_pp (Loc.none, pp)))
    >>>
    libs
  in
  let libs =
    (* Put the driver back at the end, just before migrate_driver_main *)
    match driver with
    | None -> libs
    | Some driver ->
      libs >>^ fun libs ->
      let libs, drivers =
        List.partition_map libs ~f:(fun lib ->
          if lib == driver || Lib.name lib = migrate_driver_main then
            Right lib
          else
            Left lib)
      in
      let user_driver, migrate_driver =
        List.partition_map drivers ~f:(fun lib ->
          if Lib.name lib = migrate_driver_main then
            Right lib
          else
            Left lib)
      in
      libs @ user_driver @ migrate_driver
  in
  (* Provide a better error for migrate_driver_main given that this
     is an implicit dependency *)
  let libs =
    match Lib.DB.available lib_db migrate_driver_main with
    | false ->
      Build.fail { fail = fun () ->
        die "@{<error>Error@}: I couldn't find '%s'.\n\
             I need this library in order to use ppx rewriters.\n\
             See the manual for details.\n\
             Hint: opam install ocaml-migrate-parsetree"
          migrate_driver_main
      }
      >>>
      libs
    | true ->
      libs
  in
  SC.add_rule sctx
    (libs
     >>>
     Build.dyn_paths
       (Build.arr
          (Lib.L.archive_files ~mode ~ext_lib:ctx.ext_lib))
     >>>
     Build.run ~context:ctx (Ok compiler)
       [ A "-o" ; Target target
       ; Dyn (Lib.L.link_flags ~mode ~stdlib_dir:ctx.stdlib_dir)
       ])

let gen_rules sctx components =
  match components with
  | [key] ->
    let exe = ppx_exe sctx ~key in
    let (key, lib_db) = SC.Scope_key.of_string sctx key in
    let names =
      match key with
      | "+none+" -> []
      | _ -> String.split key ~on:'+'
    in
    let names =
      match List.rev names with
      | [] -> []
      | driver :: rest -> List.sort rest ~compare:String.compare @ [driver]
    in
    let pps = List.map names ~f:Jbuild.Pp.of_string in
    build_ppx_driver sctx pps ~lib_db ~dep_kind:Required ~target:exe
  | _ -> ()

let get_ppx_driver sctx ~scope pps =
  let driver, names =
    match List.rev_map pps ~f:(fun (_loc, pp) -> Pp.to_string pp) with
    | [] -> (None, [])
    | driver :: rest -> (Some driver, rest)
  in
  let sctx = SC.host sctx in
  let name_and_scope_for_key name =
    match Lib.DB.find (Scope.libs scope) name with
    | Error _ ->
      (* XXX unknown but assume it's public *)
      (name, None)
    | Ok lib ->
      (Lib.name lib,
       match Lib.status lib with
       | Private scope_name   -> Some scope_name
       | Public _ | Installed -> None)
  in
  let driver, scope_for_key =
    match driver with
    | None -> (None, None)
    | Some driver ->
      let name, scope_for_key = name_and_scope_for_key driver in
      (Some name, scope_for_key)
  in
  let names, scope_for_key =
    List.fold_left names ~init:([], scope_for_key)
      ~f:(fun (names, scope_for_key) lib ->
        let name, scope_for_key' = name_and_scope_for_key lib in
        (name :: names,
         match scope_for_key, scope_for_key' with
         | Some a, Some b -> assert (a = b); scope_for_key
         | Some _, None   -> scope_for_key
         | None  , Some _ -> scope_for_key'
         | None  , None   -> None))
  in
  let names = List.sort ~compare:String.compare names in
  let names =
    match driver with
    | None        -> names
    | Some driver -> names @ [driver]
  in
  let key =
    match names with
    | [] -> "+none+"
    | _  -> String.concat names ~sep:"+"
  in
  let key =
    match scope_for_key with
    | None            -> key
    | Some scope_name -> SC.Scope_key.to_string key scope_name
  in
  let sctx = SC.host sctx in
  ppx_exe sctx ~key

let target_var = String_with_vars.virt_var __POS__ "@"
let root_var   = String_with_vars.virt_var __POS__ "ROOT"

let cookie_library_name lib_name =
  match lib_name with
  | None -> []
  | Some name -> ["--cookie"; sprintf "library-name=%S" name]

(* Generate rules for the reason modules in [modules] and return a
   a new module with only OCaml sources *)
let setup_reason_rules sctx ~dir (m : Module.t) =
  let ctx = SC.context sctx in
  let refmt =
    Artifacts.binary (SC.artifacts sctx) "refmt" ~hint:"opam install reason" in
  let rule src target =
    let src_path = Path.relative dir src in
    Build.run ~context:ctx refmt
      [ A "--print"
      ; A "binary"
      ; Dep src_path ]
      ~stdout_to:(Path.relative dir target) in
  let to_ml (f : Module.File.t) =
    match f.syntax with
    | OCaml  -> f
    | Reason ->
      let ml = Module.File.to_ocaml f in
      SC.add_rule sctx (rule f.name ml.name);
      ml
  in
  { m with
    impl = Option.map m.impl ~f:to_ml
  ; intf = Option.map m.intf ~f:to_ml
  }

let uses_ppx_driver ~pps =
  match (List.last pps : (_ * Pp.t) option :> (_ * string) option) with
  | Some (_, ("ppx_driver.runner" | "ppxlib.runner")) -> true
  | Some _ | None -> false

let promote_correction ~uses_ppx_driver fn build =
  if not uses_ppx_driver then
    build
  else
    Build.progn
      [ build
      ; Build.return
          (Action.diff ~optional:true
             fn
             (Path.extend_basename fn ~suffix:".ppx-corrected"))
      ]

let lint_module sctx ~dir ~dep_kind ~lint ~lib_name ~scope = Staged.stage (
  let alias = Build_system.Alias.lint ~dir in
  let add_alias fn build =
    SC.add_alias_action sctx alias build
      ~stamp:(List [ Sexp.unsafe_atom_of_string "lint"
                   ; Sexp.To_sexp.(option string) lib_name
                   ; Sexp.atom fn
                   ])
  in
  let lint =
    Per_module.map lint ~f:(function
      | Preprocess.No_preprocessing ->
        (fun ~source:_ ~ast:_ -> ())
      | Action action ->
        (fun ~source ~ast:_ ->
           let action = Action.Unexpanded.Chdir (root_var, action) in
           Module.iter source ~f:(fun _ (src : Module.File.t) ->
             let src_path = Path.relative dir src.name in
             add_alias src.name
               (Build.path src_path
                >>^ (fun _ -> [src_path])
                >>> SC.Action.run sctx
                      action
                      ~dir
                      ~dep_kind
                      ~targets:(Static [])
                      ~scope)))
      | Pps { pps; flags } ->
        let ppx_exe = get_ppx_driver sctx ~scope pps in
        let uses_ppx_driver = uses_ppx_driver ~pps in
        let args : _ Arg_spec.t =
          S [ As flags
            ; As (cookie_library_name lib_name)
            (* This hack is needed until -null is standard:
               https://github.com/ocaml-ppx/ocaml-migrate-parsetree/issues/35
            *)
            ; As (if uses_ppx_driver then
                    [ "-null"; "-diff-cmd"; "-" ]
                  else
                    [])
            ]
        in
        (fun ~source ~ast ->
           Module.iter ast ~f:(fun kind src ->
             let args =
               [ args
               ; Ml_kind.ppx_driver_flag kind
               ; Dep (Path.relative dir src.name)
               ]
             in
             add_alias src.name
               (promote_correction ~uses_ppx_driver
                  (Option.value_exn (Module.file ~dir source kind))
                  (Build.run ~context:(SC.context sctx) (Ok ppx_exe) args))
           )))
  in
  fun ~(source : Module.t) ~ast ->
    Per_module.get lint source.name ~source ~ast)

(* Generate rules to build the .pp files and return a new module map
   where all filenames point to the .pp files *)
let pp_and_lint_modules sctx ~dir ~dep_kind ~modules ~lint ~preprocess
      ~preprocessor_deps ~lib_name ~scope =
  let preprocessor_deps =
    Build.memoize "preprocessor deps" preprocessor_deps
  in
  let lint_module =
    Staged.unstage (lint_module sctx ~dir ~dep_kind ~lint ~lib_name ~scope)
  in
  let preprocess =
    Per_module.map preprocess ~f:(function
      | Preprocess.No_preprocessing ->
        (fun m ->
           let ast = setup_reason_rules sctx ~dir m in
           lint_module ~ast ~source:m;
           ast)
      | Action action ->
        (fun m ->
           let ast =
             pped_module m ~dir ~f:(fun _kind src dst ->
               SC.add_rule sctx
                 (preprocessor_deps
                  >>>
                  Build.path src
                  >>^ (fun _ -> [src])
                  >>>
                  SC.Action.run sctx
                    (Redirect
                       (Stdout,
                        target_var,
                        Chdir (root_var,
                               action)))
                    ~dir
                    ~dep_kind
                    ~targets:(Static [dst])
                    ~scope))
             |> setup_reason_rules sctx ~dir in
           lint_module ~ast ~source:m;
           ast)
      | Pps { pps; flags } ->
        let ppx_exe = get_ppx_driver sctx ~scope pps in
        let uses_ppx_driver = uses_ppx_driver ~pps in
        let args : _ Arg_spec.t =
          S [ As flags
            ; A "--dump-ast"
            ; As (cookie_library_name lib_name)
            ; As (if uses_ppx_driver then ["-diff-cmd"; "-"] else [])
            ]
        in
        (fun m ->
           let ast = setup_reason_rules sctx ~dir m in
           lint_module ~ast ~source:m;
           pped_module ast ~dir ~f:(fun kind src dst ->
             SC.add_rule sctx
               (promote_correction ~uses_ppx_driver
                  (Option.value_exn (Module.file m ~dir kind))
                  (preprocessor_deps
                   >>>
                   Build.run ~context:(SC.context sctx)
                     (Ok ppx_exe)
                     [ args
                     ; A "-o"; Target dst
                     ; Ml_kind.ppx_driver_flag kind; Dep src
                     ])))))
  in
 Module.Name.Map.map modules ~f:(fun (m : Module.t) ->
    Per_module.get preprocess m.name m)

open Import
open Jbuild
open Build.O
open! No_io

module SC = Super_context

module M = struct
  module Backend = struct
    module Info = struct
      let name = Sub_system_name.make "inline_tests.backend"

      type t =
        { runner_libraries : string list
        ; flags            : Ordered_set_lang.Unexpanded.t
        ; gen              : Action.Unexpanded.t option
        }

      type Jbuild.Sub_system_info.t += T of t

      open Sexp.Of_sexp

      let short = Short_syntax.Not_allowed
      let of_sexp =
        record
          (field "runner_libraries" (list string) ~default:[]
           >>= fun runner_libraries ->
           Ordered_set_lang.Unexpanded.field "flags" >>= fun flags ->
           field_o "gen" Action.Unexpanded.t >>= fun gen ->
           return
             { runner_libraries
             ; flags
             ; gen
             })
    end

    type t =
      { info             : Info.t
      ; runner_libraries : (Lib.t list, Lib.Error.t With_required_by.t) result
      }

    let instantiate db (info : Info.t) =
      { info
      ; runner_libraries =
          Lib.DB.find_many db info.runner_libraries
            ~required_by:[]
      }

    let to_sexp t =
      let open Sexp.To_sexp in
      let runner_libs =
        match t.runner_libraries with
        | Ok l -> List.map l ~f:Lib.name
        | Error e -> raise (Lib.Error e)
      in
      record
        [ "runner_libraries", list atom runner_libs
        ; "flags"           , Ordered_set_lang.Unexpanded.sexp_of_t t.info.flags
        ; "gen"             , option Action.Unexpanded.sexp_of_t t.info.gen
        ]
  end

  module Info = struct
    let name = Sub_system_name.make "inline_tests"

    type t =
      { loc     : Loc.t
      ; deps    : Dep_conf.t list
      ; flags   : Ordered_set_lang.Unexpanded.t
      ; backend : (Loc.t * string) option
      }

    type Jbuild.Sub_system_info.t += T of t

    let empty loc =
      { loc
      ; deps    = []
      ; flags   = Ordered_set_lang.Unexpanded.standard
      ; backend = None
      }

    let loc      t = t.loc
    let backends t = Option.to_list t.backend

    open Sexp.Of_sexp

    let short = Short_syntax.(Located empty)
    let of_sexp =
      record
        (record_loc >>= fun loc ->
         field "deps" (list Dep_conf.t) ~default:[] >>= fun deps ->
         Ordered_set_lang.Unexpanded.field "flags" >>= fun flags ->
         field_o "backend" (located string) >>= fun backend ->
         return
           { loc
           ; deps
           ; flags
           ; backend
           })
  end

  let gen_rules c ~(info:Info.t) ~backends =
    let { Sub_system.Library_compilation_context.
          super_context = sctx
        ; dir
        ; stanza = lib
        ; scope
        ; source_modules
        ; _
        } = c
    in

    let name = sprintf "_%s_test_runner" lib.name in
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

    let extra_vars =
      String_map.singleton "library-name"
        (Action.Var_expansion.Strings ([lib.name], Concat))
    in

    let runner_libs, _ =
      let open Result.O in
      Lib.Compile.make
        (Result.concat_map backends
           ~f:(fun (backend : Backend.t) -> backend.runner_libraries)
         >>= fun libs ->
         Lib.DB.find_many (Scope.libs scope) [lib.name] ~required_by:[]
         >>= fun lib ->
         Ok (lib @ libs))
      |> Super_context.Libs.requires sctx ~loc:info.loc ~dir
           ~has_dot_merlin:false
    in

    (* Generate the runner file *)
    SC.add_rule sctx (
      let target = Path.relative dir main_module_filename in
      let source_modules = String_map.values source_modules in
      let files ml_kind =
        Action.Var_expansion.Paths (
          List.filter_map source_modules ~f:(fun m ->
            Module.file m ~dir ml_kind),
          Split)
      in
      let extra_vars =
        String_map.of_alist_exn
          [ "impl-files", files Impl
          ; "intf-files", files Intf
          ]
      in
      Build.return []
      >>>
      Build.all
        (List.filter_map backends ~f:(fun (backend : Backend.t) ->
           Option.map backend.info.gen ~f:(fun action ->
             SC.Action.run sctx action
               ~extra_vars ~dir ~dep_kind:Required ~targets:Alias ~scope)))
      >>^ (fun actions ->
        Action.with_stdout_to target
          (Action.progn actions))
      >>>
      Build.action_dyn ~targets:[target] ());

    ignore (
      Exe.build_and_link sctx
        ~dir
        ~program:{ name; main_module_name }
        ~modules
        ~scope
        ~linkages:[Exe.Linkage.native_or_custom (SC.context sctx)]
        ~requires:runner_libs
        ~link_flags:(Build.return ["-linkall"])
        ~flags:(Ocaml_flags.of_list ["-w"; "-24"])
      : Path.t);

    let flags =
      let flags =
        List.map backends ~f:(fun backend ->
          backend.Backend.info.flags) @ [info.flags]
      in
      Build.all (
        List.map flags ~f:(fun flags ->
          Super_context.expand_and_eval_set sctx flags
            ~scope
            ~dir
            ~extra_vars
            ~standard:[]))
      >>^ List.concat
    in

    SC.add_alias_action sctx
      (Build_system.Alias.runtest ~dir)
      ~stamp:(List [Atom "ppx-runner"; Atom name])
      (let module A = Action in
       let exe = Path.relative dir (name ^ ".exe") in
       Build.path exe >>>
       Build.fanout
         (Super_context.Deps.interpret sctx info.deps ~dir ~scope)
         flags
       >>^ fun (_deps, flags) ->
       A.chdir dir
         (A.progn
            (A.run (Ok exe) flags ::
             (String_map.values source_modules
              |> List.concat_map ~f:(fun m ->
                [ Module.file m ~dir Impl
                ; Module.file m ~dir Intf
                ])
              |> List.filter_map ~f:(fun x -> x)
              |> List.map ~f:(fun fn ->
                A.diff ~optional:true
                  fn (Path.extend_basename fn ~suffix:".corrected"))))))
end

let () = Sub_system.register_multi_backends (module M)

let linkme = ()

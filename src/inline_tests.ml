open Import
open Jbuild
open Build.O
open! No_io

module SC = Super_context

module Backend = struct
  module M = struct
    module Info = struct
      let name = Sub_system_name.make "inline_tests.backend"

      type t =
        { loc              : Loc.t
        ; runner_libraries : (Loc.t * string) list
        ; flags            : Ordered_set_lang.Unexpanded.t
        ; generate_runner  : Action.Unexpanded.t option
        ; extends          : (Loc.t * string) list option
        }

      type Jbuild.Sub_system_info.t += T of t

      let loc t = t.loc

      open Sexp.Of_sexp

      let short = None
      let parse =
        record
          (record_loc >>= fun loc ->
           field "runner_libraries" (list (located string)) ~default:[]
           >>= fun runner_libraries ->
           Ordered_set_lang.Unexpanded.field "flags" >>= fun flags ->
           field_o "generate_runner" Action.Unexpanded.t
           >>= fun generate_runner ->
           field_o "extends" (list (located string)) >>= fun extends ->
           return
             { loc
             ; runner_libraries
             ; flags
             ; generate_runner
             ; extends
             })

      let parsers =
        Syntax.Versioned_parser.make
          [ (1, 0),
            { Jbuild.Sub_system_info.
              short
            ; parse
            }
          ]
    end

    type t =
      { info             : Info.t
      ; lib              : Lib.t
      ; runner_libraries : (Lib.t list, exn) result
      ; extends          : (    t list, exn) result option
      }

    let desc ~plural = "inline tests backend" ^ if plural then "s" else ""
    let desc_article = "an"

    let lib  t = t.lib
    let deps t = t.extends

    let instantiate ~resolve ~get lib (info : Info.t) =
      { info
      ; lib
      ; runner_libraries = Result.all (List.map info.runner_libraries ~f:resolve)
      ; extends =
          let open Result.O in
          Option.map info.extends
            ~f:(fun l ->
              Result.all
                (List.map l
                   ~f:(fun ((loc, name) as x) ->
                     resolve x >>= fun lib ->
                     match get lib with
                     | None ->
                       Error (Loc.exnf loc "%S is not an %s" name
                                (desc ~plural:false))
                     | Some t -> Ok t)))
      }

    let to_sexp t =
      let open Sexp.To_sexp in
      let lib x = string (Lib.name x) in
      let f x = string (Lib.name x.lib) in
      ((1, 0),
       record
         [ "runner_libraries", list lib (Result.ok_exn t.runner_libraries)
         ; "flags"           , Ordered_set_lang.Unexpanded.sexp_of_t
                                 t.info.flags
         ; "generate_runner" , option Action.Unexpanded.sexp_of_t
                                 t.info.generate_runner
         ; "extends"         , option (list f)
                                 (Option.map t.extends ~f:Result.ok_exn)
         ])
  end
  include M
  include Sub_system.Register_backend(M)
end

include Sub_system.Register_end_point(
  struct
    module Backend = Backend

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
    let backends t = Option.map t.backend ~f:(fun x -> [x])

    open Sexp.Of_sexp

    let short = Some empty
    let parse =
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

    let parsers =
      Syntax.Versioned_parser.make
        [ (1, 0),
          { Jbuild.Sub_system_info.
            short
          ; parse
          }
        ]
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

    let inline_test_dir =
      Path.relative dir (sprintf ".%s.inline-tests" lib.name)
    in

    let name = "run" in
    let main_module_filename = name ^ ".ml" in
    let main_module_name = String.capitalize name in
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
         Lib.DB.find_many (Scope.libs scope) [lib.name]
         >>= fun lib ->
         Ok (lib @ libs))
      |> Super_context.Libs.requires sctx ~dir ~has_dot_merlin:false
    in

    (* Generate the runner file *)
    SC.add_rule sctx (
      let target = Path.relative inline_test_dir main_module_filename in
      let source_modules = String_map.values source_modules in
      let files ml_kind =
        Action.Var_expansion.Paths (
          List.filter_map source_modules ~f:(fun m ->
            Module.file m ~dir ml_kind),
          Split)
      in
      let extra_vars =
        List.fold_left
          [ "impl-files", files Impl
          ; "intf-files", files Intf
          ]
          ~init:extra_vars
          ~f:(fun acc (k, v) -> String_map.add acc k v)
      in
      Build.return []
      >>>
      Build.all
        (List.filter_map backends ~f:(fun (backend : Backend.t) ->
           Option.map backend.info.generate_runner ~f:(fun action ->
             SC.Action.run sctx action
               ~extra_vars ~dir ~dep_kind:Required ~targets:Alias ~scope)))
      >>^ (fun actions ->
        Action.with_stdout_to target
          (Action.progn actions))
      >>>
      Build.action_dyn ~targets:[target] ());

    Exe.build_and_link sctx
      ~dir:inline_test_dir
      ~obj_dir:inline_test_dir
      ~program:{ name; main_module_name }
      ~modules
      ~scope
      ~linkages:[Exe.Linkage.native_or_custom (SC.context sctx)]
      ~requires:runner_libs
      ~link_flags:(Build.return ["-linkall"])
      ~flags:(Ocaml_flags.of_list ["-w"; "-24"]);

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
      ~stamp:(List [ Sexp.unsafe_atom_of_string "ppx-runner"
                   ; Quoted_string name
                   ])
      (let module A = Action in
       let exe = Path.relative inline_test_dir (name ^ ".exe") in
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
end)

let linkme = ()

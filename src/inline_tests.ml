open Import
open Jbuild
open Build.O
open! No_io

module SC = Super_context

module M = struct
  let name = Sub_system_name.make "inline_tests"

  module Backend = struct
    let name = Sub_system_name.make "inline_tests.backend"

    module Info = struct
      type t =
        { runner_libraries : string list
        ; flags            : Ordered_set_lang.Unexpanded.t
        ; ocaml_code       : String_with_vars.t option
        }

      let short = None

      let of_sexp ~lib_name:_ =
        let open Sexp.Of_sexp in
        record
          (field "runner_libraries" (list string) ~default:[]
           >>= fun runner_libraries ->
           Ordered_set_lang.Unexpanded.field "flags" >>= fun flags ->
           field_o "ocaml_code" String_with_vars.t >>= fun ocaml_code ->
           return
             { runner_libraries
             ; flags
             ; ocaml_code
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
        ; "ocaml_code"      , atom t.info.ocaml_code
        ]
  end

  module Info = struct
    type t =
      { loc     : Loc.t
      ; deps    : Dep_conf.t list
      ; flags   : Ordered_set_lang.Unexpanded.t
      ; backend : (Loc.t * string) option
      }

    let empty =
      { loc     = Loc.none
      ; deps    = []
      ; flags   = Ordered_set_lang.Unexpanded.standard
      ; backend = None
      }

    let short = Some empty

    let backend t = t.backend

    let of_sexp =
      let open Sexp.Of_sexp in
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

  let gen_rules sctx ~(info:Info.t) ~backends
        ~dir ~(lib : Jbuild.Library.t) ~scope ~source_modules =
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

    let extra_vars =
      String_map.singleton "library-name"
        (Action.Var_expansion.Strings ([lib.name], Concat))
    in

    let runner_libs, _ =
      Lib.Compile.make
        (Result.List.concat_map backends
           ~f:(fun backend -> backend.runner_libs))
      |> Super_context.Libs.requires sctx ~loc:info.loc ~dir
           ~has_dot_merlin:false
    in

    (* Generate the runner file *)
    SC.add_rule sctx (
      let code =
        String.concat ~sep:""
          (List.map backends ~f:(fun backend ->
             let code =
               Super_context.expand_vars sctx backend.Backend.ocaml_code
                 ~dir ~scope ~extra_vars
             in
             sprintf "let () = (%s)\n" code))
      in
      Build.write_file (Path.relative dir main_module_filename) code);

    ignore (
      Exe.build_and_link sctx
        ~loc:lib.buildable.loc
        ~dir
        ~program:{ name; main_module_name }
        ~modules
        ~scope
        ~linkages:[Exe.Linkage.native_or_custom (SC.context sctx)]
        ~requires
        ~link_flags:(Build.return ["-linkall"])
      : _ * _);

    let flags =
      let flags =
        List.map backends ~f:(fun backend ->
          backend.Backend.flags) @ [info.flags]
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
         (Super_context.Deps.interpret sctx t.deps ~dir ~scope)
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

let () = Sub_system.register (module M)

let linkme = ()

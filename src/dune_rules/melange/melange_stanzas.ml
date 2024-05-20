open Import
open Dune_lang.Decoder

module Emit = struct
  type t =
    { loc : Loc.t
    ; target : string
    ; alias : Alias.Name.t option
    ; module_systems : (Melange.Module_system.t * Filename.Extension.t) list
    ; modules : Stanza_common.Modules_settings.t
    ; emit_stdlib : bool
    ; libraries : Lib_dep.t list
    ; package : Package.t option
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; runtime_deps : Loc.t * Dep_conf.t list
    ; preprocessor_deps : Dep_conf.t list
    ; lint : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
    ; promote : Rule.Promote.t option
    ; compile_flags : Ordered_set_lang.Unexpanded.t
    ; allow_overlapping_dependencies : bool
    ; enabled_if : Blang.t
    ; dune_version : Dune_lang.Syntax.Version.t
    }

  include Stanza.Make (struct
      type nonrec t = t

      include Poly
    end)

  let implicit_alias = Alias.Name.of_string "melange"

  let decode =
    let extension_field = extension in
    let module_systems =
      let module_system =
        enum [ "esm", Melange.Module_system.ESM; "es6", ESM; "commonjs", CommonJS ]
      in
      let+ module_systems =
        repeat
          (pair module_system (located extension_field)
           <|> let+ loc, module_system = located module_system in
               let _, ext = Melange.Module_system.default in
               module_system, (loc, ext))
      in
      let module_systems =
        match
          String.Map.of_list_map module_systems ~f:(fun (ms, (loc, ext)) ->
            ext, (loc, ms))
        with
        | Ok m -> String.Map.to_list_map m ~f:(fun ext (_loc, ms) -> ms, ext)
        | Error (ext, (_, (loc1, _)), (_, (loc2, _))) ->
          let main_message =
            Pp.textf "JavaScript extension %s appears more than once:" ext
          in
          let annots =
            let main = User_message.make ~loc:loc2 [ main_message ] in
            let related =
              [ User_message.make ~loc:loc1 [ Pp.text "Already defined here" ] ]
            in
            User_message.Annots.singleton
              Compound_user_error.annot
              [ Compound_user_error.make ~main ~related ]
          in
          User_error.raise
            ~annots
            ~loc:loc2
            [ main_message
            ; Pp.enumerate ~f:Loc.pp_file_colon_line [ loc1; loc2 ]
            ; Pp.textf "Extensions must be unique per melange.emit stanza"
            ]
            ~hints:
              [ Pp.textf
                  "specify different extensions with (module_systems (<system1> \
                   <extension1>) (<system2> <extension2>))"
              ]
      in
      module_systems
    in
    fields
      (let* loc = loc in
       let+ target =
         let of_string ~loc s =
           match String.is_empty s with
           | true ->
             User_error.raise ~loc [ Pp.textf "The field target can not be empty" ]
           | false ->
             (match Filename.dirname s with
              | "." -> s
              | _ ->
                User_error.raise
                  ~loc
                  [ Pp.textf
                      "The field target must use simple names and can not include paths \
                       to other folders. To emit JavaScript files in another folder, \
                       move the `melange.emit` stanza to that folder"
                  ])
         in
         field "target" (plain_string (fun ~loc s -> of_string ~loc s))
       and+ alias = field_o "alias" Dune_lang.Alias.decode
       and+ module_systems =
         field "module_systems" module_systems ~default:[ Melange.Module_system.default ]
       and+ libraries =
         field "libraries" (Lib_dep.L.decode ~allow_re_export:false) ~default:[]
       and+ package = field_o "package" Stanza_common.Pkg.decode
       and+ runtime_deps =
         field
           "runtime_deps"
           (located (repeat Dep_conf.decode_no_files))
           ~default:(loc, [])
       and+ preprocess, preprocessor_deps = Preprocess.preprocess_fields
       and+ lint = field "lint" Lint.decode ~default:Lint.default
       and+ promote = field_o "promote" Rule_mode_decoder.Promote.decode
       and+ instrumentation = Preprocess.Instrumentation.instrumentation
       and+ compile_flags = Ordered_set_lang.Unexpanded.field "compile_flags"
       and+ allow_overlapping_dependencies = field_b "allow_overlapping_dependencies"
       and+ emit_stdlib = field "emit_stdlib" bool ~default:true
       and+ modules = Stanza_common.Modules_settings.decode
       and+ enabled_if =
         let open Enabled_if in
         let allowed_vars = Any in
         decode ~allowed_vars ~since:None ()
       and+ dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
       let preprocess =
         let init =
           let f libname = Preprocess.With_instrumentation.Ordinary libname in
           Module_name.Per_item.map preprocess ~f:(Preprocess.map ~f)
         in
         List.fold_left instrumentation ~init ~f:Preprocess.Per_module.add_instrumentation
       in
       { loc
       ; target
       ; alias
       ; module_systems
       ; modules
       ; emit_stdlib
       ; libraries
       ; package
       ; preprocess
       ; runtime_deps
       ; preprocessor_deps
       ; lint
       ; promote
       ; compile_flags
       ; allow_overlapping_dependencies
       ; enabled_if
       ; dune_version
       })
  ;;

  let target_dir (emit : t) ~dir = Path.Build.relative dir emit.target
end

let syntax =
  Dune_lang.Syntax.create
    ~name:Dune_project.Melange_syntax.name
    ~desc:"the Melange extension"
    [ (0, 1), `Since (3, 8) ]
;;

let () =
  Dune_project.Extension.register_simple
    syntax
    (return
       [ ( "melange.emit"
         , let+ stanza = Emit.decode in
           [ Emit.make_stanza stanza ] )
       ])
;;

open Import
open Dune_lang.Decoder

module Emit = struct
  type t =
    { loc : Loc.t
    ; target : string
    ; alias : Alias.Name.t option
    ; module_systems : (Melange.Module_system.t * Filename.Extension.t) list
    ; modules : Stanza_common.Modules_settings.t
    ; libraries : Lib_dep.t list
    ; package : Package.t option
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; preprocessor_deps : Dep_conf.t list
    ; promote : Rule.Promote.t option
    ; compile_flags : Ordered_set_lang.Unexpanded.t
    ; allow_overlapping_dependencies : bool
    }

  type Stanza.t += T of t

  let decode_lib =
    let+ loc = loc
    and+ t =
      let allow_re_export = false in
      repeat (Lib_dep.decode ~allow_re_export)
    in
    let add kind name acc =
      match Lib_name.Map.find acc name with
      | None -> Lib_name.Map.set acc name kind
      | Some _present ->
        User_error.raise ~loc
          [ Pp.textf "library %S is present twice" (Lib_name.to_string name) ]
    in
    ignore
      (List.fold_left t ~init:Lib_name.Map.empty ~f:(fun acc x ->
           match x with
           | Lib_dep.Direct (_, s) -> add true s acc
           | Lib_dep.Re_export (_, name) ->
             User_error.raise ~loc
               [ Pp.textf
                   "library %S is using re_export, which is not supported for \
                    melange libraries"
                   (Lib_name.to_string name)
               ]
           | Select _ ->
             User_error.raise ~loc
               [ Pp.textf "select is not supported for melange libraries" ])
        : bool Lib_name.Map.t);
    t

  let decode =
    let extension_field =
      let+ loc, extension = located string in
      if String.is_prefix ~prefix:"." extension then
        User_error.raise ~loc [ Pp.textf "extension must not start with '.'" ];
      "." ^ extension
    in
    let module_systems =
      let module_system =
        enum [ ("es6", Melange.Module_system.Es6); ("commonjs", CommonJs) ]
      in
      let+ module_systems =
        repeat
          (pair module_system (located extension_field)
          <|> let+ loc, module_system = located module_system in
              let _, ext = Melange.Module_system.default in
              (module_system, (loc, ext)))
      in

      let module_systems =
        match
          String.Map.of_list_map module_systems ~f:(fun (ms, (loc, ext)) ->
              (ext, (loc, ms)))
        with
        | Ok m -> String.Map.to_list_map m ~f:(fun ext (_loc, ms) -> (ms, ext))
        | Error (ext, (_, (loc1, _)), (_, (loc2, _))) ->
          let main_message =
            Pp.textf "JavaScript extension %s appears more than once:" ext
          in
          let annots =
            let main = User_message.make ~loc:loc2 [ main_message ] in
            let related =
              [ User_message.make ~loc:loc1 [ Pp.text "Already defined here" ] ]
            in
            User_message.Annots.singleton Compound_user_error.annot
              [ Compound_user_error.make ~main ~related ]
          in
          User_error.raise ~annots ~loc:loc2
            [ main_message
            ; Pp.textf "- %s" (Loc.to_file_colon_line loc1)
            ; Pp.textf "- %s" (Loc.to_file_colon_line loc2)
            ; Pp.textf "Extensions must be unique per melange.emit stanza"
            ]
            ~hints:
              [ Pp.textf
                  "specify different extensions with (module_systems \
                   (<system1> <extension1>) (<system2> <extension2>))"
              ]
      in

      module_systems
    in
    fields
      (let+ loc = loc
       and+ target =
         let of_string ~loc s =
           match String.is_empty s with
           | true ->
             User_error.raise ~loc
               [ Pp.textf "The field target can not be empty" ]
           | false -> (
             match Filename.dirname s with
             | "." -> s
             | _ ->
               User_error.raise ~loc
                 [ Pp.textf
                     "The field target must use simple names and can not \
                      include paths to other folders. To emit JavaScript files \
                      in another folder, move the `melange.emit` stanza to \
                      that folder"
                 ])
         in
         field "target" (plain_string (fun ~loc s -> of_string ~loc s))
       and+ alias = field_o "alias" Alias.Name.decode
       and+ module_systems =
         field "module_systems" module_systems
           ~default:[ Melange.Module_system.default ]
       and+ libraries = field "libraries" decode_lib ~default:[]
       and+ package = field_o "package" Stanza_common.Pkg.decode
       and+ preprocess, preprocessor_deps = Stanza_common.preprocess_fields
       and+ promote = field_o "promote" Rule_mode_decoder.Promote.decode
       and+ loc_instrumentation, instrumentation = Stanza_common.instrumentation
       and+ compile_flags = Ordered_set_lang.Unexpanded.field "compile_flags"
       and+ allow_overlapping_dependencies =
         field_b "allow_overlapping_dependencies"
       and+ modules =
         Stanza_common.Modules_settings.decode ~modules_field_name:"entries"
       in
       let preprocess =
         let init =
           let f libname = Preprocess.With_instrumentation.Ordinary libname in
           Module_name.Per_item.map preprocess ~f:(Preprocess.map ~f)
         in
         List.fold_left instrumentation ~init
           ~f:(fun accu ((backend, flags), deps) ->
             Preprocess.Per_module.add_instrumentation accu
               ~loc:loc_instrumentation ~flags ~deps backend)
       in
       { loc
       ; target
       ; alias
       ; module_systems
       ; modules
       ; libraries
       ; package
       ; preprocess
       ; preprocessor_deps
       ; promote
       ; compile_flags
       ; allow_overlapping_dependencies
       })
end

let syntax =
  Dune_lang.Syntax.create ~name:Dune_project.Melange_syntax.name
    ~desc:"support for Melange compiler"
    [ ((0, 1), `Since (3, 7)) ]

let () =
  Dune_project.Extension.register_simple syntax
    (return [ ("melange.emit", Emit.decode >>| fun x -> [ Emit.T x ]) ])

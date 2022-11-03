open Import
open Dune_lang.Decoder

module Emit = struct
  type t =
    { loc : Loc.t
    ; target : string
    ; module_system : Melange.Module_system.t
    ; entries : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; package : Package.t option
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; preprocessor_deps : Dep_conf.t list
    ; flags : Ocaml_flags.Spec.t
    }

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
       and+ module_system =
         field "module_system"
           (enum [ ("es6", Melange.Module_system.Es6); ("commonjs", CommonJs) ])
       and+ entries = Stanza_common.modules_field "entries"
       and+ libraries = field "libraries" decode_lib ~default:[]
       and+ package = field_o "package" Stanza_common.Pkg.decode
       and+ preprocess, preprocessor_deps = Stanza_common.preprocess_fields
       and+ loc_instrumentation, instrumentation = Stanza_common.instrumentation
       and+ flags = Ocaml_flags.Spec.decode in
       let preprocess =
         let init =
           let f libname = Preprocess.With_instrumentation.Ordinary libname in
           Module_name.Per_item.map preprocess ~f:(Preprocess.map ~f)
         in
         List.fold_left instrumentation
           ~f:(fun accu ((backend, flags), deps) ->
             Preprocess.Per_module.add_instrumentation accu
               ~loc:loc_instrumentation ~flags ~deps backend)
           ~init
       in
       { loc
       ; target
       ; module_system
       ; entries
       ; libraries
       ; package
       ; preprocess
       ; preprocessor_deps
       ; flags
       })
end

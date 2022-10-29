open Import
open Dune_lang.Decoder

module Emit = struct
  type t =
    { loc : Loc.t
    ; target : string
    ; module_system : Melange.Module_system.t
    ; libraries : Lib_dep.t list
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
       and+ target = field "target" string
       and+ module_system =
         field "module_system"
           (enum [ ("es6", Melange.Module_system.Es6); ("commonjs", CommonJs) ])
       and+ libraries = field "libraries" decode_lib ~default:[] in
       { loc; target; module_system; libraries })
end

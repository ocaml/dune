open Import
open Dune_lang.Decoder

module Entry = struct
  type t =
    | Module of Module_name.t
    | Folder of Path.Source.t

  let decode =
    let open Dune_lang.Decoder in
    sum ~force_parens:true
      [ ( "module"
        , let+ module_ = Module_name.decode in
          Module module_ )
      ; ( "folder"
        , let+ str = string in
          let t = Path.Source.of_string str in
          Folder t )
      ]
    <|> let+ module_ = Module_name.decode in
        Module module_
end

module Emit = struct
  type t =
    { loc : Loc.t
    ; target : string
    ; module_system : Melange.Module_system.t
    ; entries : Entry.t list
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
       and+ entries = field "entries" (repeat Entry.decode)
       and+ libraries = field "libraries" decode_lib ~default:[] in
       { loc; target; module_system; entries; libraries })
end

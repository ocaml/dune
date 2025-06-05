open Import

module Object_name = struct
  let decode = Dune_lang.Decoder.string
  let filename t ~ext_obj = t ^ ext_obj

  let build_path t ~error_loc ~ext_obj ~dir =
    Path.Build.relative ~error_loc dir (filename t ~ext_obj)
  ;;
end

(* Associate each object name with its location in the config *)
type t = (Loc.t * string) list

let empty = []
let is_empty = List.is_empty

let decode =
  let open Dune_lang.Decoder in
  let+ t = repeat (located Object_name.decode) in
  (* Check for duplicate names *)
  match String.Map.of_list (List.map t ~f:Tuple.T2.swap) with
  | Ok _ -> t
  | Error (name, loc, loc') ->
    let main_message = sprintf "Duplicate object name: %s." name in
    let annots =
      let main = User_message.make ~loc [ Pp.text main_message ] in
      let related = [ User_message.make ~loc:loc' [ Pp.text "" ] ] in
      User_message.Annots.singleton
        Compound_user_error.annot
        [ Compound_user_error.make ~main ~related ]
    in
    User_error.raise
      ~loc
      ~annots
      [ Pp.textf "%s Already appears at:" main_message
      ; Pp.textf "- %s" (Loc.to_file_colon_line loc')
      ]
;;

let build_paths t ~ext_obj ~dir =
  (* Foreign objects are not mode-dependent *)
  List.map t ~f:(fun (loc, name) ->
    Object_name.build_path name ~error_loc:loc ~ext_obj ~dir |> Path.build)
;;

open Import

type t = (Loc.t * Foreign.Source.t) String.Map.t

let to_list_map t ~f = String.Map.to_list_map t ~f
let make t = t

let object_files t ~dir ~ext_obj =
  String.Map.to_list_map t ~f:(fun c _ -> Path.Build.relative dir (c ^ ext_obj))
;;

let has_cxx_sources (t : t) =
  String.Map.exists t ~f:(fun (_loc, source) ->
    let language = Foreign.Source.language source in
    Foreign_language.(equal `Cxx language))
;;

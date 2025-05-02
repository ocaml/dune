open Import

module Include_dir_without_include = struct
  type t =
    | Dir of String_with_vars.t
    | Lib of Loc.t * Lib_name.t

  let decode : t Dune_lang.Decoder.t =
    let open Dune_lang.Decoder in
    let parse_dir =
      let+ s = String_with_vars.decode in
      Dir s
    in
    let parse_lib =
      sum
        [ ( "lib"
          , let+ loc, lib_name = located Lib_name.decode in
            Lib (loc, lib_name) )
        ]
    in
    parse_dir <|> parse_lib
  ;;
end

module Include_dir = struct
  type t = Include_dir_without_include.t Recursive_include.t

  let decode =
    Recursive_include.decode
      ~base_term:Include_dir_without_include.decode
      ~include_keyword:"include"
      ~non_sexp_behaviour:`Parse_as_base_term
      ~include_allowed_in_versions:(`Since (3, 5))
  ;;

  let expand_include = Recursive_include.expand_include

  module Without_include = Include_dir_without_include
end

type t =
  { loc : Loc.t
  ; language : Foreign_language.t
  ; names : Ordered_set_lang.t
  ; mode : Mode.Select.t
  ; flags : Ordered_set_lang.Unexpanded.t
  ; include_dirs : Include_dir.t list
  ; extra_deps : Dep_conf.t list
  }

let make ~loc ~language ~names ~flags =
  { loc; language; names; flags; include_dirs = []; extra_deps = []; mode = All }
;;

let syntax =
  let name = "mode_specific_stubs" in
  let desc = "syntax extension for mode-specific foreign stubs" in
  Dune_lang.Syntax.create ~name ~desc [ (0, 1), `Since (3, 5) ]
;;

let () = Dune_project.Extension.register_simple syntax (Dune_lang.Decoder.return [])

let decode_stubs ~for_library =
  let open Dune_lang.Decoder in
  let* loc = loc in
  let+ loc_archive_name, archive_name = located (field_o "archive_name" string)
  and+ language = field "language" Foreign_language.decode
  and+ names = Ordered_set_lang.field "names"
  and+ loc_mode, mode =
    located (field_o "mode" (Dune_lang.Syntax.since syntax (0, 1) >>> Mode.decode))
  and+ flags = Ordered_set_lang.Unexpanded.field "flags"
  and+ include_dirs = field ~default:[] "include_dirs" (repeat Include_dir.decode)
  and+ extra_deps = field_o "extra_deps" (repeat Dep_conf.decode) in
  let extra_deps = Option.value ~default:[] extra_deps in
  let () =
    match archive_name with
    | None -> ()
    | Some _ ->
      User_error.raise
        ~loc:loc_archive_name
        [ Pp.textf
            "The field \"archive_name\" is not allowed in the (foreign_stubs ...) \
             stanza. For named foreign archives use the (foreign_library ...) stanza."
        ]
  in
  let () =
    match mode with
    | Some _ when for_library ->
      User_error.raise
        ~loc:loc_mode
        [ Pp.textf "The field \"mode\" is not available for foreign libraries" ]
    | _ -> ()
  in
  let mode = Mode.Select.of_option mode in
  { loc; language; names; mode; flags; include_dirs; extra_deps }
;;

let decode = Dune_lang.Decoder.fields @@ decode_stubs ~for_library:false
let is_mode_dependent t = Mode.Select.is_not_all t.mode

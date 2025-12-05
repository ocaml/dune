open Import
open Memo.O

module Include_term = struct
  type t =
    { context : Univ_map.t
    ; path : String_with_vars.t
    }

  let decode ~include_keyword ~allowed_in_versions =
    let open Dune_lang.Decoder in
    let version_check () =
      match allowed_in_versions with
      | `Since version -> Syntax.since Stanza.syntax version
      | `All -> return ()
    in
    sum
      [ ( include_keyword
        , let+ () = version_check ()
          and+ context = get_all
          and+ path = String_with_vars.decode in
          { context; path } )
      ]
  ;;
end

type 'a t =
  | Base of 'a
  | Include of Include_term.t * 'a decoder Lazy.t

and 'a decoder =
  { decode : 'a t Dune_lang.Decoder.t
  ; non_sexp_behaviour : [ `User_error | `Parse_as_base_term ]
  }

let of_base base = Base base

let decode ~base_term ~include_keyword ~include_allowed_in_versions config =
  let open Dune_lang.Decoder in
  let base_term_decode =
    let+ base_term = base_term in
    Base base_term
  in
  let include_term_decode =
    let+ include_term =
      Include_term.decode
        ~include_keyword
        ~allowed_in_versions:include_allowed_in_versions
    in
    Include (include_term, config)
  in
  include_term_decode <|> base_term_decode
;;

let decode ~base_term ~include_keyword ~include_allowed_in_versions ~non_sexp_behaviour =
  let rec config =
    lazy
      { non_sexp_behaviour
      ; decode = decode ~base_term ~include_keyword ~include_allowed_in_versions config
      }
  in
  (Lazy.force config).decode
;;

let load_included_file config path ~context =
  let+ contents = Build_system.read_file path in
  let config = Lazy.force config in
  match
    Dune_lang.Parser.parse_string contents ~mode:Single ~fname:(Path.to_string path)
  with
  | List (_loc, terms) ->
    List.map terms ~f:(Dune_lang.Decoder.parse config.decode context)
  | other ->
    (match config.non_sexp_behaviour with
     | `Parse_as_base_term -> [ Dune_lang.Decoder.parse config.decode context other ]
     | `User_error ->
       let loc = Dune_sexp.Ast.loc other in
       User_error.raise ~loc [ Pp.textf "Expected list, got:"; Pp.verbatim contents ])
;;

let expand_include (type a) (t : a t) ~expand ~dir =
  let rec expand_include t ~seen =
    match t with
    | Base base_term -> Memo.return [ base_term ]
    | Include ({ context; path = path_sw }, config) ->
      let* path =
        let loc = String_with_vars.loc path_sw in
        expand path_sw >>| Value.to_path ~error_loc:loc ~dir
      in
      if Path.Set.mem seen path
      then
        User_error.raise
          ~loc:(String_with_vars.loc path_sw)
          [ Pp.textf "Include loop detected via: %s" (Path.to_string path) ];
      let seen = Path.Set.add seen path in
      load_included_file config path ~context
      >>= Memo.List.concat_map ~f:(expand_include ~seen)
  in
  expand_include t ~seen:Path.Set.empty
;;

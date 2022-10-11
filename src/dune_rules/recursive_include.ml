open! Import

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
end

module Make (Base_term : sig
  type t

  val decode : t Dune_lang.Decoder.t
end) (Config : sig
  val include_keyword : string

  val include_allowed_in_versions : [ `Since of Syntax.Version.t | `All ]

  val non_sexp_behaviour : [ `User_error | `Parse_as_base_term ]
end) =
struct
  type t =
    | Base of Base_term.t
    | Include of Include_term.t

  let of_base base = Base base

  let decode =
    let open Dune_lang.Decoder in
    let base_term_decode =
      let+ base_term = Base_term.decode in
      Base base_term
    in
    let include_term_decode =
      let+ include_term =
        Include_term.decode ~include_keyword:Config.include_keyword
          ~allowed_in_versions:Config.include_allowed_in_versions
      in
      Include include_term
    in
    include_term_decode <|> base_term_decode

  let load_included_file path ~context =
    let open Memo.O in
    let+ contents = Build_system.read_file (Path.build path) ~f:Io.read_file in
    let ast =
      Dune_lang.Parser.parse_string contents ~mode:Single
        ~fname:(Path.Build.to_string path)
    in
    let parse = Dune_lang.Decoder.parse decode context in
    match ast with
    | List (_loc, terms) -> List.map terms ~f:parse
    | other -> (
      match Config.non_sexp_behaviour with
      | `User_error ->
        let loc = Dune_sexp.Ast.loc other in
        User_error.raise ~loc [ Pp.textf "Expected list, got:\n%s" contents ]
      | `Parse_as_base_term ->
        let term = Dune_lang.Decoder.parse decode context other in
        [ term ])

  let expand_include t ~expand_str ~dir =
    let rec expand_include t ~seen =
      match t with
      | Base base_term -> Memo.return [ base_term ]
      | Include { context; path = path_sw } ->
        let open Memo.O in
        let* path =
          expand_str path_sw
          >>| Path.Build.relative ~error_loc:(String_with_vars.loc path_sw) dir
        in
        if Path.Build.Set.mem seen path then
          User_error.raise
            ~loc:(String_with_vars.loc path_sw)
            [ Pp.textf "Include loop detected via: %s"
                (Path.Build.to_string path)
            ];
        let seen = Path.Build.Set.add seen path in
        let* contents = load_included_file path ~context in
        Memo.List.concat_map contents ~f:(expand_include ~seen)
    in
    expand_include t ~seen:Path.Build.Set.empty
end

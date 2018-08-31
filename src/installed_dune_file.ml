open! Stdune

module Virtual_library = struct
  type t =
    { virtual_modules : Module.t Module.Name.Map.t
    ; lib_name : Lib_name.Local.t
    ; wrapped : bool
    }

  let dgen { virtual_modules ; lib_name ; wrapped } =
    let open Dsexp.To_sexp in
    record
      [ "lib_name", Lib_name.Local.dgen lib_name
      ; "virtual_modules", (list Module.Name.dgen)
                             (Module.Name.Map.keys virtual_modules)
      ; "wrapped", bool wrapped
      ]

  let dparse =
    let open Dsexp.Of_sexp in
    fields (
      let%map lib_name = field "lib_name" Lib_name.Local.dparse
      and virtual_modules = field "virtual_modules" (list Module.Name.dparse)
      and wrapped = field "wrapped" bool
      in
      { lib_name
      ; virtual_modules =
          List.map ~f:(fun name -> (name, Module.make name)) virtual_modules
          |> Module.Name.Map.of_list_exn
      ; wrapped
      }
    )
end

type 'subsystem t =
  { virtual_library : Virtual_library.t option
  ; sub_systems     : 'subsystem Sub_system_name.Map.t
  }

let parse_sub_systems ~parsing_context sexps =
  let (sub_systems, virtual_library) =
    List.fold_left ~init:([], None) sexps
      ~f:(fun (sub_systems, virtual_library) sexp ->
        let name, ver, data = Dsexp.Of_sexp.(
          parse
            (triple (located string)
               (located Syntax.Version.dparse)
               raw) parsing_context) sexp
        in
        match name, virtual_library with
        | (loc, "virtual_library"), Some _ ->
          Errors.fail loc "virtual_library defined twice"
        | (_, "virtual_library"), None ->
          (sub_systems, Some (Dsexp.Of_sexp.parse Virtual_library.dparse
                                Univ_map.empty data))
        | (_loc, name), _ ->
          (* We ignore sub-systems that are not internally known. These
             correspond to plugins that are not in use in the current
             workspace. *)
          ( (match Sub_system_name.get name with
            | Some name -> (name, (Dsexp.Ast.loc sexp, ver, data)) :: sub_systems
            | None -> sub_systems)
          , virtual_library
          ))
  in
  let sub_systems =
    Sub_system_name.Map.of_list sub_systems
    |> (function
      | Ok x -> x
      | Error (name, _, (loc, _, _)) ->
        Errors.fail loc "%S present twice" (Sub_system_name.to_string name))
    |> Sub_system_name.Map.mapi ~f:(fun name (_, version, data) ->
      let (module M) = Dune_file.Sub_system_info.get name in
      Syntax.check_supported M.syntax version;
      let parsing_context =
        (* We set the syntax to the version used when generating this subsystem.
           We cannot do this for jbuild defined subsystems however since those use
           1.0 as the version. Which would correspond to the dune syntax (because
           subsystems share the syntax of the dune lang) *)
        match Univ_map.find_exn parsing_context (Syntax.key Stanza.syntax) with
        | (0, 0) ->
          parsing_context
        | (_, _) ->
          Univ_map.add parsing_context (Syntax.key M.syntax) (snd version)
      in
      M.T (Dsexp.Of_sexp.parse M.parse parsing_context data))
  in
  { sub_systems
  ; virtual_library
  }

let of_sexp =
  let open Dsexp.Of_sexp in
  let version =
    plain_string (fun ~loc -> function
      | "1" -> (0, 0)
      | "2" -> (1, 0)
      | v ->
        of_sexp_errorf loc
          "Unsupported version %S, only version 1 is supported" v)
  in
  sum
    [ "dune",
      (version >>= fun version ->
       set (Syntax.key Stanza.syntax) version
         (let%map parsing_context = get_all
          and sub_systems = list raw
          in
          parse_sub_systems ~parsing_context sub_systems))
    ]

let load fname =
  Io.with_lexbuf_from_file fname ~f:(fun lexbuf ->
    (* Installed dune files are versioned but they don't use the
       [(lang ...)] line which was introduced after. Installed dune
       files in version 1 are using the jbuild syntax and version 2
       are using the dune syntax, so we start by lexing the first
       tokens with the dune lexer until we reach the file version, at
       which point we can decide what lexer to use for the reset of
       the file. *)
    let state = ref 0 in
    let lexer = ref Dsexp.Lexer.token in
    let lexer lb =
      let token : Dsexp.Lexer.Token.t = !lexer lb in
      (match !state, token with
       | 0, Lparen -> state := 1
       | 1, Atom (A "dune") -> state := 2
       | 2, Atom (A "1") -> state := 3; lexer := Dsexp.Lexer.jbuild_token
       | 2, Atom (A "2") -> state := 3; lexer := Dsexp.Lexer.token
       | 2, Atom (A version) ->
         Errors.fail (Loc.of_lexbuf lexbuf) "Unsupported version %S" version
       | 3, _ -> ()
       | _ ->
         Errors.fail (Loc.of_lexbuf lexbuf)
           "This <lib>.dune file looks invalid, it should \
            contain a S-expression of the form (dune x.y ..)"
      );
      token
    in
    Dsexp.Of_sexp.parse of_sexp Univ_map.empty
      (Dsexp.Parser.parse ~lexer ~mode:Single lexbuf))

let gen ~(dune_version : Syntax.Version.t) { sub_systems ; virtual_library } =
  let sexps =
    Sub_system_name.Map.to_list sub_systems
    |> List.map ~f:(fun (name, (ver, conf)) ->
      let (module M) = Dune_file.Sub_system_info.get name in
      Dsexp.List [ Dsexp.atom (Sub_system_name.to_string name)
                 ; Syntax.Version.dgen ver
                 ; conf
                 ])
  in
  let sexps =
    match virtual_library with
    | None -> sexps
    | Some virtual_library ->
      [ Dsexp.unsafe_atom_of_string "virtual_library"
      ; Dsexp.unsafe_atom_of_string "1.0"
      ; Virtual_library.dgen virtual_library
      ] @ sexps
  in
  Dsexp.List
    [ Dsexp.unsafe_atom_of_string "dune"
    ; Dsexp.unsafe_atom_of_string
        (match dune_version with
         | (0, 0) -> "1"
         | (x, _) when x >= 1 -> "2"
         | (_, _) ->
           Exn.code_error "Cannot generate dune with unknown version"
             ["dune_version", Syntax.Version.to_sexp dune_version])
    ; List sexps
    ]

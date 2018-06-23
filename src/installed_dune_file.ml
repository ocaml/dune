open Import

let parse_sub_systems sexps =
  List.filter_map sexps ~f:(fun sexp ->
    let name, ver, data =
      Sexp.Of_sexp.(parse (triple string (located Syntax.Version.t) raw)
                      Univ_map.empty) sexp
    in
    match Sub_system_name.get name with
    | None ->
      (* We ignore sub-systems that are not internally known. These
         correspond to plugins that are not in use in the current
         workspace. *)
      None
    | Some name -> Some (name, (Sexp.Ast.loc sexp, ver, data)))
  |> Sub_system_name.Map.of_list
  |> (function
    | Ok x -> x
    | Error (name, _, (loc, _, _)) ->
      Loc.fail loc "%S present twice" (Sub_system_name.to_string name))
  |> Sub_system_name.Map.mapi ~f:(fun name (_, version, data) ->
    let (module M) = Jbuild.Sub_system_info.get name in
    Syntax.check_supported M.syntax version;
    let parsing_context =
      Univ_map.singleton (Syntax.key M.syntax)
        (* This is wrong, see #909 *)
        (0, 0)
    in
    M.T (Sexp.Of_sexp.parse M.parse parsing_context data))

let of_sexp =
  let open Sexp.Of_sexp in
  let version =
    plain_string (fun ~loc -> function
      | "1" -> ()
      | _ ->
        of_sexp_errorf loc
          "Unsupported version, only version 1 is supported")
  in
  sum
    [ "dune",
      (version >>= fun () ->
       list raw >>| fun l ->
       parse_sub_systems l)
    ]

let load fname =
  Sexp.Of_sexp.parse of_sexp Univ_map.empty (Io.Sexp.load ~mode:Single fname)

let gen confs =
  let sexps =
    Sub_system_name.Map.to_list confs
    |> List.map ~f:(fun (name, (ver, conf)) ->
      let (module M) = Jbuild.Sub_system_info.get name in
      Sexp.List [ Sexp.atom (Sub_system_name.to_string name)
                ; Syntax.Version.sexp_of_t ver
                ; conf
                ])
  in
  Sexp.List
    [ Sexp.unsafe_atom_of_string "dune"
    ; Sexp.unsafe_atom_of_string "1"
    ; List sexps
    ]

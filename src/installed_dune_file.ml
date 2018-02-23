open Import

let parse_sub_systems sexps =
  List.filter_map sexps ~f:(fun sexp ->
    let name, ver, data =
      Sexp.Of_sexp.(triple string (located Syntax.Version.t_of_sexp) raw) sexp
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
    let vloc, ver = version in
    let parser =
      Syntax.Versioned_parser.find_exn M.parsers ~loc:vloc
        ~data_version:ver
    in
    M.T (parser.parse data))

let of_sexp =
  let open Sexp.Of_sexp in
  let version sexp =
    match string sexp with
    | "1" -> ()
    | _  ->
      of_sexp_error sexp "Unsupported version, only version 1 is supported"
  in
  sum
    [ cstr "dune" (version @> list raw @> nil)
        (fun () l -> parse_sub_systems l)
    ]

let load ~fname = of_sexp (Sexp.load ~mode:Single ~fname)

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

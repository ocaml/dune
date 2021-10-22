open Import

type 'path context =
  { current_file : 'path
  ; include_stack : (Loc.t * 'path) list
  }

let get_current_file c = c.current_file

let in_file file = { current_file = file; include_stack = [] }

let error ~to_string_maybe_quoted { current_file = file; include_stack } =
  let last, rest =
    match include_stack with
    | [] -> assert false
    | last :: rest -> (last, rest)
  in
  let loc = fst (Option.value (List.last rest) ~default:last) in
  let line_loc (loc, file) =
    sprintf "%s:%d" (to_string_maybe_quoted file) loc.Loc.start.pos_lnum
  in
  User_error.raise ~loc
    [ Pp.text "Recursive inclusion of dune files detected:"
    ; Pp.textf "File %s is included from %s"
        (to_string_maybe_quoted file)
        (line_loc last)
    ; Pp.vbox
        (Pp.concat_map rest ~sep:Pp.cut ~f:(fun x ->
             Pp.box ~indent:3
               (Pp.seq (Pp.verbatim "-> ")
                  (Pp.textf "included from %s" (line_loc x)))))
    ]

let load_sexps ~context:{ current_file; include_stack } (loc, fn) =
  let include_stack = (loc, current_file) :: include_stack in
  let dir = Path.Source.parent_exn current_file in
  let current_file = Path.Source.relative dir fn in
  if not (Path.Untracked.exists (Path.source current_file)) then
    User_error.raise ~loc
      [ Pp.textf "File %s doesn't exist in source tree."
          (Path.Source.to_string_maybe_quoted current_file)
      ];
  if
    List.exists include_stack ~f:(fun (_, f) ->
        Path.Source.equal f current_file)
  then
    error ~to_string_maybe_quoted:Path.Source.to_string_maybe_quoted
      { current_file; include_stack };
  let sexps = Dune_lang.Parser.load (Path.source current_file) ~mode:Many in
  (sexps, { current_file; include_stack })

let get_include_path_generated ~context:{ current_file; include_stack } (loc, fn)
    =
  let include_stack = (loc, current_file) :: include_stack in
  let dir = Path.Build.parent_exn current_file in
  let current_file = Path.Build.relative dir fn in
  if
    List.exists include_stack ~f:(fun (_, f) -> Path.Build.equal f current_file)
  then
    error ~to_string_maybe_quoted:Path.Build.to_string_maybe_quoted
      { current_file; include_stack };
  { current_file; include_stack }

let load_sexps_generated ~context =
  let sexps =
    Dune_lang.Parser.load (Path.build context.current_file) ~mode:Many
  in
  sexps

let load_sexps_source ~loc ~context =
  let source_file = Path.Build.drop_build_context_exn context.current_file in
  if not (Path.Untracked.exists (Path.source source_file)) then
    User_error.raise ~loc
      [ Pp.textf "File %s doesn't exist in source tree."
          (Path.Source.to_string_maybe_quoted source_file)
      ];
  let sexps = Dune_lang.Parser.load (Path.source source_file) ~mode:Many in
  sexps

open Import

let syntax =
  Dune_lang.Syntax.create ~name:"generated_include" ~experimental:true
    ~desc:"experimental feature for allowing to include generated dune file"
    [ ((0, 1), `Since (3, 0)) ]

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

let load_sexps ~context:{ current_file; include_stack } ~generation_authorized
    (loc, fn) =
  let include_stack = (loc, current_file) :: include_stack in
  let dir = Path.Source.parent_exn current_file in
  let current_file = Path.Source.relative dir fn in
  if not (Path.Untracked.exists (Path.source current_file)) then
    if generation_authorized then
      None
    else
      User_error.raise ~loc
        [ Pp.textf "File %s doesn't exist in source tree."
            (Path.Source.to_string_maybe_quoted current_file)
        ]
  else (
    if
      List.exists include_stack ~f:(fun (_, f) ->
          Path.Source.equal f current_file)
    then
      error ~to_string_maybe_quoted:Path.Source.to_string_maybe_quoted
        { current_file; include_stack };
    let sexps = Dune_lang.Parser.load (Path.source current_file) ~mode:Many in
    Some (sexps, { current_file; include_stack })
  )

let load_sexps_generated
    ~(read_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.Build.t)
    ~(file_exists : Path.Source.t -> bool Memo.Build.t)
    ~context:{ current_file; include_stack } (loc, fn) =
  let include_stack = (loc, current_file) :: include_stack in
  let dir = Path.Build.parent_exn current_file in
  let current_file = Path.Build.relative dir fn in
  if
    List.exists include_stack ~f:(fun (_, f) -> Path.Build.equal f current_file)
  then
    error ~to_string_maybe_quoted:Path.Build.to_string_maybe_quoted
      { current_file; include_stack };
  let context = { current_file; include_stack } in
  let open Memo.Build.O in
  let* path =
    let source_path = Path.Build.drop_build_context_exn context.current_file in
    let+ exists = file_exists source_path in
    if exists then
      Path.source source_path
    else
      Path.build context.current_file
  in
  let+ sexp = read_file path ~f:(Dune_lang.Parser.load ~mode:Many) in
  (sexp, context)

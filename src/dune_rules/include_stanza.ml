open Import

type context =
  { current_file : Path.Source.t
  ; include_stack : (Loc.t * Path.Source.t) list
  }

let in_file file = { current_file = file; include_stack = [] }

let error { current_file = file; include_stack } =
  let last, rest =
    match include_stack with
    | [] -> assert false
    | last :: rest -> (last, rest)
  in
  let loc = fst (Option.value (List.last rest) ~default:last) in
  let line_loc (loc, file) =
    sprintf "%s:%d"
      (Path.Source.to_string_maybe_quoted file)
      loc.Loc.start.pos_lnum
  in
  User_error.raise ~loc
    [ Pp.text "Recursive inclusion of dune files detected:"
    ; Pp.textf "File %s is included from %s"
        (Path.Source.to_string_maybe_quoted file)
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
  let open Memo.O in
  let* exists = Fs_memo.file_exists (In_source_dir current_file) in
  if not exists then
    User_error.raise ~loc
      [ Pp.textf "File %s doesn't exist."
          (Path.Source.to_string_maybe_quoted current_file)
      ];
  if
    List.exists include_stack ~f:(fun (_, f) ->
        Path.Source.equal f current_file)
  then error { current_file; include_stack };
  let+ sexps =
    Fs_memo.with_lexbuf_from_file (In_source_dir current_file)
      ~f:(Dune_lang.Parser.parse ~mode:Many)
  in
  (sexps, { current_file; include_stack })

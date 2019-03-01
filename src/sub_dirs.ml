open! Stdune

type 'set t =
  { dirs : 'set
  ; data_only : 'set
  }

let default =
  let standard_dirs =
    Predicate_lang.of_pred (function
      | "" -> false
      | s -> s.[0] <> '.' && s.[0] <> '_')
  in
  { dirs = standard_dirs
  ; data_only = Predicate_lang.empty
  }

let make ~dirs ~data_only ~ignored_sub_dirs =
  let dirs = Option.value dirs ~default:default.dirs in
  let data_only =
    let data_only = Option.value data_only ~default:default.data_only in
    Predicate_lang.union (data_only :: ignored_sub_dirs)
  in
  { dirs ; data_only }

let add_data_only_dirs t ~dirs =
  { t with data_only =
             Predicate_lang.union
               [t.data_only; (Predicate_lang.of_string_set dirs)] }

let eval t ~dirs =
  let dirs = Predicate_lang.filter t.dirs ~standard:default.dirs dirs in
  let data_only =
    Predicate_lang.filter t.data_only ~standard:default.data_only dirs
    |> String.Set.of_list
  in
  let dirs = String.Set.of_list dirs in
  { dirs
  ; data_only
  }

module Status = struct
  type t = Ignored | Data_only | Normal
end

let status t ~dir =
  match String.Set.mem t.dirs dir, String.Set.mem t.data_only dir with
  | true, false  -> Status.Normal
  | true, true   -> Data_only
  | false, false -> Ignored
  | false, true  -> assert false

let decode =
  let open Stanza.Decoder in
  let ignored_sub_dirs =
    let open Dune_lang.Decoder in
    let ignored =
      plain_string (fun ~loc dn ->
        if Filename.dirname dn <> Filename.current_dir_name ||
           match dn with
           | "" | "." | ".." -> true
           | _ -> false
        then
          of_sexp_errorf loc "Invalid sub-directory name %S" dn
        else
          dn)
      |> list
      >>| (fun l ->
        Predicate_lang.of_string_set (String.Set.of_list l))
    in
    let+ version = Syntax.get_exn Stanza.syntax
    and+ (loc, ignored) = located ignored
    in
    if version >= (1, 6) then begin
      Errors.warn loc
        "ignored_subdirs is deprecated in 1.6. Use dirs to specify \
         visible directories or data_only_dirs for ignoring only dune \
         files."
    end;
    ignored
  in
  let plang =
    Syntax.since Stanza.syntax (1, 6) >>>
    Predicate_lang.decode
  in
  let decode =
    let+ dirs = field_o "dirs" (located plang)
    and+ data_only = field_o "data_only_dirs" (located plang)
    and+ ignored_sub_dirs = multi_field "ignored_subdirs" ignored_sub_dirs
    and+ rest = leftover_fields
    in
    match data_only, dirs, ignored_sub_dirs with
    | None, Some (loc, _), _::_ ->
      Errors.fail loc
        "Cannot have both dirs and ignored_subdirs \
         stanza in a dune file. "
    | Some (loc, _), None, _::_ ->
      Errors.fail loc
        "Cannot have both data_only_dirs and ignored_subdirs \
         stanza in a dune file. "
    | _ ->
      let dirs = Option.map ~f:snd dirs in
      let data_only = Option.map ~f:snd data_only in
      ( make ~dirs ~data_only ~ignored_sub_dirs
      , rest
      )
  in
  enter (fields decode)

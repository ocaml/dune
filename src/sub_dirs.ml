open! Stdune

type 'set t =
  { sub_dirs : 'set
  ; data_only : 'set
  }

let default =
  let standard_dirs =
    Predicate_lang.of_pred (function
      | "" -> false
      | s -> s.[0] <> '.' && s.[0] <> '_')
  in
  { sub_dirs = standard_dirs
  ; data_only = Predicate_lang.empty
  }

let make ~sub_dirs ~data_only ~ignored_sub_dirs =
  let sub_dirs =
    Option.value sub_dirs ~default:default.sub_dirs in
  let data_only =
    let data_only = Option.value data_only ~default:default.data_only in
    Predicate_lang.union (data_only :: ignored_sub_dirs)
  in
  { sub_dirs ; data_only }

let add_data_only_dirs t ~dirs =
  { t with data_only =
             Predicate_lang.union
               [t.data_only; (Predicate_lang.of_string_set dirs)] }

let eval t ~dirs =
  let sub_dirs =
    Predicate_lang.filter t.sub_dirs ~standard:default.sub_dirs dirs
  in
  let data_only =
    Predicate_lang.filter t.data_only ~standard:default.data_only sub_dirs
    |> String.Set.of_list
  in
  let sub_dirs = String.Set.of_list sub_dirs in
  { sub_dirs
  ; data_only
  }

module Status = struct
  type t = Ignored | Data_only | Normal
end

let status t ~dir =
  match String.Set.mem t.sub_dirs dir
      , String.Set.mem t.data_only dir
  with
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
    let%map version = Syntax.get_exn Stanza.syntax
    and (loc, ignored) = located ignored
    in
    if version >= (1, 6) then begin
      Errors.warn loc
        "ignored_subdirs is deprecated in 1.6.Use subdirs to specify \
         visibile directories or data_only_dirs for ignoring only dune \
         files."
    end;
    ignored
  in
  let plang =
    Syntax.since Stanza.syntax (1, 6) >>>
    Predicate_lang.decode
  in
  let decode =
    let%map ignored_sub_dirs =
      multi_field "ignored_subdirs" ignored_sub_dirs
    and data_only = field_o "data_only_dirs" (located plang)
    and sub_dirs = field_o "subdirs" (located plang)
    and rest = leftover_fields
    in
    match data_only, sub_dirs, ignored_sub_dirs with
    | None, Some (loc, _), _::_ ->
      Errors.fail loc
        "Cannot have both sub_dirs and ignored_subdirs \
         stanza in a dune file. "
    | Some (loc, _), None, _::_ ->
      Errors.fail loc
        "Cannot have both data_only_dirs and ignored_subdirs \
         stanza in a dune file. "
    | _ ->
      let sub_dirs = Option.map ~f:snd sub_dirs in
      let data_only = Option.map ~f:snd data_only in
      ( make ~sub_dirs ~data_only ~ignored_sub_dirs
      , rest
      )
  in
  enter (fields decode)

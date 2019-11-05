open! Stdune

module Status = struct
  type t =
    | Data_only
    | Normal
    | Vendored

  module Map = struct
    type 'a t =
      { data_only : 'a
      ; vendored : 'a
      ; normal : 'a
      }

    let find { data_only; vendored; normal } = function
      | Data_only -> data_only
      | Vendored -> vendored
      | Normal -> normal

    let to_dyn f { data_only; vendored; normal } =
      let open Dyn.Encoder in
      record
        [ ("data_only", f data_only)
        ; ("vendored", f vendored)
        ; ("normal", f normal)
        ]
  end

  let to_dyn t =
    let open Dyn in
    match t with
    | Data_only -> Variant ("Data_only", [])
    | Vendored -> Variant ("Vendored", [])
    | Normal -> Variant ("Normal", [])

  module Or_ignored = struct
    type nonrec t =
      | Ignored
      | Status of t
  end

  module Set = struct
    open Map

    type t = bool Map.t

    let all = { data_only = true; vendored = true; normal = true }

    let normal_only = { data_only = false; vendored = false; normal = true }
  end
end

let status status_by_dir ~dir : Status.Or_ignored.t =
  match String.Map.find status_by_dir dir with
  | None -> Ignored
  | Some d -> Status d

let default =
  let standard_dirs =
    Predicate_lang.Glob.of_pred (function
      | "" -> false
      | s -> s.[0] <> '.' && s.[0] <> '_')
  in
  { Status.Map.normal = standard_dirs
  ; data_only = Predicate_lang.empty
  ; vendored = Predicate_lang.empty
  }

let make ~dirs ~data_only ~ignored_sub_dirs ~vendored_dirs =
  let normal = Option.value dirs ~default:default.normal in
  let data_only =
    let data_only = Option.value data_only ~default:default.data_only in
    Predicate_lang.union (data_only :: ignored_sub_dirs)
  in
  let vendored = Option.value vendored_dirs ~default:default.vendored in
  { Status.Map.normal; data_only; vendored }

type status_map = Status.t String.Map.t

let eval (t : _ Status.Map.t) ~dirs =
  let normal =
    Predicate_lang.Glob.filter t.normal ~standard:default.normal dirs
  in
  let eval ~standard pred = Predicate_lang.Glob.filter pred ~standard dirs in
  let data_only = eval ~standard:default.data_only t.data_only in
  let vendored = eval ~standard:default.vendored t.vendored in
  let statuses =
    List.fold_left normal ~init:String.Map.empty ~f:(fun acc dir ->
        String.Map.set acc dir Status.Normal)
  in
  let statuses =
    List.fold_left data_only ~init:statuses ~f:(fun acc dir ->
        String.Map.set acc dir Status.Data_only)
  in
  List.fold_left vendored ~init:statuses ~f:(fun acc dir ->
      String.Map.update acc dir ~f:(function
        | None
        | Some Status.Vendored
        | Some Normal ->
          Some Vendored
        | Some Data_only ->
          User_error.raise
            [ Pp.textf
                "Directory %s was marked as vendored and data_only, it can't \
                 be marked as both."
                dir
            ]))

let decode =
  let open Dune_lang.Decoder in
  let ignored_sub_dirs =
    let open Dune_lang.Decoder in
    let ignored =
      let+ l =
        enter
          (repeat
             (plain_string (fun ~loc dn ->
                  if
                    Filename.dirname dn <> Filename.current_dir_name
                    ||
                    match dn with
                    | ""
                    | "."
                    | ".." ->
                      true
                    | _ -> false
                  then
                    User_error.raise ~loc
                      [ Pp.textf "Invalid sub-directory name %S" dn ]
                  else
                    dn)))
      in
      Predicate_lang.Glob.of_string_set (String.Set.of_list l)
    in
    let+ version = Dune_lang.Syntax.get_exn Stanza.syntax
    and+ loc, ignored = located ignored in
    if version >= (1, 6) then
      User_warning.emit ~loc
        [ Pp.text
            "ignored_subdirs is deprecated in 1.6. Use dirs to specify visible \
             directories or data_only_dirs for ignoring only dune files."
        ];
    ignored
  in
  let plang =
    Dune_lang.Syntax.since Stanza.syntax (1, 6) >>> Predicate_lang.Glob.decode
  in
  let vendored_dirs =
    let decode = Predicate_lang.Glob.decode in
    located (Dune_lang.Syntax.since Stanza.syntax (1, 11) >>> decode)
  in
  let decode =
    let+ dirs = field_o "dirs" (located plang)
    and+ data_only = field_o "data_only_dirs" (located plang)
    and+ ignored_sub_dirs = multi_field "ignored_subdirs" ignored_sub_dirs
    and+ vendored_dirs = field_o "vendored_dirs" vendored_dirs
    and+ rest = leftover_fields in
    match (data_only, dirs, ignored_sub_dirs) with
    | None, Some (loc, _), _ :: _ ->
      User_error.raise ~loc
        [ Pp.text
            "Cannot have both dirs and ignored_subdirs stanza in a dune file. "
        ]
    | Some (loc, _), None, _ :: _ ->
      User_error.raise ~loc
        [ Pp.text
            "Cannot have both data_only_dirs and ignored_subdirs stanza in a \
             dune file. "
        ]
    | _ ->
      let dirs = Option.map ~f:snd dirs in
      let data_only = Option.map ~f:snd data_only in
      let vendored_dirs = Option.map ~f:snd vendored_dirs in
      (make ~dirs ~data_only ~ignored_sub_dirs ~vendored_dirs, rest)
  in
  enter (fields decode)

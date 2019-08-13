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
      | Data_only ->
        data_only
      | Vendored ->
        vendored
      | Normal ->
        normal
  end

  let to_dyn t =
    let open Dyn in
    match t with
    | Data_only ->
      Variant ("Data_only", [])
    | Vendored ->
      Variant ("Vendored", [])
    | Normal ->
      Variant ("Normal", [])

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

let status { Status.Map.normal; data_only; vendored } ~dir :
    Status.Or_ignored.t =
  match
    ( String.Set.mem normal dir
    , String.Set.mem data_only dir
    , String.Set.mem vendored dir )
  with
  | true, false, false ->
    Status Normal
  | true, false, true ->
    Status Vendored
  | true, true, _ ->
    Status Data_only
  | false, false, _ ->
    Ignored
  | false, true, _ ->
    assert false

let default =
  let standard_dirs =
    Predicate_lang.of_pred (function
      | "" ->
        false
      | s ->
        s.[0] <> '.' && s.[0] <> '_')
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

let add_data_only_dirs (t : _ Status.Map.t) ~dirs =
  { t with
    Status.Map.data_only =
      Predicate_lang.union [ t.data_only; Predicate_lang.of_string_set dirs ]
  }

let eval (t : _ Status.Map.t) ~dirs =
  let normal = Predicate_lang.filter t.normal ~standard:default.normal dirs in
  let to_set ~standard pred =
    String.Set.of_list (Predicate_lang.filter pred ~standard dirs)
  in
  let data_only = to_set ~standard:default.data_only t.data_only in
  let vendored = to_set ~standard:default.vendored t.vendored in
  let both_vendored_and_data = String.Set.inter data_only vendored in
  match String.Set.choose both_vendored_and_data with
  | None ->
    let normal = String.Set.of_list normal in
    { Status.Map.normal; data_only; vendored }
  | Some dir ->
    User_error.raise
      [ Pp.textf
          "Directory %s was marked as vendored and data_only, it can't be \
           marked as both."
          dir
      ]

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
                    || match dn with "" | "." | ".." -> true | _ -> false
                  then
                    User_error.raise ~loc
                      [ Pp.textf "Invalid sub-directory name %S" dn ]
                  else
                    dn)))
      in
      Predicate_lang.of_string_set (String.Set.of_list l)
    in
    let+ version = Syntax.get_exn Stanza.syntax
    and+ loc, ignored = located ignored in
    if version >= (1, 6) then
      User_warning.emit ~loc
        [ Pp.text
            "ignored_subdirs is deprecated in 1.6. Use dirs to specify \
             visible directories or data_only_dirs for ignoring only dune \
             files."
        ];
    ignored
  in
  let plang = Syntax.since Stanza.syntax (1, 6) >>> Predicate_lang.decode in
  let vendored_dirs =
    let decode =
      if Bootstrap.bootstrapping then
        let pred = Predicate_lang.of_pred (fun _ -> true) in
        Dune_lang.Decoder.(map ~f:(fun () -> pred) (keyword "*"))
      else
        Predicate_lang.decode
    in
    located (Syntax.since Stanza.syntax (1, 11) >>> decode)
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

open! Stdune

module Status = struct
  type t =
    | Data_only
    | Normal
    | Vendored
    | Generated

  module Map = struct
    type 'a t =
      { data_only : 'a
      ; vendored : 'a
      ; normal : 'a
      ; generated : 'a
      }

    let merge x y ~f =
      { data_only = f x.data_only y.data_only
      ; vendored = f x.vendored y.vendored
      ; normal = f x.normal y.normal
      ; generated = f x.generated y.generated
      }

    let find { data_only; vendored; normal; generated } = function
      | Data_only -> data_only
      | Vendored -> vendored
      | Normal -> normal
      | Generated -> generated

    let to_dyn f { data_only; vendored; normal; generated } =
      let open Dyn in
      record
        [ ("data_only", f data_only)
        ; ("vendored", f vendored)
        ; ("normal", f normal)
        ; ("generated", f generated)
        ]

    let init ~f =
      { data_only = f Data_only
      ; vendored = f Vendored
      ; normal = f Normal
      ; generated = f Generated
      }
  end

  let to_dyn t =
    let open Dyn in
    match t with
    | Data_only -> Variant ("Data_only", [])
    | Vendored -> Variant ("Vendored", [])
    | Normal -> Variant ("Normal", [])
    | Generated -> Variant ("Generated", [])

  let to_string = function
    | Data_only -> "data_only"
    | Vendored -> "vendored"
    | Normal -> "normal"
    | Generated -> "generated"

  module Or_ignored = struct
    type nonrec t =
      | Ignored
      | Status of t
  end

  module Set = struct
    open Map

    type t = bool Map.t

    let all =
      { data_only = true; vendored = true; normal = true; generated = true }

    let normal_only =
      { data_only = false; vendored = false; normal = true; generated = false }

    let to_list { data_only; vendored; normal; generated } =
      let acc = [] in
      let acc =
        if generated then
          Generated :: acc
        else
          acc
      in
      let acc =
        if vendored then
          Vendored :: acc
        else
          acc
      in
      let acc =
        if data_only then
          Data_only :: acc
        else
          acc
      in
      let acc =
        if normal then
          Normal :: acc
        else
          acc
      in
      acc
  end
end

type subdir_specifier =
  | No_specifier
  | Predicate of Loc.t * Predicate_lang.Glob.t
  | Directory_targets of String.Set.t

type subdir_specifiers = subdir_specifier Status.Map.t

type compiled_subdir_specifiers = Predicate_lang.Glob.t Status.Map.t

let is_generated (t : subdir_specifiers) ~sub_dir =
  match t.generated with
  | No_specifier -> false
  | Predicate _ -> assert false
  | Directory_targets set -> String.Set.mem set sub_dir

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
  ; generated = Predicate_lang.empty
  }

let or_default (t : _ Status.Map.t) : _ Status.Map.t =
  Status.Map.merge t default ~f:(fun t default ->
      match t with
      | No_specifier -> default
      | Predicate (_, p) -> p
      | Directory_targets dirs -> Predicate_lang.Glob.of_string_set dirs)

let make ~dirs ~data_only ~ignored_sub_dirs ~vendored_dirs ~generated =
  let data_only =
    match (data_only, ignored_sub_dirs) with
    | None, [] -> None
    | Some (loc, data_only), [] -> Some (loc, data_only)
    | None, (loc, _) :: _ ->
      let ignored_sub_dirs = List.map ~f:snd ignored_sub_dirs in
      Some (loc, Predicate_lang.union ignored_sub_dirs)
    | Some _data_only, _ :: _ -> assert false
  in
  let make_pred = function
    | None -> No_specifier
    | Some (loc, p) -> Predicate (loc, p)
  in
  let dirs = make_pred dirs in
  let data_only = make_pred data_only in
  let vendored = make_pred vendored_dirs in
  let generated = Directory_targets generated in
  { Status.Map.normal = dirs; data_only; vendored; generated }

type status_map = Status.t String.Map.t

let eval (t : compiled_subdir_specifiers) ~dirs =
  (* This function defines the unexpected behavior of: (dirs foo)
     (data_only_dirs bar)

     In this setup, bar is actually ignored rather than being data only. Because
     it was excluded from the total set of directories. *)
  String.Set.of_list dirs
  |> String.Set.to_map ~f:(fun _ -> ())
  |> String.Map.filter_mapi ~f:(fun dir () : Status.t option ->
         let statuses =
           Status.Map.merge t default ~f:(fun pred standard ->
               Predicate_lang.Glob.exec pred ~standard dir)
           |> Status.Set.to_list
         in
         match statuses with
         | [] -> None
         | statuses -> (
           (* If a directory has a status other than [Normal], then the [Normal]
              status is irrelevant so we just filter it out. *)
           match
             List.filter statuses ~f:(function
               | Status.Normal -> false
               | _ -> true)
           with
           | [] -> Some Normal
           | [ status ] -> Some status
           | statuses ->
             User_error.raise
               [ Pp.textf
                   "Directory %s was marked as %s, it can't be marked as %s."
                   dir
                   (String.enumerate_and
                      (List.map statuses ~f:Status.to_string))
                   (match List.length statuses with
                   | 2 -> "both"
                   | _ -> "all these")
               ]))

module Dir_map = struct
  type per_dir =
    { sexps : Dune_lang.Ast.t list
    ; subdir_status : subdir_specifiers
    }

  type t =
    { data : per_dir
    ; nodes : t String.Map.t
    }

  let empty_per_dir =
    { sexps = []; subdir_status = Status.Map.init ~f:(fun _ -> No_specifier) }

  let empty = { data = empty_per_dir; nodes = String.Map.empty }

  let root t = t.data

  let descend t (p : string) = String.Map.find t.nodes p

  let sub_dirs t = String.Map.keys t.nodes

  let rec make_at_path path data =
    match path with
    | [] -> data
    | x :: xs ->
      let nodes = String.Map.singleton x (make_at_path xs data) in
      { empty with nodes }

  let singleton data = { empty with data }

  let merge_data d1 d2 =
    { sexps = d1.sexps @ d2.sexps
    ; subdir_status =
        Status.Map.merge d1.subdir_status d2.subdir_status ~f:(fun l r ->
            match (l, r) with
            | No_specifier, No_specifier -> No_specifier
            | No_specifier, x
            | x, No_specifier ->
              x
            | Predicate _, Directory_targets _
            | Directory_targets _, Predicate _ ->
              (* This case is impossible because the syntax dicatates that
                 [generated] can only be [Directory_targets] or [No_specifier]
                 and all other fields can only be [Predicate] or
                 [No_specifier]. *)
              assert false
            | Predicate (loc, _), Predicate (loc2, _) ->
              User_error.raise ~loc
                [ Pp.text "This stanza stanza was already specified at:"
                ; Pp.verbatim (Loc.to_file_colon_line loc2)
                ]
            | Directory_targets d1, Directory_targets d2 ->
              Directory_targets (String.Set.union d1 d2))
    }

  let rec merge t1 t2 : t =
    let data = merge_data t1.data t2.data in
    let nodes =
      String.Map.union t1.nodes t2.nodes ~f:(fun _ l r -> Some (merge l r))
    in
    { data; nodes }

  let merge_all = List.fold_left ~f:merge ~init:empty
end

let descendant_path =
  Dune_lang.Decoder.plain_string (fun ~loc fn ->
      if Filename.is_relative fn then
        Path.Local.parse_string_exn ~loc fn |> Path.Local.explode
      else
        let msg = [ Pp.textf "invalid sub-directory path %S" fn ] in
        let hints = [ Pp.textf "sub-directory path must be relative" ] in
        User_error.raise ~loc ~hints msg)

let strict_subdir field_name =
  let open Dune_lang.Decoder in
  plain_string (fun ~loc dn ->
      let msg = [ Pp.textf "invalid sub-directory name %S" dn ] in
      if Filename.dirname dn <> Filename.current_dir_name then
        let msg =
          [ Pp.textf "only immediate sub-directories may be specified." ]
        in
        let hints =
          [ Pp.textf "to ignore %s, write \"(%s %s)\" in %s/dune" dn field_name
              (Filename.basename dn) (Filename.dirname dn)
          ]
        in
        User_error.raise ~loc ~hints msg
      else if
        match dn with
        | ""
        | "." ->
          let hints = [ Pp.textf "did you mean (%s *)?" field_name ] in
          User_error.raise ~loc ~hints msg
        | ".." -> true
        | _ -> false
      then
        User_error.raise ~loc msg
      else
        (loc, dn))

let strict_subdir_glob field_name =
  let open Dune_lang.Decoder in
  let+ globs =
    repeat
      (let+ loc, l = strict_subdir field_name in
       Predicate_lang.Glob.of_glob (Glob.of_string_exn loc l))
  in
  Predicate_lang.union globs

(* This code extract directory targets from the "target"/"targets" fields of
   "rule" stanzas. The syntax must be kept in sync with the code for parsing of
   "rule" stanzas in [dune_rules]. *)
let extract_directory_targets (sexps : Dune_lang.Ast.t list) =
  List.concat_map sexps ~f:(function
    | List (_, Atom (_, A "rule") :: sexps) ->
      List.concat_map sexps ~f:(function
        | List (_, Atom (_, A ("target" | "targets")) :: sexps) ->
          List.filter_map sexps ~f:(function
            | List
                ( _
                , [ Atom (_, A "dir"); (Atom (_, A s) | Quoted_string (_, s)) ]
                ) ->
              Some s
            | _ -> None)
        | _ -> [])
    | _ -> [])
  |> String.Set.of_list

let decode =
  let open Dune_lang.Decoder in
  let ignored_sub_dirs =
    let ignored =
      let+ l = enter (repeat (strict_subdir "ignored_sub_dirs")) in
      Predicate_lang.Glob.of_string_set (String.Set.of_list_map ~f:snd l)
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
  let dirs =
    located
      (Dune_lang.Syntax.since Stanza.syntax (1, 6)
      >>> Predicate_lang.Glob.decode)
  in
  let data_only_dirs =
    located
      (Dune_lang.Syntax.since Stanza.syntax (1, 6)
      >>> strict_subdir_glob "data_only")
  in
  let vendored_dirs =
    (* let decode = Predicate_lang.Glob.decode in *)
    located
      (Dune_lang.Syntax.since Stanza.syntax (1, 11)
      >>> strict_subdir_glob "vendored_dirs")
  in
  let rec subdir () =
    let* () = Dune_lang.Syntax.since Stanza.syntax (2, 5) in
    let* subdir = descendant_path in
    let+ node = fields (decode ~allow_ignored_subdirs:false) in
    Dir_map.make_at_path subdir node
  and decode ~allow_ignored_subdirs =
    let+ dirs = field_o "dirs" dirs
    and+ data_only = field_o "data_only_dirs" data_only_dirs
    and+ ignored_sub_dirs =
      let parser =
        if allow_ignored_subdirs then
          ignored_sub_dirs
        else
          let+ loc = loc in
          User_error.raise ~loc
            [ Pp.textf
                "ignored_subdirs is not allowed under subdir. Use dirs instead"
            ]
      in
      multi_field "ignored_subdirs" (located parser)
    and+ vendored_dirs = field_o "vendored_dirs" vendored_dirs
    and+ subdirs = multi_field "subdir" (subdir ())
    and+ rest = leftover_fields in
    let generated = extract_directory_targets rest in
    match (data_only, dirs, ignored_sub_dirs) with
    | _, Some (loc, _), _ :: _ ->
      User_error.raise ~loc
        [ Pp.text
            "Cannot have both dirs and ignored_subdirs stanza in a dune file. "
        ]
    | Some (loc, _), _, _ :: _ ->
      User_error.raise ~loc
        [ Pp.text
            "Cannot have both data_only_dirs and ignored_subdirs stanza in a \
             dune file. "
        ]
    | _ ->
      Dir_map.merge_all
        (let subdir_status =
           make ~dirs ~data_only ~ignored_sub_dirs ~vendored_dirs ~generated
         in
         Dir_map.singleton { Dir_map.sexps = rest; subdir_status } :: subdirs)
  in
  enter (fields (decode ~allow_ignored_subdirs:true))

let decode_includes ~context =
  let open Dune_lang.Decoder in
  let rec subdir ~context ~path ~inside_include =
    let* dune_version = Dune_lang.Syntax.get_exn Stanza.syntax in
    let* loc = loc in
    let* name, path =
      plain_string (fun ~loc s ->
          (Dune_lang.Ast.atom_or_quoted_string loc s, s :: path))
    in
    let+ nodes = fields (decode ~context ~path ~inside_include) in
    let required_version = (2, 7) in
    if inside_include && dune_version < required_version then
      Dune_lang.Syntax.Error.since loc Stanza.syntax required_version
        ~what:"Using a `subdir' stanza within an `include'd file";
    List
      (loc, Atom (Loc.none, Dune_lang.Atom.of_string "subdir") :: name :: nodes)
  and decode ~context ~path ~inside_include =
    let* includes =
      multi_field "include"
        (let* loc = loc
         and+ fn = relative_file in
         let fn =
           List.fold_left
             ~f:(fun fn dir -> Filename.concat dir fn)
             ~init:fn path
         in
         let sexps, context = Include_stanza.load_sexps ~context (loc, fn) in
         let* () = set_input sexps in
         fields (decode ~context ~path ~inside_include:true))
    in
    let+ subdirs = multi_field "subdir" (subdir ~context ~path ~inside_include)
    and+ sexps = leftover_fields in
    List.concat (sexps :: subdirs :: includes)
  in
  enter (fields (decode ~context ~path:[] ~inside_include:false))

let decode ~file =
  let open Dune_lang.Decoder in
  let* sexps = decode_includes ~context:(Include_stanza.in_file file) in
  let* () = set_input [ List (Loc.none, sexps) ] in
  decode

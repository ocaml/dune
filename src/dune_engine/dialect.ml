open Import
module Action = Dune_lang.Action

module File_kind = struct
  type t =
    { kind : Ml_kind.t
    ; extension : string
    ; preprocess : (Loc.t * Action.t) option
    ; format : (Loc.t * Action.t * string list) option
    }

  let encode { kind; extension; preprocess; format } =
    let open Dune_lang.Encoder in
    let kind =
      string
      @@
      match kind with
      | Ml_kind.Impl -> "implementation"
      | Ml_kind.Intf -> "interface"
    in
    list sexp
      (kind
      :: record_fields
           [ field "extension" string extension
           ; field_o "preprocess" Action.encode (Option.map ~f:snd preprocess)
           ; field_o "format" Action.encode
               (Option.map ~f:(fun (_, x, _) -> x) format)
           ])

  let to_dyn { kind; extension; preprocess; format } =
    let open Dyn in
    record
      [ ("kind", Ml_kind.to_dyn kind)
      ; ("extension", string extension)
      ; ("preprocess", option (fun (_, x) -> Action.to_dyn x) preprocess)
      ; ( "format"
        , option
            (fun (_, x, y) -> pair Action.to_dyn (list string) (x, y))
            format )
      ]
end

type t =
  { name : string
  ; file_kinds : File_kind.t Ml_kind.Dict.t
  }

let name t = t.name

let to_dyn { name; file_kinds } =
  let open Dyn in
  record
    [ ("name", string name)
    ; ("file_kinds", Ml_kind.Dict.to_dyn File_kind.to_dyn file_kinds)
    ]

let encode { name; file_kinds } =
  let open Dune_lang.Encoder in
  let file_kind_stanzas =
    let open Ml_kind in
    List.map ~f:File_kind.encode
      [ Dict.get file_kinds Intf; Dict.get file_kinds Impl ]
  in
  let fields = record_fields [ field "name" string name ] @ file_kind_stanzas in
  list sexp (string "dialect" :: fields)

let decode =
  let open Dune_lang.Decoder in
  let kind kind =
    let+ loc, extension = field "extension" (located string)
    and+ preprocess = field_o "preprocess" (located Action.decode)
    and+ format =
      field_o "format"
        (map ~f:(fun (loc, x) -> (loc, x, [])) (located Action.decode))
    in
    let extension =
      if String.contains extension '.' then
        User_error.raise ~loc [ Pp.textf "extension must not contain '.'" ];
      "." ^ extension
    in
    { File_kind.kind; extension; preprocess; format }
  in
  fields
    (let+ name = field "name" string
     and+ impl = field "implementation" (fields (kind Ml_kind.Impl))
     and+ intf = field "interface" (fields (kind Ml_kind.Intf)) in
     { name; file_kinds = Ml_kind.Dict.make ~intf ~impl })

let extension { file_kinds; _ } ml_kind =
  (Ml_kind.Dict.get file_kinds ml_kind).extension

let preprocess { file_kinds; _ } ml_kind =
  (Ml_kind.Dict.get file_kinds ml_kind).preprocess

let format { file_kinds; _ } ml_kind =
  (Ml_kind.Dict.get file_kinds ml_kind).format

let ocaml =
  let format kind =
    let flag_of_kind = function
      | Ml_kind.Impl -> "--impl"
      | Intf -> "--intf"
    in
    let module S = String_with_vars in
    Action.chdir
      (S.make_pform Loc.none (Var Workspace_root))
      (Action.run
         (S.make_text Loc.none "ocamlformat")
         [ S.make_text Loc.none (flag_of_kind kind)
         ; S.make_pform Loc.none (Var Input_file)
         ])
  in
  let file_kind kind extension =
    { File_kind.kind
    ; extension
    ; preprocess = None
    ; format =
        Some
          ( Loc.none
          , format kind
          , [ ".ocamlformat"; ".ocamlformat-ignore"; ".ocamlformat-enable" ] )
    }
  in
  let intf = file_kind Ml_kind.Intf ".mli" in
  let impl = file_kind Ml_kind.Impl ".ml" in
  { name = "ocaml"; file_kinds = Ml_kind.Dict.make ~intf ~impl }

let reason =
  let file_kind kind extension =
    let module S = String_with_vars in
    let preprocess =
      Action.run
        (S.make_text Loc.none "refmt")
        [ S.make_text Loc.none "--print"
        ; S.make_text Loc.none "binary"
        ; S.make_pform Loc.none (Var Input_file)
        ]
    in
    let format =
      Action.run
        (S.make_text Loc.none "refmt")
        [ S.make_pform Loc.none (Var Input_file) ]
    in
    { File_kind.kind
    ; extension
    ; preprocess = Some (Loc.none, preprocess)
    ; format = Some (Loc.none, format, [])
    }
  in
  let intf = file_kind Ml_kind.Intf ".rei" in
  let impl = file_kind Ml_kind.Impl ".re" in
  { name = "reason"; file_kinds = Ml_kind.Dict.make ~intf ~impl }

let ml_suffix { file_kinds = { Ml_kind.Dict.intf; impl }; _ } ml_kind =
  match (ml_kind, intf.preprocess, impl.preprocess) with
  | Ml_kind.Intf, None, _ | Impl, _, None -> None
  | _ -> Some (extension ocaml ml_kind)

module DB = struct
  type dialect = t

  type t =
    { by_name : dialect String.Map.t
    ; by_extension : dialect String.Map.t
    ; mutable extensions_for_merlin : string Ml_kind.Dict.t list option
    }

  let fold { by_name; _ } = String.Map.fold by_name

  let empty =
    { by_name = String.Map.empty
    ; by_extension = String.Map.empty
    ; extensions_for_merlin = None
    }

  let set_extensions_for_merlin t =
    let v =
      fold t ~init:[] ~f:(fun d s ->
          let impl = extension d Ml_kind.Impl in
          let intf = extension d Ml_kind.Intf in
          if
            (* Only include dialects with no preprocessing and skip default file
               extensions *)
            preprocess d Ml_kind.Impl <> None
            || preprocess d Ml_kind.Intf <> None
            || impl = extension ocaml Ml_kind.Impl
               && intf = extension ocaml Ml_kind.Intf
          then s
          else { Ml_kind.Dict.impl; intf } :: s)
      |> List.sort ~compare:(Ml_kind.Dict.compare String.compare)
    in
    t.extensions_for_merlin <- Some v;
    v

  let extensions_for_merlin t =
    match t.extensions_for_merlin with
    | Some s -> s
    | None -> set_extensions_for_merlin t

  let add { by_name; by_extension; extensions_for_merlin = _ } ~loc dialect =
    let by_name =
      match String.Map.add by_name dialect.name dialect with
      | Ok by_name -> by_name
      | Error _ ->
        User_error.raise ~loc
          [ Pp.textf "dialect %S is already defined" dialect.name ]
    in
    let add_ext map ext =
      match String.Map.add map ext dialect with
      | Ok map -> map
      | Error dialect ->
        User_error.raise ~loc
          [ Pp.textf "extension %S is already registered by dialect %S"
              (String.drop ext 1) dialect.name
          ]
    in
    let by_extension =
      add_ext
        (add_ext by_extension dialect.file_kinds.intf.extension)
        dialect.file_kinds.impl.extension
    in
    { by_name; by_extension; extensions_for_merlin = None }

  let of_list dialects =
    List.fold_left ~f:(add ~loc:Loc.none) ~init:empty dialects

  let find_by_name { by_name; _ } name = String.Map.find by_name name

  let find_by_extension { by_extension; _ } extension =
    Option.map
      ~f:(fun dialect ->
        let kind =
          if dialect.file_kinds.intf.extension = extension then Ml_kind.Intf
          else Ml_kind.Impl
        in
        (dialect, kind))
      (String.Map.find by_extension extension)

  let to_dyn { by_name; _ } = String.Map.to_dyn to_dyn by_name

  let builtin = of_list [ ocaml; reason ]
end

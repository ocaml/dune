open! Stdune

module File_kind = struct
  type t =
    { kind : Ml_kind.t
    ; extension : string
    ; preprocess : (Loc.t * Action_dune_lang.t) option
    ; format : (Loc.t * Action_dune_lang.t * string list) option
    }

  let to_dyn { kind; extension; preprocess; format } =
    let open Dyn.Encoder in
    record
      [ ("kind", Ml_kind.to_dyn kind)
      ; ("extension", string extension)
      ; ( "preprocess"
        , option (fun (_, x) -> Action_dune_lang.to_dyn x) preprocess )
      ; ( "format"
        , option
            (fun (_, x, y) -> pair Action_dune_lang.to_dyn (list string) (x, y))
            format )
      ]
end

type t =
  { name : string
  ; file_kinds : File_kind.t Ml_kind.Dict.t
  }

let name t = t.name

let to_dyn { name; file_kinds } =
  let open Dyn.Encoder in
  record
    [ ("name", string name)
    ; ("file_kinds", Ml_kind.Dict.to_dyn File_kind.to_dyn file_kinds)
    ]

let decode =
  let open Dune_lang.Decoder in
  let kind kind =
    let+ loc, extension = field "extension" (located string)
    and+ preprocess = field_o "preprocess" (located Action_dune_lang.decode)
    and+ format =
      field_o "format"
        (map
           ~f:(fun (loc, x) -> (loc, x, []))
           (located Action_dune_lang.decode))
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

let extension { file_kinds = { Ml_kind.Dict.intf; impl }; _ } = function
  | Ml_kind.Intf -> intf.extension
  | Impl -> impl.extension

let preprocess { file_kinds = { Ml_kind.Dict.intf; impl }; _ } = function
  | Ml_kind.Intf -> intf.preprocess
  | Impl -> impl.preprocess

let format { file_kinds = { Ml_kind.Dict.intf; impl }; _ } = function
  | Ml_kind.Intf -> intf.format
  | Impl -> impl.format

let ocaml =
  let format kind =
    let flag_of_kind = function
      | Ml_kind.Impl -> "--impl"
      | Intf -> "--intf"
    in
    let module S = String_with_vars in
    Action_dune_lang.chdir
      (S.make_var Loc.none "workspace_root")
      (Action_dune_lang.run
         (S.make_text Loc.none "ocamlformat")
         [ S.make_text Loc.none (flag_of_kind kind)
         ; S.make_var Loc.none "input-file"
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
      Action_dune_lang.run
        (S.make_text Loc.none "refmt")
        [ S.make_text Loc.none "--print"
        ; S.make_text Loc.none "binary"
        ; S.make_var Loc.none "input-file"
        ]
    in
    let format =
      Action_dune_lang.run
        (S.make_text Loc.none "refmt")
        [ S.make_var Loc.none "input-file" ]
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
  | Ml_kind.Intf, None, _
  | Impl, _, None ->
    None
  | _ -> Some (extension ocaml ml_kind)

module DB = struct
  type dialect = t

  type t =
    { by_name : dialect String.Map.t
    ; by_extension : dialect String.Map.t
    }

  let empty = { by_name = String.Map.empty; by_extension = String.Map.empty }

  let add { by_name; by_extension } ~loc dialect =
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
    { by_name; by_extension }

  let of_list dialects =
    List.fold_left ~f:(add ~loc:Loc.none) ~init:empty dialects

  let find_by_name { by_name; _ } name = String.Map.find by_name name

  let find_by_extension { by_extension; _ } extension =
    Option.map
      ~f:(fun dialect ->
        let kind =
          if dialect.file_kinds.intf.extension = extension then
            Ml_kind.Intf
          else
            Ml_kind.Impl
        in
        (dialect, kind))
      (String.Map.find by_extension extension)

  let to_dyn { by_name; _ } = String.Map.to_dyn to_dyn by_name

  let builtin = of_list [ ocaml; reason ]
end

open Import
module Action = Dune_lang.Action
module Ml_kind = Ocaml.Ml_kind
module Cm_kind = Ocaml.Cm_kind
module Mode = Ocaml.Mode

module File_kind = struct
  type t =
    { kind : Ml_kind.t
    ; extension : Filename.Extension.t
    ; preprocess : (Loc.t * Action.t) option
    ; format : (Loc.t * Action.t * string list) option
    ; print_ast : (Loc.t * Action.t) option
    ; merlin_reader : (Loc.t * string list) option
    }

  let encode { kind; extension; preprocess; format; print_ast; merlin_reader } =
    let open Dune_lang.Encoder in
    let kind =
      string
      @@
      match kind with
      | Ml_kind.Impl -> "implementation"
      | Ml_kind.Intf -> "interface"
    in
    list
      sexp
      (kind
       :: record_fields
            [ field "extension" string extension
            ; field_o "preprocess" Action.encode (Option.map ~f:snd preprocess)
            ; field_o "format" Action.encode (Option.map ~f:(fun (_, x, _) -> x) format)
            ; field_o
                "print_ast"
                Action.encode
                (Option.map ~f:(fun (_, x) -> x) print_ast)
            ; field_o "merlin_reader" (list string) (Option.map ~f:snd merlin_reader)
            ])
  ;;

  let to_dyn { kind; extension; preprocess; format; print_ast; merlin_reader } =
    let open Dyn in
    record
      [ "kind", Ml_kind.to_dyn kind
      ; "extension", string extension
      ; "preprocess", option (fun (_, x) -> Action.to_dyn x) preprocess
      ; "format", option (fun (_, x, y) -> pair Action.to_dyn (list string) (x, y)) format
      ; "print_ast", option (fun (_, x) -> Action.to_dyn x) print_ast
      ; "merlin_reader", option (fun (_, x) -> list string x) merlin_reader
      ]
  ;;
end

type t =
  { name : string
  ; file_kinds : File_kind.t option Ml_kind.Dict.t
  }

let name t = t.name

let to_dyn { name; file_kinds } =
  let open Dyn in
  record
    [ "name", string name
    ; "file_kinds", Ml_kind.Dict.to_dyn (Dyn.option File_kind.to_dyn) file_kinds
    ]
;;

let encode { name; file_kinds } =
  let open Dune_lang.Encoder in
  let open Option.O in
  let file_kind_stanzas =
    List.filter_map Ml_kind.all ~f:(fun kind ->
      let+ file_kind = Ml_kind.Dict.get file_kinds kind in
      File_kind.encode file_kind)
  in
  let fields = record_fields [ field "name" string name ] @ file_kind_stanzas in
  list sexp (string "dialect" :: fields)
;;

let decode =
  let open Dune_lang.Decoder in
  let kind kind =
    let+ loc, extension = field "extension" (located extension)
    and+ preprocess = field_o "preprocess" (located Action.decode_dune_file)
    and+ format =
      field_o
        "format"
        (map ~f:(fun (loc, x) -> loc, x, []) (located Action.decode_dune_file))
    and+ print_ast = field_o "print_ast" (located Action.decode_dune_file)
    and+ merlin_reader =
      field_o
        "merlin_reader"
        (Dune_lang.Syntax.since Stanza.syntax (3, 16) >>> located (repeat1 string))
    and+ syntax_ver = Syntax.get_exn Stanza.syntax in
    let ver = 3, 9 in
    if syntax_ver < ver && Option.is_some (String.index_from extension 1 '.')
    then (
      let what = "the possibility of defining extensions containing periods" in
      Syntax.Error.since loc Stanza.syntax ver ~what);
    { File_kind.kind; extension; preprocess; format; print_ast; merlin_reader }
  in
  fields
    (let+ name = field "name" string
     and+ loc = loc
     and+ impl = field_o "implementation" (fields (kind Ml_kind.Impl))
     and+ intf = field_o "interface" (fields (kind Ml_kind.Intf))
     and+ version = Dune_lang.Syntax.get_exn Stanza.syntax in
     let dialect_kind_optional_since = 3, 9 in
     if version < dialect_kind_optional_since
     then (
       if Option.is_none impl
       then
         Syntax.Error.since
           loc
           Stanza.syntax
           dialect_kind_optional_since
           ~what:"omitting (implementation) in dialects";
       if Option.is_none intf
       then
         Syntax.Error.since
           loc
           Stanza.syntax
           dialect_kind_optional_since
           ~what:"omitting (interface) in dialects");
     { name; file_kinds = Ml_kind.Dict.make ~intf ~impl })
;;

let extension { file_kinds; _ } ml_kind =
  let open Option.O in
  let+ x = Ml_kind.Dict.get file_kinds ml_kind in
  x.extension
;;

let preprocess { file_kinds; _ } ml_kind =
  let open Option.O in
  let* x = Ml_kind.Dict.get file_kinds ml_kind in
  x.preprocess
;;

let format { file_kinds; _ } ml_kind =
  let open Option.O in
  let* x = Ml_kind.Dict.get file_kinds ml_kind in
  x.format
;;

let print_ast { file_kinds; _ } ml_kind =
  let open Option.O in
  let* x = Ml_kind.Dict.get file_kinds ml_kind in
  x.print_ast
;;

let merlin_reader { file_kinds; _ } ml_kind =
  let open Option.O in
  let* dialect = Ml_kind.Dict.get file_kinds ml_kind in
  let+ _, merlin_reader = dialect.merlin_reader in
  merlin_reader
;;

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
  let print_ast _kind =
    let module S = String_with_vars in
    Action.chdir
      (S.make_pform Loc.none (Var Workspace_root))
      (Action.run
         (S.make_text Loc.none "ocamlc")
         [ S.make_text Loc.none "-stop-after"
         ; S.make_text Loc.none "parsing"
         ; S.make_text Loc.none "-dsource"
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
    ; print_ast = Some (Loc.none, print_ast kind)
    ; merlin_reader = None
    }
  in
  let intf = Some (file_kind Ml_kind.Intf ".mli") in
  let impl = Some (file_kind Ml_kind.Impl ".ml") in
  { name = "ocaml"; file_kinds = Ml_kind.Dict.make ~intf ~impl }
;;

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
      Action.run (S.make_text Loc.none "refmt") [ S.make_pform Loc.none (Var Input_file) ]
    in
    let print_ast =
      let flag_of_kind = function
        | Ml_kind.Impl -> "false"
        | Intf -> "true"
      in
      let module S = String_with_vars in
      Action.chdir
        (S.make_pform Loc.none (Var Workspace_root))
        (Action.run
           (S.make_text Loc.none "refmt")
           [ S.make_text Loc.none "--parse=binary"
           ; S.make_text Loc.none "-i"
           ; S.make_text Loc.none (flag_of_kind kind)
           ; S.make_pform Loc.none (Var Input_file)
           ])
    in
    { File_kind.kind
    ; extension
    ; preprocess = Some (Loc.none, preprocess)
    ; format = Some (Loc.none, format, [])
    ; print_ast = Some (Loc.none, print_ast)
    ; merlin_reader = None
    }
  in
  let intf = Some (file_kind Ml_kind.Intf ".rei") in
  let impl = Some (file_kind Ml_kind.Impl ".re") in
  { name = "reason"; file_kinds = Ml_kind.Dict.make ~intf ~impl }
;;

let rescript =
  let file_kind kind extension =
    let module S = String_with_vars in
    let exe_name = "rescript_syntax" in
    let preprocess =
      Action.run
        (S.make_text Loc.none exe_name)
        [ S.make_text Loc.none "-print"
        ; S.make_text Loc.none "binary"
        ; S.make_pform Loc.none (Var Input_file)
        ]
    in
    let format =
      Action.run
        (S.make_text Loc.none exe_name)
        [ S.make_pform Loc.none (Var Input_file) ]
    in
    { File_kind.kind
    ; extension
    ; preprocess = Some (Loc.none, preprocess)
    ; format = Some (Loc.none, format, [])
    ; print_ast = None
    ; merlin_reader = None
    }
  in
  let intf = Some (file_kind Ml_kind.Intf ".resi") in
  let impl = Some (file_kind Ml_kind.Impl ".res") in
  { name = "rescript"; file_kinds = Ml_kind.Dict.make ~intf ~impl }
;;

let ml_suffix { file_kinds = { intf; impl }; _ } ml_kind =
  match ml_kind, intf, impl with
  | Ml_kind.Intf, (None | Some { preprocess = None; _ }), _
  | Impl, _, (None | Some { preprocess = None; _ }) -> None
  | _ -> extension ocaml ml_kind
;;

module DB = struct
  type dialect = t

  type t =
    { by_name : dialect String.Map.t
    ; by_extension : dialect Filename.Extension.Map.t
    ; for_merlin : for_merlin Lazy.t
    }

  and for_merlin =
    { extensions : Filename.Extension.t option Ml_kind.Dict.t list
    ; readers : Filename.Extension.t list String.Map.t
    }

  let fold { by_name; _ } = String.Map.fold by_name

  let empty =
    { by_name = String.Map.empty
    ; by_extension = Filename.Extension.Map.empty
    ; for_merlin = lazy { extensions = []; readers = String.Map.empty }
    }
  ;;

  let compute_for_merlin =
    let handle_ml_kind ~dialect kind readers =
      let ext = extension dialect kind in
      if ext = extension ocaml kind
      then (* this is standard dialect, exclude *) None, readers
      else (
        match ext, merlin_reader dialect kind with
        | Some ext, Some reader -> Some ext, String.Map.add_exn readers ext reader
        | _ ->
          if preprocess dialect kind <> None
          then (* we have preprocessor defined *) None, readers
          else ext, readers)
    in
    fun by_name ->
      let extensions, readers =
        String.Map.fold
          by_name
          ~init:([], String.Map.empty)
          ~f:(fun dialect (extensions, readers) ->
            let impl, readers = handle_ml_kind ~dialect Ml_kind.Impl readers in
            let intf, readers = handle_ml_kind ~dialect Ml_kind.Intf readers in
            let extensions =
              match impl, intf with
              | None, None -> extensions
              | _ -> { Ml_kind.Dict.impl; intf } :: extensions
            in
            extensions, readers)
      in
      let extensions =
        List.sort
          ~compare:(Ml_kind.Dict.compare (Option.compare String.compare))
          extensions
      in
      { extensions; readers }
  ;;

  let for_merlin t = Lazy.force t.for_merlin

  let add { by_name; by_extension; for_merlin = _ } ~loc dialect =
    let by_name =
      match String.Map.add by_name dialect.name dialect with
      | Ok by_name -> by_name
      | Error _ ->
        User_error.raise ~loc [ Pp.textf "dialect %S is already defined" dialect.name ]
    in
    let add_ext map = function
      | Some { File_kind.extension = ext; _ } ->
        (match Filename.Extension.Map.add map ext dialect with
         | Ok map -> map
         | Error dialect ->
           User_error.raise
             ~loc
             [ Pp.textf
                 "extension %S is already registered by dialect %S"
                 (String.drop ext 1)
                 dialect.name
             ])
      | None -> map
    in
    let by_extension =
      add_ext (add_ext by_extension dialect.file_kinds.intf) dialect.file_kinds.impl
    in
    { by_name; by_extension; for_merlin = lazy (compute_for_merlin by_name) }
  ;;

  let of_list dialects = List.fold_left ~f:(add ~loc:Loc.none) ~init:empty dialects
  let find_by_name { by_name; _ } name = String.Map.find by_name name

  let find_by_extension { by_extension; _ } extension =
    Option.map
      ~f:(fun dialect ->
        let kind =
          match dialect.file_kinds.intf with
          | Some intf when intf.extension = extension -> Ml_kind.Intf
          | _ -> Ml_kind.Impl
        in
        dialect, kind)
      (Filename.Extension.Map.find by_extension extension)
  ;;

  let to_dyn { by_name; _ } = String.Map.to_dyn to_dyn by_name
  let builtin = of_list [ ocaml; reason ]
  let is_default t = phys_equal t builtin
end

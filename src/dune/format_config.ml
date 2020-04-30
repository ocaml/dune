open! Stdune
open Import
open Dune_lang.Decoder

let syntax =
  Dune_lang.Syntax.create ~name:"fmt"
    ~desc:"integration with automatic formatters"
    [ ((1, 0), `Since (1, 4))
    ; ((1, 1), `Since (1, 7))
    ; ((1, 2), `Since (1, 11))
    ]

module Language = struct
  type t =
    | Dialect of string
    | Dune

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Dialect name -> constr "dialect" [ string name ]
    | Dune -> constr "dune" []

  let of_string = function
    | "dune" -> Dune
    | s -> Dialect s

  let in_ext_1_0 = [ Dialect "ocaml"; Dialect "reason" ]

  let in_ext_1_1 = Dune :: in_ext_1_0

  let encode =
    let open Dune_lang.Encoder in
    function
    | Dune -> string "dune"
    | Dialect d -> string d
end

module Enabled_for = struct
  type t =
    | Only of Language.t list
    | All

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Only l -> constr "only" (List.map ~f:Language.to_dyn l)
    | All -> string "all"

  let includes t =
    match t with
    | Only l -> List.mem ~set:l
    | All -> fun _ -> true

  let field = field_o "enabled_for" (repeat (map ~f:Language.of_string string))

  let field_ext =
    let+ list_opt = field
    and+ ext_version = Dune_lang.Syntax.get_exn syntax in
    match (list_opt, ext_version) with
    | Some l, _ -> Only l
    | None, (1, 0) -> Only Language.in_ext_1_0
    | None, (1, 1) -> Only Language.in_ext_1_1
    | None, (1, 2) -> All
    | None, _ ->
      Code_error.raise "This fmt version does not exist"
        [ ("version", Dune_lang.Syntax.Version.to_dyn ext_version) ]
end

type 'enabled_for generic_t =
  { loc : Loc.t
  ; enabled_for : 'enabled_for
  }

type t = Enabled_for.t generic_t

let includes t lang = Enabled_for.includes t.enabled_for lang

let to_dyn { enabled_for; loc = _ } =
  let open Dyn.Encoder in
  record [ ("enabled_for", Enabled_for.to_dyn enabled_for) ]

let dparse_args =
  let+ loc = loc
  and+ enabled_for = fields Enabled_for.field_ext in
  ({ loc; enabled_for }, [])

let dune2_record_syntax =
  let+ loc = loc
  and+ ef = Enabled_for.field in
  let enabled_for =
    match ef with
    | Some l -> Enabled_for.Only l
    | None -> All
  in
  Some { loc; enabled_for }

let dune2_dec =
  keyword "disabled" >>> return None <|> fields dune2_record_syntax

let dune2_default = Some { loc = Loc.none; enabled_for = Enabled_for.All }

let field_dune2 = field "formatting" dune2_dec ~default:dune2_default

let field =
  let* dune_lang_version = Dune_lang.Syntax.get_exn Stanza.syntax in
  match Dune_lang.Syntax.Version.compare dune_lang_version (2, 0) with
  | Lt -> return None
  | Gt
  | Eq ->
    field_dune2

let loc t = t.loc

let encode_formatting { loc = _; enabled_for } =
  let open Dune_lang.Encoder in
  record_fields
    [ field_i "enabled_for" (List.map ~f:Language.encode) enabled_for ]

let encode_explicit conf =
  let open Dune_lang.Encoder in
  [ field_i "formatting" encode_formatting conf ] |> record_fields |> List.hd

let to_explicit { loc; enabled_for } =
  match enabled_for with
  | Enabled_for.All -> None
  | Only l -> Some { loc; enabled_for = l }

let of_config ~ext ~dune_lang =
  match (ext, dune_lang) with
  | None, None -> None
  | Some x, None -> Some x
  | None, Some x -> Some x
  | Some ext, Some _ ->
    let suggestion =
      match to_explicit ext with
      | Some explicit ->
        let dlang = encode_explicit explicit in
        [ Pp.textf "To port it to the new syntax, you can replace this part by:"
        ; Pp.tag User_message.Style.Details (Dune_lang.pp dlang)
        ]
      | None ->
        [ Pp.textf "To port it to the new syntax, you can delete this part." ]
    in
    User_error.raise ~loc:ext.loc
      ( Pp.textf
          "Starting with (lang dune 2.0), formatting is enabled by default."
      :: suggestion )

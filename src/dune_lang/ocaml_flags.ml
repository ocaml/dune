open Import

type 'a t =
  { common : 'a
  ; specific : 'a Lib_mode.Map.t
  }

let equal f { common; specific } t =
  f common t.common && Lib_mode.Map.equal f specific t.specific
;;

module Spec = struct
  type melange =
    | Flags
    | Compile_flags

  type nonrec t =
    { flags : Ordered_set_lang.Unexpanded.t t
    ; melange : melange option
    }

  let equal { flags; melange } t =
    let equal = equal Ordered_set_lang.Unexpanded.equal in
    equal flags t.flags && Poly.equal melange t.melange
  ;;

  let standard =
    { flags =
        { common = Ordered_set_lang.Unexpanded.standard
        ; specific = Lib_mode.Map.make_all Ordered_set_lang.Unexpanded.standard
        }
    ; melange = None
    }
  ;;

  let make ~common ~specific : t = { flags = { common; specific }; melange = None }

  let decode =
    let open Decoder in
    let field_oslu = Ordered_set_lang.Unexpanded.field in
    let+ common = field_oslu "flags"
    and+ byte = field_oslu "ocamlc_flags"
    and+ native = field_oslu "ocamlopt_flags"
    and+ melange =
      fields_mutually_exclusive
        ~default:None
        [ ( "melange.flags"
          , Syntax.since Stanza.syntax (3, 26)
            >>> Syntax.since Melange.syntax (0, 1)
            >>> let+ flags = Ordered_set_lang.Unexpanded.decode in
                Some (Flags, flags) )
        ; ( "melange.compile_flags"
          , Syntax.since Melange.syntax (0, 1)
            >>> Syntax.deprecated_in
                  Stanza.syntax
                  (3, 26)
                  ~extra_info:"Use 'melange.flags' instead."
            >>> let+ flags = Ordered_set_lang.Unexpanded.decode in
                Some (Compile_flags, flags) )
        ]
    in
    let melange, melange_flags =
      match melange with
      | None -> None, Ordered_set_lang.Unexpanded.standard
      | Some (melange, flags) -> Some melange, flags
    in
    let specific = Lib_mode.Map.make ~byte ~native ~melange:melange_flags in
    { flags = { common; specific }; melange }
  ;;
end

let open_flags modules =
  List.concat_map modules ~f:(fun name -> [ "-open"; Module_name.to_string name ])
;;

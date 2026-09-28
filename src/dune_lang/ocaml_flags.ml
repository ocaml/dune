open Import

type 'a t =
  { common : 'a
  ; specific : 'a Lib_mode.Map.t
  }

let equal f { common; specific } t =
  f common t.common && Lib_mode.Map.equal f specific t.specific
;;

module Spec = struct
  type nonrec t = Ordered_set_lang.Unexpanded.t t

  let equal = equal Ordered_set_lang.Unexpanded.equal

  let standard =
    { common = Ordered_set_lang.Unexpanded.standard
    ; specific = Lib_mode.Map.make_all Ordered_set_lang.Unexpanded.standard
    }
  ;;

  let make ~common ~specific : t = { common; specific }

  let decode =
    let open Decoder in
    let field_oslu = Ordered_set_lang.Unexpanded.field in
    let+ common = field_oslu "flags"
    and+ byte = field_oslu "ocamlc_flags"
    and+ native = field_oslu "ocamlopt_flags"
    and+ melange =
      fields_mutually_exclusive
        ~default:Ordered_set_lang.Unexpanded.standard
        [ ( "melange.flags"
          , Syntax.since Stanza.syntax (3, 25)
            >>> Syntax.since Melange.syntax (0, 1)
            >>> Ordered_set_lang.Unexpanded.decode )
        ; ( "melange.compile_flags"
          , Syntax.since Melange.syntax (0, 1)
            >>> Syntax.deprecated_in
                  Stanza.syntax
                  (3, 25)
                  ~extra_info:"Use 'melange.flags' instead."
            >>> Ordered_set_lang.Unexpanded.decode )
        ]
    in
    let specific = Lib_mode.Map.make ~byte ~native ~melange in
    { common; specific }
  ;;
end

let open_flags modules =
  List.concat_map modules ~f:(fun name -> [ "-open"; Module_name.to_string name ])
;;

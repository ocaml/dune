open Import

type 'a t =
  { flags : 'a
  ; explain : Blang.t option
  }

let map ~f t = { t with flags = f t.flags }

let equal { flags; explain } t =
  Ordered_set_lang.Unexpanded.equal flags t.flags
  && Option.equal Blang.equal explain t.explain
;;

let decode =
  let open Decoder in
  fields
  @@ let+ flags = Ordered_set_lang.Unexpanded.field "flags"
     and+ explain = field_o "explain" Blang.decode in
     { flags; explain }
;;

let empty = { flags = Ordered_set_lang.Unexpanded.standard; explain = None }
let default = { flags = []; explain = None }

let dump t =
  let open Dune_engine.Action_builder.O in
  let+ flags = t.flags in
  List.map
    ~f:Encoder.(pair string Fun.id)
    [ "menhir_flags", Encoder.(list string) flags
    ; "menhir_explain", Encoder.option Blang.encode t.explain
    ]
;;

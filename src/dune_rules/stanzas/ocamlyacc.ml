open Import

type t =
  { loc : Loc.t
  ; modules : string list
  ; mode : Rule_mode.t
  ; enabled_if : Blang.t
  }

include Stanza.Make (struct
    type nonrec t = t

    include Poly
  end)

let decode =
  let open Dune_lang.Decoder in
  (let+ loc = loc
   and+ modules = repeat string in
   { loc; modules; mode = Standard; enabled_if = Blang.true_ })
  <|> fields
        (let+ loc = loc
         and+ modules = field "modules" (repeat string)
         and+ mode = Rule_mode_decoder.field
         and+ enabled_if = Enabled_if.decode ~allowed_vars:Any ~since:(Some (1, 4)) () in
         { loc; modules; mode; enabled_if })
;;

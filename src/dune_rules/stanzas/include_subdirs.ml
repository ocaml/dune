open Import

type qualification =
  | Unqualified
  | Qualified

type t =
  | No
  | Include of qualification

type stanza = Loc.t * t

include Stanza.Make (struct
    type nonrec t = stanza

    include Poly
  end)

let decode ~enable_qualified =
  let open Dune_lang.Decoder in
  sum
    [ "no", return No
    ; "unqualified", return (Include Unqualified)
    ; ( "qualified"
      , let+ () =
          if enable_qualified then return () else Syntax.since Stanza.syntax (3, 7)
        in
        Include Qualified )
    ]
;;

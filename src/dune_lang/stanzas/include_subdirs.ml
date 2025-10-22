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

let decode ~qualified =
  let open Decoder in
  let* () = Syntax.since Stanza.syntax (1, 1) in
  let* loc = loc in
  let+ t =
    sum
      [ "no", return No
      ; "unqualified", return (Include Unqualified)
      ; ( "qualified"
        , let+ () = qualified in
          Include Qualified )
      ]
  in
  loc, t
;;

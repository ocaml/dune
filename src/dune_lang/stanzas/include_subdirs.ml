open Import

type qualification =
  | Unqualified
  | Qualified of { dirs : File_binding.Unexpanded.t list }

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
          Include (Qualified { dirs = [] }) )
      ]
  in
  loc, t
;;

let decode ~qualified =
  let open Decoder in
  let legacy = decode ~qualified in
  let decode =
    fields
      (let+ loc, mode = field "mode" (decode ~qualified)
       and+ dirs =
         field_o
           "dirs"
           (let* () = Syntax.since Stanza.syntax (3, 25) in
            let+ loc = loc
            and+ dirs = repeat File_binding.Unexpanded.decode in
            loc, dirs)
       in
       match dirs with
       | None -> loc, mode
       | Some (dirs_loc, dirs) ->
         (match mode with
          | No | Include Unqualified ->
            User_error.raise
              ~loc:dirs_loc
              [ Pp.text "The dirs field is only allowed with (mode qualified)." ]
          | Include (Qualified _) -> loc, Include (Qualified { dirs })))
  in
  legacy <|> decode
;;

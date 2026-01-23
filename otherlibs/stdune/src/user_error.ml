exception E of User_message.t

module Severity = User_message.Severity
module Related = User_message.Related
module Diagnostic = User_message.Diagnostic

let related = User_message.related
let with_related = User_message.with_related
let prefix = Pp.seq (Pp.tag User_message.Style.Error (Pp.verbatim "Error")) (Pp.char ':')

let make ?has_embedded_location ?needs_stack_trace ?loc ?hints ?annots paragraphs =
  User_message.make
    ?has_embedded_location
    ?needs_stack_trace
    ?loc
    ?hints
    ?annots
    paragraphs
    ~prefix
;;

let raise
      ?has_embedded_location
      ?needs_stack_trace
      ?loc
      ?hints
      ?annots
      ?related
      paragraphs
  =
  raise
    (E
       (User_message.make
          ?has_embedded_location
          ?needs_stack_trace
          ?loc
          ?hints
          ?annots
          ?related
          paragraphs
          ~prefix))
;;

let ok_exn = function
  | Ok x -> x
  | Error msg -> Stdlib.raise (E msg)
;;

let reason =
  let reason = Pp.tag User_message.Style.Kwd (Pp.verbatim "Reason") in
  fun msg -> Pp.hovbox (Pp.concat [ reason; Pp.char ':'; Pp.space; msg ])
;;

let () =
  Printexc.register_printer (function
    | E t ->
      let open Pp.O in
      let pp =
        User_message.pp t
        ++
        if User_message.Annots.is_empty t.annots
        then Pp.nop
        else Dyn.pp (User_message.Annots.to_dyn t.annots)
      in
      Some (Format.asprintf "%a" Pp.to_fmt pp)
    | _ -> None)
;;

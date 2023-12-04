exception E of User_message.t

let prefix = Pp.seq (Pp.tag User_message.Style.Error (Pp.verbatim "Error")) (Pp.char ':')

let make ?loc ?hints ?annots paragraphs =
  User_message.make ?loc ?hints ?annots paragraphs ~prefix
;;

let raise ?loc ?hints ?annots paragraphs = raise (E (make ?loc ?hints ?annots paragraphs))

let ok_exn = function
  | Ok x -> x
  | Error msg -> Stdlib.raise (E msg)
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

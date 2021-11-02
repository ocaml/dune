exception E of User_message.t

let prefix =
  Pp.seq (Pp.tag User_message.Style.Error (Pp.verbatim "Error")) (Pp.char ':')

let make ?loc ?hints ?annots paragraphs =
  User_message.make ?loc ?hints ?annots paragraphs ~prefix

let raise ?loc ?hints ?annots paragraphs =
  raise (E (make ?loc ?hints ?annots paragraphs))

let () =
  Printexc.register_printer (function
    | E ({ annots = []; _ } as t) ->
      Some (Format.asprintf "%a@?" Pp.to_fmt (User_message.pp t))
    | E t ->
      let open Pp.O in
      let pp =
        User_message.pp t
        ++ Pp.vbox
             (Pp.concat_map t.annots ~f:(fun annot ->
                  Pp.box (User_message.Annot.pp annot) ++ Pp.cut))
      in
      Some (Format.asprintf "%a" Pp.to_fmt pp)
    | _ -> None)

let reporter = ref (fun msg -> User_message.prerr msg)

let set_reporter f = reporter := f

let emit ?loc ?hints paragraphs =
  !reporter
    (User_message.make paragraphs ?loc ?hints
       ~prefix:(Pp.seq
                  (Pp.tag (Pp.verbatim "Warning")
                     ~tag:User_message.Style.Warning)
                  (Pp.char ':')))

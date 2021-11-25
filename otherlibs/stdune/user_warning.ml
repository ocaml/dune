let reporter = ref (fun msg -> User_message.prerr msg)

let set_reporter f = reporter := f

let emit ?loc ?hints ?(is_error = false) paragraphs =
  if is_error then
    User_error.raise ?loc ?hints paragraphs
  else
    !reporter
      (User_message.make paragraphs ?loc ?hints
         ~prefix:
           (Pp.seq
              (Pp.tag User_message.Style.Warning (Pp.verbatim "Warning"))
              (Pp.char ':')))

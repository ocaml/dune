let reporter = ref (fun msg -> User_message.prerr msg)
let set_reporter f = reporter := f

let prefix =
  Pp.seq (Pp.tag User_message.Style.Warning (Pp.verbatim "Warning")) (Pp.char ':')
;;

let prefix_paragraphs paragraphs =
  match paragraphs with
  | [] -> [ prefix ]
  | x :: l -> Pp.concat ~sep:Pp.space [ prefix; x ] :: l
;;

let emit_message (message : User_message.t) =
  let paragraphs = prefix_paragraphs message.paragraphs in
  !reporter { message with paragraphs }
;;

let emit ?loc ?hints ?(is_error = false) paragraphs =
  if is_error
  then User_error.raise ?loc ?hints paragraphs
  else !reporter (User_message.make paragraphs ?loc ?hints ~prefix)
;;

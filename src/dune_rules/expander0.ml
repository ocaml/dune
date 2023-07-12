open Import

let isn't_allowed_in_this_position ~(source : Dune_lang.Template.Pform.t) =
  let exn =
    User_error.make ~loc:source.loc
      [ Pp.textf "%s isn't allowed in this position."
          (Dune_lang.Template.Pform.describe source)
      ]
  in
  raise (User_error.E exn)

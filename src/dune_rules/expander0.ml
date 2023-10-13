open Import

let isn't_allowed_in_this_position ~(source : Dune_lang.Template.Pform.t) =
  let exn =
    User_error.make
      ~loc:source.loc
      [ Pp.textf
          "%s isn't allowed in this position."
          (Dune_lang.Template.Pform.describe source)
      ]
  in
  raise (User_error.E exn)
;;

let as_in_build_dir ~what ~loc p =
  match Path.as_in_build_dir p with
  | Some p -> p
  | None ->
    User_error.raise
      ~loc
      [ Pp.textf
          "%s %s is outside the build directory. This is not allowed."
          what
          (Path.to_string_maybe_quoted p)
      ]
;;

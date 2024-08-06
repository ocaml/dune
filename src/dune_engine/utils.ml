open Import

let not_found fmt ?loc ?context ?hint x =
  User_error.make
    ?loc
    (Pp.textf fmt (String.maybe_quoted x)
     ::
     (match context with
      | None -> []
      | Some name -> [ Pp.textf " (context: %s)" (Context_name.to_string name) ]))
    ~hints:
      (match hint with
       | None -> []
       | Some hint -> [ Pp.text hint ])
;;

let program_not_found_message ?context ?hint ~loc prog =
  not_found "Program %s not found in the tree or in PATH" ?context ?hint ?loc prog
;;

let program_not_found ?context ?hint ~loc prog =
  raise (User_error.E (program_not_found_message ?context ?hint ~loc prog))
;;

let pp_command_hint command =
  let open Pp.O in
  Pp.textf "try:" ++ Pp.newline ++ Pp.cut ++ Pp.hbox (Pp.textf "  " ++ Pp.verbatim command)
;;

open Import

type t =
  | Executables of (Loc.t * string) Nonempty_list.t
  | Melange_emit of string

let executables names = Executables names
let melange_emit name = Melange_emit name

let compilation_mode = function
  | Executables _ -> Compilation_mode.Ocaml
  | Melange_emit _ -> Melange
;;

let names = function
  | Executables names -> Nonempty_list.map names ~f:snd
  | Melange_emit name -> Nonempty_list.[ name ]
;;

let first_name = function
  | Executables ((_, name) :: _) | Melange_emit name -> name
;;

let description = function
  | Melange_emit name -> Pp.textf "melange target %s" name
  | Executables Nonempty_list.[ (loc, name) ] ->
    Pp.textf "executable %s in %s" name (Loc.to_file_colon_line loc)
  | Executables (Nonempty_list.((loc, _) :: _) as names) ->
    Pp.textf
      "executables %s in %s"
      (String.enumerate_and (Nonempty_list.map ~f:snd names |> Nonempty_list.to_list))
      (Loc.to_file_colon_line loc)
;;

let repr =
  let executables =
    Repr.view (Repr.list (Repr.pair Loc.repr Repr.string)) ~to_:Nonempty_list.to_list
  in
  Repr.variant
    "exe-target"
    [ Repr.case "Executables" executables ~proj:(function
        | Executables names -> Some names
        | Melange_emit _ -> None)
    ; Repr.case "Melange_emit" Repr.string ~proj:(function
        | Melange_emit name -> Some name
        | Executables _ -> None)
    ]
;;

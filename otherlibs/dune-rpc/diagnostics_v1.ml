open Import
open Exported_types

module Related = struct
  type t =
    { message : unit Pp.t
    ; loc : Loc.t
    }

  let sexp =
    let open Conv in
    record
      (Record.make (fun loc message -> { loc; message })
       |> Record.field "loc" (required Loc.sexp) ~get:(fun { loc; _ } -> loc)
       |> Record.field "message" (required sexp_pp_unit) ~get:(fun { message; _ } ->
         message)
       |> Record.finish)
  ;;

  let to_diagnostic_related t : Diagnostic.Related.t =
    { message = t.message |> Pp.map_tags ~f:(fun _ -> User_message.Style.Details)
    ; loc = t.loc
    }
  ;;

  let of_diagnostic_related (t : Diagnostic.Related.t) =
    { message = t.message |> Pp.map_tags ~f:(fun _ -> ()); loc = t.loc }
  ;;
end

type t = (unit Pp.t, Related.t) Diagnostic.fields

let sexp = Diagnostic.sexp_fields ~message:sexp_pp_unit ~related:Related.sexp

let to_diagnostic (t : t) : Diagnostic.t =
  { targets = t.targets
  ; message = t.message |> Pp.map_tags ~f:(fun _ -> User_message.Style.Details)
  ; loc = t.loc
  ; severity = t.severity
  ; promotion = t.promotion
  ; directory = t.directory
  ; id = t.id
  ; related = t.related |> List.map ~f:Related.to_diagnostic_related
  }
;;

let of_diagnostic (t : Diagnostic.t) : t =
  { targets = t.targets
  ; message = t.message |> Pp.map_tags ~f:(fun _ -> ())
  ; loc = t.loc
  ; severity = t.severity
  ; promotion = t.promotion
  ; directory = t.directory
  ; id = t.id
  ; related = t.related |> List.map ~f:Related.of_diagnostic_related
  }
;;

module Event = struct
  type nonrec t =
    | Add of t
    | Remove of t

  let sexp =
    let diagnostic = sexp in
    let open Conv in
    let add = constr "Add" diagnostic (fun a -> Add a) in
    let remove = constr "Remove" diagnostic (fun a -> Remove a) in
    sum
      [ econstr add; econstr remove ]
      (function
        | Add t -> case t add
        | Remove t -> case t remove)
  ;;

  let to_event : t -> Diagnostic.Event.t = function
    | Add t -> Add (to_diagnostic t)
    | Remove t -> Remove (to_diagnostic t)
  ;;

  let of_event : Diagnostic.Event.t -> t = function
    | Add t -> Add (of_diagnostic t)
    | Remove t -> Remove (of_diagnostic t)
  ;;
end

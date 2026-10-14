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

type t =
  { targets : Target.t list
  ; id : Diagnostic.Id.t
  ; message : unit Pp.t
  ; loc : Loc.t option
  ; severity : Diagnostic.severity option
  ; promotion : Diagnostic.Promotion.t list
  ; directory : string option
  ; related : Related.t list
  }

let sexp_severity =
  let open Conv in
  enum [ "error", Diagnostic.Error; "warning", Warning ]
;;

let sexp =
  let open Conv in
  record
    (Record.make (fun targets message loc severity promotion directory id related ->
       { targets; message; loc; severity; promotion; directory; id; related })
     |> Record.field
          "targets"
          (required (list Target.sexp))
          ~get:(fun { targets; _ } -> targets)
     |> Record.field "message" (required sexp_pp_unit) ~get:(fun { message; _ } ->
       message)
     |> Record.field "loc" (optional Loc.sexp) ~get:(fun { loc; _ } -> loc)
     |> Record.field "severity" (optional sexp_severity) ~get:(fun { severity; _ } ->
       severity)
     |> Record.field
          "promotion"
          (required (list Diagnostic.Promotion.sexp))
          ~get:(fun { promotion; _ } -> promotion)
     |> Record.field "directory" (optional string) ~get:(fun { directory; _ } ->
       directory)
     |> Record.field "id" (required Diagnostic.Id.sexp) ~get:(fun { id; _ } -> id)
     |> Record.field
          "related"
          (required (list Related.sexp))
          ~get:(fun { related; _ } -> related)
     |> Record.finish)
;;

let to_diagnostic t : Diagnostic.t =
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

let of_diagnostic (t : Diagnostic.t) =
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
    Conv.iso
      (event_sexp sexp)
      (function
        | `Add t -> Add t
        | `Remove t -> Remove t)
      (function
        | Add t -> `Add t
        | Remove t -> `Remove t)
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

open Import
open Exported_types

module Related = struct
  type t =
    { message : unit Pp.t
    ; loc : Loc.t
    }

  let sexp =
    let open Conv in
    let loc = field "loc" (required Loc.sexp) in
    let message = field "message" (required sexp_pp_unit) in
    let to_ (loc, message) = { loc; message } in
    let from { loc; message } = loc, message in
    iso (record (both loc message)) to_ from
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
  let from { targets; message; loc; severity; promotion; directory; id; related } =
    targets, message, loc, severity, promotion, directory, id, related
  in
  let to_ (targets, message, loc, severity, promotion, directory, id, related) =
    { targets; message; loc; severity; promotion; directory; id; related }
  in
  let loc = field "loc" (optional Loc.sexp) in
  let message = field "message" (required sexp_pp_unit) in
  let targets = field "targets" (required (list Target.sexp)) in
  let severity = field "severity" (optional sexp_severity) in
  let directory = field "directory" (optional string) in
  let promotion = field "promotion" (required (list Diagnostic.Promotion.sexp)) in
  let id = field "id" (required Diagnostic.Id.sexp) in
  let related = field "related" (required (list Related.sexp)) in
  iso
    (record (eight targets message loc severity promotion directory id related))
    to_
    from
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

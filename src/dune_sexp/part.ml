open Stdune

module Pform = struct
  module Payload = struct
    type t = string

    let of_string t = t
    let to_string t = t
    let repr = Repr.view Repr.string ~to_:to_string
    let to_dyn = Repr.to_dyn repr
    let equal = String.equal
    let compare = String.compare

    module Args = struct
      let sep = ':'
      let whole t = t

      let lsplit2 t loc =
        match String.lsplit2 t ~on:sep with
        | Some args -> Ok args
        | None ->
          Error
            (User_error.make
               ~loc
               [ Pp.textf
                   "Expected two arguments separated by '%c' but no '%c' found."
                   sep
                   sep
               ])
      ;;

      let split t = String.split t ~on:sep
    end

    let of_args = String.concat ~sep:(String.make 1 Args.sep)
  end

  type t =
    { loc : Loc.t
    ; name : string
    ; payload : Payload.t option
    }

  let loc (t : t) = t.loc

  let payload_loc t =
    if Option.is_some t.payload
    then (
      let loc_with_start =
        Loc.set_start
          t.loc
          { (Loc.start t.loc) with
            (* Add 3 for the "%{" at the start of the pform and then ":"
               separating the name from the payload *)
            pos_cnum = Loc.start_pos_cnum t.loc + String.length t.name + 3
          }
      in
      Loc.set_stop
        loc_with_start
        { (Loc.stop loc_with_start) with
          (* Subtract 1 for the "}" at the end of the pform *)
          pos_cnum = Loc.stop_pos_cnum loc_with_start - 1
        })
    else Loc.none
  ;;

  let name { name; _ } = name

  let equal { name; payload; loc } t =
    String.equal name t.name
    && Option.equal Payload.equal payload t.payload
    && Loc.equal loc t.loc
  ;;

  let payload t = t.payload

  let to_string { loc = _; name; payload } =
    let before, after = "%{", "}" in
    match payload with
    | None -> before ^ name ^ after
    | Some p -> before ^ name ^ ":" ^ Payload.to_string p ^ after
  ;;

  let repr =
    Repr.record
      "dune-sexp-template-pform"
      [ Repr.field "name" Repr.string ~get:name
      ; Repr.field "payload" (Repr.option Payload.repr) ~get:payload
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
  let with_name t ~name = { t with name }

  let describe t =
    to_string
      (match t.payload with
       | None -> t
       | Some _ -> { t with payload = Some (Payload.of_string "..") })
  ;;

  let describe_kind t =
    match t.payload with
    | None -> "variable"
    | Some _ -> "macro"
  ;;
end

type t =
  | Text of string
  | Pform of Pform.t

let equal s1 s2 =
  match s1, s2 with
  | Text s1, Text s2 -> String.equal s1 s2
  | Pform v1, Pform v2 -> Pform.equal v1 v2
  | _ -> false
;;

let repr =
  Repr.variant
    "dune-sexp-template-part"
    [ Repr.case "Text" Repr.string ~proj:(function
        | Text s -> Some s
        | Pform _ -> None)
    ; Repr.case "Pform" Pform.repr ~proj:(function
        | Text _ -> None
        | Pform pform -> Some pform)
    ]
;;

let to_dyn = function
  | Text s -> Dyn.variant "Text" [ Dyn.string s ]
  | Pform v -> Dyn.variant "Pform" [ Pform.to_dyn v ]
;;

let remove_locs = function
  | Text s -> Text s
  | Pform v -> Pform { v with loc = Loc.none }
;;

let list_to_string parts =
  String.concat
    ~sep:""
    (List.map parts ~f:(function
       | Text s -> s
       | Pform pf -> Pform.to_string pf))
;;

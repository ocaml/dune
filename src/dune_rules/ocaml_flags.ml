open Import
open Action_builder.O

let default_ocamlc_flags = [ "-g" ]
let default_ocamlopt_flags = [ "-g" ]

let dev_mode_warnings =
  (* New warnings should be introduced here *)
  let all =
    Int.Set.diff
      (* TODO update this list once OCaml versions are out *)
      (Int.Set.of_list (List.init 70 ~f:succ))
      (Int.Set.of_list
         [ 4; 29; 40; 41; 42; 44; 45; 48; 58; 59; 60; 63; 64; 65; 66; 67; 68; 69; 70 ])
  in
  let add n range = Int.Set.add range n in
  let remove n range = Int.Set.remove range n in
  let warnings_range ws =
    let wrange_to_flag (x, y) =
      if x = y then sprintf "@%d" x else sprintf "@%d..%d" x y
    in
    Int.Set.fold ws ~init:[] ~f:(fun x acc ->
      match acc with
      | [] -> [ x, x ]
      | (l, u) :: acc when succ u = x -> (l, x) :: acc
      | _ -> (x, x) :: acc)
    |> List.rev_map ~f:wrange_to_flag
    |> String.concat ~sep:""
  in
  let and_warnings lazy_range =
    lazy_range, lazy (warnings_range (Lazy.force lazy_range))
  in
  let range_pre_3_3, pre_3_3 = and_warnings @@ lazy all in
  let range_pre_3_13, pre_3_13 =
    and_warnings @@ lazy (Lazy.force range_pre_3_3 |> add 67 |> add 69)
  in
  let _range_later, later =
    and_warnings @@ lazy (Lazy.force range_pre_3_13 |> remove 30)
  in
  fun ~dune_version ->
    if dune_version < (3, 3)
    then Lazy.force pre_3_3
    else if dune_version < (3, 13)
    then Lazy.force pre_3_13
    else Lazy.force later
;;

let vendored_warnings = [ "-w"; "-a" ]
let vendored_alerts = [ "-alert"; "-all" ]
let default_warnings = "-40"

let default_flags ~dune_version ~profile =
  if Profile.is_dev profile
  then
    [ "-w"
    ; dev_mode_warnings ~dune_version ^ default_warnings
    ; "-strict-sequence"
    ; "-strict-formats"
    ; "-short-paths"
    ; "-keep-locs"
    ]
  else [ "-w"; default_warnings ]
;;

type 'a t' =
  { common : 'a
  ; keywords : string option * string list
  ; specific : 'a Lib_mode.Map.t
  }

module Keywords = struct
  let empty = None, []
  let standard = empty

  let equal (v1, e1) (v2, e2) =
    Option.equal String.equal v1 v2 && List.equal String.equal e1 e2
  ;;

  let to_string_list t =
    match t.keywords with
    | None, [] -> []
    | Some version, [] -> "-keywords" :: [ version ]
    | None, extra_l ->
      let keywords_arg = "" :: extra_l |> String.concat ~sep:"+" in
      "-keywords" :: [ keywords_arg ]
    | Some version, extra_l ->
      let keywords_arg = version :: extra_l |> String.concat ~sep:"+" in
      "-keywords" :: [ keywords_arg ]
  ;;
end

let equal f { common; keywords; specific } t =
  f common t.common
  && Lib_mode.Map.equal f specific t.specific
  && Keywords.equal keywords t.keywords
;;

module Spec = struct
  type t = Ordered_set_lang.Unexpanded.t t'

  let equal = equal Ordered_set_lang.Unexpanded.equal

  let standard =
    { common = Ordered_set_lang.Unexpanded.standard
    ; keywords = Keywords.standard
    ; specific = Lib_mode.Map.make_all Ordered_set_lang.Unexpanded.standard
    }
  ;;

  let make ~common ~keywords ~specific : t = { common; keywords; specific }

  let decode_keywords =
    let open Dune_lang.Decoder in
    fields
      (let+ version = field_o "version" string
       and+ extra = field_o "extra" (repeat string) in
       version, Option.value extra ~default:[])
  ;;

  let decode =
    let open Dune_lang.Decoder in
    let field_oslu = Ordered_set_lang.Unexpanded.field in
    let+ common = field_oslu "flags"
    and+ byte = field_oslu "ocamlc_flags"
    and+ native = field_oslu "ocamlopt_flags"
    and+ keywords =
      field
        "keywords"
        (Dune_lang.Syntax.since Stanza.syntax (3, 18) >>> decode_keywords)
        ~default:Keywords.empty
    and+ melange =
      field_oslu
        ~check:(Dune_lang.Syntax.since Melange_stanzas.syntax (0, 1))
        "melange.compile_flags"
    in
    let specific = Lib_mode.Map.make ~byte ~native ~melange in
    { common; keywords; specific }
  ;;
end

type t = string list Action_builder.t t'

let empty =
  let build = Action_builder.return [] in
  { common = build; keywords = Keywords.empty; specific = Lib_mode.Map.make_all build }
;;

let of_list l = { empty with common = Action_builder.return l }

let default ~dune_version ~profile =
  { common = Action_builder.return (default_flags ~dune_version ~profile)
  ; keywords = Keywords.standard
  ; specific =
      { ocaml =
          { byte = Action_builder.return default_ocamlc_flags
          ; native = Action_builder.return default_ocamlopt_flags
          }
      ; melange = Action_builder.return default_ocamlc_flags
      }
  }
;;

let make ~spec ~default ~eval =
  let f name x standard =
    Action_builder.memoize ~cutoff:(List.equal String.equal) name (eval x ~standard)
  in
  { common = f "common flags" spec.common default.common
  ; keywords = spec.keywords
  ; specific =
      { ocaml =
          { byte = f "ocamlc flags" spec.specific.ocaml.byte default.specific.ocaml.byte
          ; native =
              f "ocamlopt flags" spec.specific.ocaml.native default.specific.ocaml.native
          }
      ; melange = f "melange compile_flags" spec.specific.melange default.specific.melange
      }
  }
;;

let get t mode =
  let+ common = t.common
  and+ specific = Lib_mode.Map.get t.specific mode in
  common @ specific
;;

let map_common t ~f =
  let common =
    let+ l = t.common in
    f l
  in
  { t with common }
;;

let append_common t flags = map_common t ~f:(fun l -> l @ flags)
let with_vendored_warnings t = append_common t vendored_warnings
let with_vendored_alerts t = append_common t vendored_alerts
let with_keywords t = Keywords.to_string_list t |> append_common t

let dump t =
  let+ common = t.common
  and+ byte = t.specific.ocaml.byte
  and+ native = t.specific.ocaml.native
  and+ melange = t.specific.melange in
  List.map
    ~f:Dune_lang.Encoder.(pair string (list string))
    [ "flags", common
    ; "ocamlc_flags", byte
    ; "ocamlopt_flags", native
    ; "melange.compile_flags", melange
    ]
;;

let with_vendored_flags flags ~ocaml_version =
  let with_warnings = with_vendored_warnings flags in
  if Ocaml.Version.supports_alerts ocaml_version
  then with_vendored_alerts with_warnings
  else with_warnings
;;

let allow_only_melange t =
  let ocaml =
    Ocaml.Mode.Dict.make_both
      (Action_builder.fail
         { fail =
             (fun () ->
               Code_error.raise
                 "only melange flags are allowed to be evaluated with this flags set"
                 [])
         })
  in
  { t with specific = { t.specific with ocaml } }
;;

let open_flags modules =
  List.concat_map modules ~f:(fun name -> [ "-open"; Module_name.to_string name ])
;;

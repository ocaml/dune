open Import
open Action_builder.O
module Ocaml_flags = Dune_lang.Ocaml_flags
open Ocaml_flags

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

let dune_warnings ~dune_version ~profile =
  if Profile.is_dev profile
  then
    [ "-w"
    ; dev_mode_warnings ~dune_version ^ default_warnings
    ; "-strict-sequence"
    ; "-strict-formats"
    ]
  else [ "-w"; default_warnings ]
;;

let default_flags ~dune_version ~profile =
  (if dune_version < (3, 21) then dune_warnings ~dune_version ~profile else [])
  @
  if Profile.is_dev profile
  then
    "-short-paths"
    :: "-keep-locs"
    :: (if dune_version >= (3, 21) then [ "-warn-error"; "+a" ] else [])
  else []
;;

(* NOTE(anmonteiro): we track `nostdlib` separately to avoid repeating flags in
   the case of compiling the stdlib module alias. This may be removed once
   Melange accepts duplicate `-nopervasives` CLI flags. *)
type t =
  { nostdlib : bool
  ; flags : string list Action_builder.t Dune_lang.Ocaml_flags.t
  }

let empty =
  let build = Action_builder.return [] in
  { nostdlib = false
  ; flags = { Ocaml_flags.common = build; specific = Lib_mode.Map.make_all build }
  }
;;

let of_list l =
  { empty with flags = { empty.flags with common = Action_builder.return l } }
;;

let default ~dune_version ~profile =
  { nostdlib = false
  ; flags =
      { common = Action_builder.return (default_flags ~dune_version ~profile)
      ; specific =
          { ocaml =
              { byte = Action_builder.return default_ocamlc_flags
              ; native = Action_builder.return default_ocamlopt_flags
              }
          ; melange = Action_builder.return default_ocamlc_flags
          }
      }
  }
;;

let make ~(spec : Ocaml_flags.Spec.t) ~default ~eval =
  let f name x standard =
    Action_builder.memoize ~cutoff:(List.equal String.equal) name (eval x ~standard)
  in
  { nostdlib = false
  ; flags =
      { common = f "common flags" spec.common default.flags.common
      ; specific =
          { ocaml =
              { byte =
                  f
                    "ocamlc flags"
                    spec.specific.ocaml.byte
                    default.flags.specific.ocaml.byte
              ; native =
                  f
                    "ocamlopt flags"
                    spec.specific.ocaml.native
                    default.flags.specific.ocaml.native
              }
          ; melange =
              f
                "melange compile_flags"
                spec.specific.melange
                default.flags.specific.melange
          }
      }
  }
;;

let nostdlib_flags = [ "-nopervasives"; "-nostdlib" ]

let get t mode =
  let+ common = t.flags.common
  and+ specific = Lib_mode.Map.get t.flags.specific mode in
  let nostdlib = if t.nostdlib then nostdlib_flags else [] in
  nostdlib @ common @ specific
;;

let map_common t ~f =
  let common =
    let+ l = t.common in
    f l
  in
  { t with common }
;;

let append_common t flags = { t with flags = map_common t.flags ~f:(fun l -> l @ flags) }
let append_nostdlib t = { t with nostdlib = true }
let with_vendored_warnings t = append_common t vendored_warnings
let with_vendored_alerts t = append_common t vendored_alerts

let dump t =
  let+ common = t.flags.common
  and+ byte = t.flags.specific.ocaml.byte
  and+ native = t.flags.specific.ocaml.native
  and+ melange = t.flags.specific.melange in
  List.map
    ~f:Dune_lang.Encoder.(pair string (list string))
    [ "flags", (if t.nostdlib then nostdlib_flags else []) @ common
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
  { t with flags = { t.flags with specific = { t.flags.specific with ocaml } } }
;;

let open_flags modules =
  List.concat_map modules ~f:(fun name -> [ "-open"; Module_name.to_string name ])
;;

open! Stdune
open Import
open Build.O

let default_ocamlc_flags = [ "-g" ]

let default_ocamlopt_flags = [ "-g" ]

let dev_mode_warnings =
  (* New warnings should be introduced here *)
  let all =
    Int.Set.diff
      (Int.Set.of_list (List.init 62 ~f:succ))
      (Int.Set.of_list [ 4; 29; 40; 41; 42; 44; 45; 48; 58; 59; 60 ])
  in
  let warnings_range ws =
    let wrange_to_flag (x, y) =
      if x = y then
        sprintf "@%d" x
      else
        sprintf "@%d..%d" x y
    in
    let acc, last_range =
      Int.Set.fold ws ~init:([], None) ~f:(fun x (acc, last_range) ->
          match last_range with
          | None ->
            assert (acc = []);
            ([], Some (x, x))
          | Some (l, u) ->
            if succ u = x then
              (acc, Some (l, succ u))
            else
              (wrange_to_flag (l, u) :: acc, Some (x, x)))
    in
    let acc =
      match last_range with
      | None -> acc
      | Some (x, y) -> wrange_to_flag (x, y) :: acc
    in
    List.rev acc |> String.concat ~sep:""
  in
  fun ~dune_version:_ -> warnings_range all

let vendored_warnings = [ "-w"; "-a" ]

let default_warnings = "-40"

let default_flags ~dune_version ~profile =
  if Profile.is_dev profile then
    [ "-w"
    ; dev_mode_warnings ~dune_version ^ default_warnings
    ; "-strict-sequence"
    ; "-strict-formats"
    ; "-short-paths"
    ; "-keep-locs"
    ]
  else
    [ "-w"; default_warnings ]

type 'a t' =
  { common : 'a
  ; specific : 'a Mode.Dict.t
  }

let equal f { common; specific } t =
  f common t.common && Mode.Dict.equal f specific t.specific

module Spec = struct
  type t = Ordered_set_lang.Unexpanded.t t'

  let equal = equal Ordered_set_lang.Unexpanded.equal

  let standard =
    { common = Ordered_set_lang.Unexpanded.standard
    ; specific = Mode.Dict.make_both Ordered_set_lang.Unexpanded.standard
    }

  let decode =
    let open Dune_lang.Decoder in
    let field_oslu = Ordered_set_lang.Unexpanded.field in
    let+ common = field_oslu "flags"
    and+ byte = field_oslu "ocamlc_flags"
    and+ native = field_oslu "ocamlopt_flags" in
    let specific = Mode.Dict.make ~native ~byte in
    { common; specific }
end

type t = string list Build.t t'

let empty =
  let build = Build.return [] in
  { common = build; specific = Mode.Dict.make_both build }

let of_list l = { empty with common = Build.return l }

let default ~dune_version ~profile =
  { common = Build.return (default_flags ~dune_version ~profile)
  ; specific =
      { byte = Build.return default_ocamlc_flags
      ; native = Build.return default_ocamlopt_flags
      }
  }

let make ~spec ~default ~eval =
  let f name x standard = Build.memoize name (eval x ~standard) in
  { common = f "common flags" spec.common default.common
  ; specific =
      { byte = f "ocamlc flags" spec.specific.byte default.specific.byte
      ; native = f "ocamlopt flags" spec.specific.native default.specific.native
      }
  }

let get t mode =
  let+ common = t.common
  and+ specific = Mode.Dict.get t.specific mode in
  common @ specific

let map_common t ~f =
  let common =
    let+ l = t.common in
    f l
  in
  { t with common }

let append_common t flags = map_common t ~f:(fun l -> l @ flags)

let prepend_common flags t = map_common t ~f:(fun l -> flags @ l)

let with_vendored_warnings t = append_common t vendored_warnings

let common t = t.common

let dump t =
  let+ common = t.common
  and+ byte = t.specific.byte
  and+ native = t.specific.native in
  List.map
    ~f:Dune_lang.Encoder.(pair string (list string))
    [ ("flags", common); ("ocamlc_flags", byte); ("ocamlopt_flags", native) ]

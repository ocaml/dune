open Import
open Build.O

let default_ocamlc_flags   = Utils.g
let default_ocamlopt_flags = Utils.g

let dev_mode_warnings =
  "@a" ^
  String.concat ~sep:""
    (List.map ~f:(sprintf "-%d")
       [ 4
       ; 29
       ; 40
       ; 41
       ; 42
       ; 44
       ; 45
       ; 48
       ; 58
       ; 59
       ; 60
       ])

let default_flags () =
  if !Clflags.dev_mode then
    [ "-w"; dev_mode_warnings ^ !Clflags.warnings
    ; "-strict-sequence"
    ; "-strict-formats"
    ; "-short-paths"
    ; "-keep-locs"
    ]
  else
    [ "-w"; !Clflags.warnings ]

type t =
  { common     : (unit, string list) Build.t
  ; specific   : (unit, string list) Build.t Mode.Dict.t
  }

let empty =
  let build = Build.arr (fun () -> []) in
  { common   = build
  ; specific = Mode.Dict.make_both build
  }

let of_list l =
  { empty with common = Build.arr (fun () -> l) }

let make { Jbuild.Buildable. flags; ocamlc_flags; ocamlopt_flags; _ } ctx ~scope ~dir =
  let eval = Super_context.expand_and_eval_set ctx ~scope ~dir in
  { common   = Build.memoize "common flags" (eval flags ~standard:(default_flags ()))
  ; specific =
      { byte   = Build.memoize "ocamlc flags" (eval ocamlc_flags   ~standard:(default_ocamlc_flags ()))
      ; native = Build.memoize "ocamlopt flags" (eval ocamlopt_flags ~standard:(default_ocamlopt_flags ()))
      }
  }

let get t mode =
  t.common
  &&&
  (Mode.Dict.get t.specific mode)
  >>^ fun (common, specific) ->
  common @ specific

let get_for_cm t ~cm_kind = get t (Mode.of_cm_kind cm_kind)

let default () =
  { common = Build.return (default_flags ())
  ; specific =
      { byte   = Build.return (default_ocamlc_flags   ())
      ; native = Build.return (default_ocamlopt_flags ())
      }
  }

let append_common t flags = {t with common = t.common >>^ fun l -> l @ flags}

let prepend_common flags t = {t with common = t.common >>^ fun l -> flags @ l}

let common t = t.common

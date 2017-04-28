open Import

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
  { common   : string list
  ; specific : string list Mode.Dict.t
  }

let make { Jbuild_types.Buildable. flags; ocamlc_flags; ocamlopt_flags; _ } =
  let eval = Ordered_set_lang.eval_with_standard in
  { common   = eval flags ~standard:(default_flags ())
  ; specific =
      { byte   = eval ocamlc_flags   ~standard:(default_ocamlc_flags ())
      ; native = eval ocamlopt_flags ~standard:(default_ocamlopt_flags ())
      }
  }

let get t mode = Arg_spec.As (t.common @ Mode.Dict.get t.specific mode)

let get_for_cm t ~cm_kind = get t (Mode.of_cm_kind cm_kind)

let default () =
  { common = default_flags ()
  ; specific =
      { byte   = default_ocamlc_flags   ()
      ; native = default_ocamlopt_flags ()
      }
  }

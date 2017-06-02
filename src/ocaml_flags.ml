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
  { common   : string list Per_file.t
  ; specific : string list Per_file.t Mode.Dict.t
  }

let make { Jbuild_types.Buildable. flags; ocamlc_flags; ocamlopt_flags; _ } =
  let (>>|) = Per_file.(>>|) in
  let eval = Ordered_set_lang.eval_with_standard in
  { common   = flags >>| eval ~standard:(default_flags ())
  ; specific =
      { byte   = ocamlc_flags >>| eval ~standard:(default_ocamlc_flags ())
      ; native = ocamlopt_flags >>| eval ~standard:(default_ocamlopt_flags ())
      }
  }

let get t mode ~target =
  let _get = Per_file.get ~target ~default:[] in
  Arg_spec.As (_get t.common @ _get (Mode.Dict.get t.specific mode))

let get_for_cm t ~target ~cm_kind = get t (Mode.of_cm_kind cm_kind) target

let default () =
  let open Per_file in
  { common = as_forall (default_flags ())
  ; specific =
      { byte   = as_forall (default_ocamlc_flags ())
      ; native = as_forall (default_ocamlopt_flags ())
      }
  }

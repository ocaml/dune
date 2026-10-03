open Import

type purpose =
  | Workspace
  | Mounted

type t =
  { resolver : Context.t
  ; output_root : Path.Build.t
  ; purpose : purpose
  }

let workspace resolver =
  { resolver; output_root = Context.build_dir resolver; purpose = Workspace }
;;

let mounted ~resolver ~output_root = { resolver; output_root; purpose = Mounted }
let build_context t = Context.build_context t.resolver
let output_root t = t.output_root
let purpose t = t.purpose

let to_dyn t =
  Dyn.record
    [ "resolver", Context.to_dyn_concise t.resolver
    ; "output_root", Path.Build.to_dyn t.output_root
    ; ( "purpose"
      , Dyn.variant
          (match t.purpose with
           | Workspace -> "Workspace"
           | Mounted -> "Mounted")
          [] )
    ]
;;

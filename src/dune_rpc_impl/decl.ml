open Import
open Dune_rpc

module Status = struct
  module Menu = struct
    type t =
      | Uninitialized
      | Menu of (string * int) list

    let sexp =
      let open Conv in
      let menu = constr "menu" (list (pair string int)) (fun m -> Menu m) in
      let uninitialized = constr "stage1" unit (fun () -> Uninitialized) in
      let variants = [ econstr menu; econstr uninitialized ] in
      sum variants (function
        | Uninitialized -> case () uninitialized
        | Menu m -> case m menu)
    ;;
  end

  type t = { clients : (Id.t * Menu.t) list }

  let sexp =
    let open Conv in
    let to_ clients = { clients } in
    let from { clients } = clients in
    iso (list (pair Id.sexp Menu.sexp)) to_ from
  ;;

  let v1 = Decl.Request.make_current_gen ~req:Conv.unit ~resp:sexp ~version:1
  let decl = Decl.Request.make ~method_:"status" ~generations:[ v1 ]
end

module Build = struct
  let v1 =
    Decl.Request.make_current_gen
      ~req:(Conv.list Conv.string)
      ~resp:Dune_rpc.Build_outcome_with_diagnostics.sexp_v1
      ~version:1
  ;;

  let v2 =
    Decl.Request.make_current_gen
      ~req:(Conv.list Conv.string)
      ~resp:Dune_rpc.Build_outcome_with_diagnostics.sexp_v2
      ~version:2
  ;;

  let decl = Decl.Request.make ~method_:"build" ~generations:[ v1; v2 ]
end
(*
   module Format = struct
  let sexp : Dune_engine.Clflags.Promote.t Conv.value =
    let open Conv in
    let auto =
      constr "Automatically" unit (fun () -> Dune_engine.Clflags.Promote.Automatically)
    in
    let never = constr "Never" unit (fun () -> Dune_engine.Clflags.Promote.Never) in
    sum
      [ econstr auto; econstr never ]
      (function
        | Dune_engine.Clflags.Promote.Automatically -> case () auto
        | Never -> case () never)
  ;;

  let v1 =
    Decl.Request.make_current_gen
      ~req:sexp
      ~resp:Build_outcome_with_diagnostics.sexp_v2
      ~version:1
  ;;

  let decl = Decl.Request.make ~method_:"format" ~generations:[ v1 ]
end *)

let build = Build.decl
let status = Status.decl
(* let format = Format.decl *)

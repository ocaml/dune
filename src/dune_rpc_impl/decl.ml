open Dune_rpc_private

(* Internal RPC requests *)

module Build_outcome = struct
  type t = Dune_engine.Scheduler.Run.Build_outcome_for_rpc.t =
    | Success
    | Failure

  let sexp = Conv.enum [ ("Success", Success); ("Failure", Failure) ]
end

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
  end

  type t = { clients : (Id.t * Menu.t) list }

  let sexp =
    let open Conv in
    let to_ clients = { clients } in
    let from { clients } = clients in
    iso (list (pair Id.sexp Menu.sexp)) to_ from

  let v1 = Decl.Request.make_current_gen ~req:Conv.unit ~resp:sexp ~version:1

  let decl = Decl.Request.make ~method_:"status" ~generations:[ v1 ]
end

module Build = struct
  let v1 =
    Decl.Request.make_current_gen ~req:(Conv.list Conv.string)
      ~resp:Build_outcome.sexp ~version:1

  let decl = Decl.Request.make ~method_:"build" ~generations:[ v1 ]
end

let build = Build.decl

let status = Status.decl

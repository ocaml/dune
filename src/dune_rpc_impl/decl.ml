open Dune_rpc_private

module Build_outcome = struct
  type t = Dune_engine.Scheduler.Run.Build_outcome_for_rpc.t =
    | Success
    | Failure

  let sexp = Conv.enum [ ("Success", Success); ("Failure", Failure) ]
end

let build = Decl.request ~method_:"build" Conv.(list string) Build_outcome.sexp

let shutdown = Decl.notification ~method_:"shutdown" Conv.unit

module Status = struct
  type t = { clients : Id.t list }

  let sexp =
    let open Conv in
    let to_ clients = { clients } in
    let from { clients } = clients in
    iso (list Id.sexp) to_ from
end

let status = Decl.request ~method_:"status" Conv.unit Status.sexp

open Import
open Dune_rpc

module Compound_user_error = struct
  include Dune_engine.Compound_user_error

  let sexp =
    let open Conv in
    let from { main; related } = main, related in
    let to_ (main, related) = make ~main ~related in
    let main = field "main" (required User_message.sexp_without_annots) in
    let related = field "related" (required (list User_message.sexp_without_annots)) in
    iso (record (both main related)) to_ from
  ;;
end

module Build_outcome_with_diagnostics = struct
  type t =
    | Success
    | Failure of Dune_engine.Compound_user_error.t list

  let sexp_v1 =
    let open Conv in
    let success = constr "Success" unit (fun () -> Success) in
    let failure = constr "Failure" unit (fun () -> Failure []) in
    let variants = [ econstr success; econstr failure ] in
    sum variants (function
      | Success -> case () success
      | Failure _ -> case () failure)
  ;;

  let sexp_v2 =
    let open Conv in
    let success = constr "Success" unit (fun () -> Success) in
    let failure =
      constr "Failure" (list Compound_user_error.sexp) (fun errors -> Failure errors)
    in
    let variants = [ econstr success; econstr failure ] in
    sum variants (function
      | Success -> case () success
      | Failure errors -> case errors failure)
  ;;

  let sexp = sexp_v2
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
      ~resp:Build_outcome_with_diagnostics.sexp_v1
      ~version:1
  ;;

  let v2 =
    Decl.Request.make_current_gen
      ~req:(Conv.list Conv.string)
      ~resp:Build_outcome_with_diagnostics.sexp_v2
      ~version:2
  ;;

  let decl = Decl.Request.make ~method_:"build" ~generations:[ v1; v2 ]
end

module Runtest = struct
  let v1 =
    Decl.Request.make_current_gen
      ~req:(Conv.list Conv.string)
      ~resp:Build_outcome_with_diagnostics.sexp_v2
      ~version:1
  ;;

  let decl = Decl.Request.make ~method_:"runtest" ~generations:[ v1 ]
end

let build = Build.decl
let status = Status.decl
let runtest = Runtest.decl

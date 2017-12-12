open Import
open Sexp.Of_sexp

module Context = struct
  module Opam = struct
    type t =
      { name   : string
      ; switch : string
      ; root   : string option
      ; merlin : bool
      }

    let t =
      record
        (field   "switch" string                 >>= fun switch ->
         field   "name"   string ~default:switch >>= fun name ->
         field_o "root"   string                 >>= fun root ->
         field_b "merlin"                        >>= fun merlin ->
         return { switch
                ; name
                ; root
                ; merlin
                })
  end

  type t = Default | Opam of Opam.t

  let t = function
    | Atom (_, "default") -> Default
    | sexp -> Opam (Opam.t sexp)

  let name = function
    | Default -> "default"
    | Opam o  -> o.name
end

type t =
  { merlin_context : string option
  ; contexts       : Context.t list
  }

let t sexps =
  let merlin_ctx, contexts =
    List.fold_left sexps ~init:(None, []) ~f:(fun (merlin_ctx, ctxs) sexp ->
      let ctx =
        sum
          [ cstr "context" (Context.t @> nil) (fun x -> x) ]
          sexp
      in
      let name = Context.name ctx in
      if name = "" ||
         String.is_prefix name ~prefix:"." ||
         name = "log" ||
         name = "install" ||
         String.contains name '/' ||
         String.contains name '\\' then
        of_sexp_errorf sexp "%S is not allowed as a build context name" name;
      if List.exists ctxs ~f:(fun c -> Context.name c = name) then
        of_sexp_errorf sexp "second definition of build context %S" name;
      match ctx, merlin_ctx with
      | Opam { merlin = true; _ }, Some _ ->
        of_sexp_errorf sexp "you can only have one context for merlin"
      | Opam { merlin = true; _ }, None ->
        (Some name, ctx :: ctxs)
      | _ ->
        (merlin_ctx, ctx :: ctxs))
  in
  let contexts =
    match contexts with
    | [] -> [Context.Default]
    | _  -> contexts
  in
  let merlin_ctx =
    match merlin_ctx with
    | Some _ -> merlin_ctx
    | None ->
      if List.mem Context.Default ~set:contexts then
        Some "default"
      else
        None
  in
  { merlin_context = merlin_ctx
  ; contexts       = List.rev contexts
  }

let load fname = t (Sexp.load ~fname ~mode:Many)

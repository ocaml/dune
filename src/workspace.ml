open Import
open Sexp.Of_sexp

module Context = struct
  module Target = struct
    type t =
      | Native
      | Named of string

    let t sexp =
      match string sexp with
      | "native" -> Native
      | s        -> Named s
  end

  module Opam = struct
    type t =
      { name    : string
      ; switch  : string
      ; root    : string option
      ; merlin  : bool
      ; targets : Target.t list
      }

    let t =
      field   "switch"  string                                    >>= fun switch ->
      field   "name"    string ~default:switch                    >>= fun name ->
      field   "targets" (list Target.t) ~default:[Target.Native]  >>= fun targets ->
      field_o "root"    string                                    >>= fun root ->
      field_b "merlin"                                            >>= fun merlin ->
      return { switch
             ; name
             ; root
             ; merlin
             ; targets
             }
  end

  type t = Default of Target.t list | Opam of Opam.t

  let t = function
    | Atom (_, "default") -> Default [Native]
    | List (_, List _ :: _) as sexp -> Opam (record Opam.t sexp)
    | sexp ->
      sum
        [ cstr_record "default"
            (field "targets" (list Target.t) ~default:[Target.Native]
             >>= fun targets ->
             return (Default targets))
        ; cstr_record "opam"
            (Opam.t >>= fun x -> return (Opam x))
        ]
        sexp

  let name = function
    | Default _ -> "default"
    | Opam    o -> o.name

  let targets = function
    | Default l -> l
    | Opam    o -> o.targets

  let all_names t =
    let n = name t in
    n :: List.filter_map (targets t) ~f:(function
      | Native -> None
      | Named s -> Some (n ^ "." ^ s))
end

type t =
  { merlin_context : string option
  ; contexts       : Context.t list
  }

let t ?x sexps =
  let defined_names = ref String_set.empty in
  let merlin_ctx, contexts =
    List.fold_left sexps ~init:(None, []) ~f:(fun (merlin_ctx, ctxs) sexp ->
      let ctx =
        sum
          [ cstr "context" (Context.t @> nil) (fun x -> x) ]
          sexp
      in
      let ctx =
        match x with
        | None -> ctx
        | Some s ->
          let target = Context.Target.Named s in
          let add_target target targets =
            if List.mem target ~set:targets then
              targets
            else
              targets @ [target]
          in
          match ctx with
          | Default targets -> Default (add_target target targets)
          | Opam o -> Opam { o with targets = add_target target o.targets }
      in
      let name = Context.name ctx in
      if name = "" ||
         String.is_prefix name ~prefix:"." ||
         name = "log" ||
         name = "install" ||
         String.contains name '/' ||
         String.contains name '\\' then
        of_sexp_errorf sexp "%S is not allowed as a build context name" name;
      if String_set.mem name !defined_names then
        of_sexp_errorf sexp "second definition of build context %S" name;
      defined_names := String_set.union !defined_names (String_set.of_list (Context.all_names ctx));
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
    | [] -> [Context.Default [Native]]
    | _  -> contexts
  in
  let merlin_ctx =
    match merlin_ctx with
    | Some _ -> merlin_ctx
    | None ->
      if List.exists contexts ~f:(function Context.Default _ -> true | _ -> false) then
        Some "default"
      else
        None
  in
  { merlin_context = merlin_ctx
  ; contexts       = List.rev contexts
  }

let load ?x fname = t ?x (Sexp.load ~fname ~mode:Many)

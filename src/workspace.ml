open Import
open Sexp.Of_sexp

module Context = struct
  module Target = struct
    module Derived = struct
      type t =
        { name: string
        ; instrumented: Coverage0.Context.t
        }
    end

    type t =
      | Native
      | Named of string
      | Derived of Derived.t

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

  type t =
    | Default of Target.t list
    | Opam of Opam.t

  let name = function
    | Default _ -> "default"
    | Opam    o -> o.name

  module Unresolved = struct
    module Derived = struct
      type t =
        { from: string
        ; name: string
        ; instrumented: Coverage0.Context.t
        }

      let t =
        field "from" string >>= fun from ->
        field "name" string >>= fun name ->
        field "instrumented" Coverage0.Context.t >>= fun instrumented ->
        return
          { from
          ; name
          ; instrumented
          }
    end
    type t =
      | Default of Target.t list
      | Opam of Opam.t
      | Derived of Derived.t

    let name = function
      | Default _ -> "default"
      | Opam o -> o.name
      | Derived d -> d.name

    let targets = function
      | Default ts -> ts
      | Opam o -> o.targets
      | Derived _ -> []

    let all_names t =
      let n = name t in
      n :: List.filter_map (targets t) ~f:(function
        | Native -> None
        | Named s -> Some (n ^ "." ^ s)
        | Derived d -> Some d.name)

    let t = function
      | Atom (_, A "default") -> Default [Native]
      | List (_, List _ :: _) as sexp -> Opam (record Opam.t sexp)
      | sexp ->
        sum
          [ cstr_record "default"
              (field "targets" (list Target.t) ~default:[Target.Native]
               >>= fun targets ->
               return (Default targets))
          ; cstr_record "opam"
              (Opam.t >>= fun x -> return (Opam x))
          ; cstr_record "derived"
              (Derived.t >>= fun x -> return (Derived x))
          ]
          sexp
  end

  let resolve (uns : Unresolved.t list) =
    let resolved, derived =
      List.partition_map uns ~f:(function
        | Unresolved.Opam x -> Left (Opam x)
        | Unresolved.Default x -> Left (Default x)
        | Derived d -> Right d) in
    let resolved =
      resolved
      |> List.map ~f:(fun c -> name c, c)
      |> String_map.of_list
      |> function
      | Ok s -> s
      | Error (_, _, _) -> failwith "TODO" in
    let make_target (d : Unresolved.Derived.t) : Target.t =
      Target.Derived { name = d.name ; instrumented = d.instrumented } in
    List.fold_left ~init:resolved derived
      ~f:(fun r (d : Unresolved.Derived.t) ->
        String_map.update r d.from ~f:(function
          | None ->
            failwith "context derives from non-existent"
          | Some (Default ts) ->
            Some (Default (make_target d :: ts))
          | Some (Opam ts) ->
            Some (Opam { ts with targets = make_target d :: ts.targets }))
      )
    |> String_map.values
end

type t =
  { merlin_context : string option
  ; contexts       : Context.t list
  }

let t ?x sexps =
  let defined_names = ref String_set.empty in
  let merlin_ctx, (contexts : Context.Unresolved.t list) =
    List.fold_left sexps ~init:(None, []) ~f:(fun (merlin_ctx, ctxs) sexp ->
      let ctx =
        sum
          [ cstr "context" (Context.Unresolved.t @> nil) (fun x -> x) ]
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
          | Derived _ as d -> d
      in
      let name = Context.Unresolved.name ctx in
      if name = "" ||
         String.is_prefix name ~prefix:"." ||
         name = "log" ||
         name = "install" ||
         String.contains name '/' ||
         String.contains name '\\' then
        of_sexp_errorf sexp "%S is not allowed as a build context name" name;
      if String_set.mem !defined_names name then
        of_sexp_errorf sexp "second definition of build context %S" name;
      defined_names :=
        String_set.union !defined_names
          (String_set.of_list (Context.Unresolved.all_names ctx));
      match ctx, merlin_ctx with
      | Opam { merlin = true; _ }, Some _ ->
        of_sexp_errorf sexp "you can only have one context for merlin"
      | Opam { merlin = true; _ }, None ->
        (Some name, ctx :: ctxs)
      | _ ->
        (merlin_ctx, ctx :: ctxs))
  in
  let contexts =
    match Context.resolve contexts with
    | [] -> [Context.Default [Native]]
    | contexts  -> contexts
  in
  let merlin_ctx =
    match merlin_ctx with
    | Some _ -> merlin_ctx
    | None ->
      if List.exists contexts
           ~f:(function Context.Default _ -> true | _ -> false) then
        Some "default"
      else
        None
  in
  { merlin_context = merlin_ctx
  ; contexts       = List.rev contexts
  }

let load ?x fname = t ?x (Sexp.load ~fname ~mode:Many)

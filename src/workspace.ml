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
      ; profile : string
      ; switch  : string
      ; root    : string option
      ; merlin  : bool
      ; targets : Target.t list
      }

    let t ~profile =
      field   "switch"  string                                    >>= fun switch ->
      field   "name"    string ~default:switch                    >>= fun name ->
      field   "targets" (list Target.t) ~default:[Target.Native]  >>= fun targets ->
      field_o "root"    string                                    >>= fun root ->
      field_b "merlin"                                            >>= fun merlin ->
      field   "profile" string ~default:profile                   >>= fun profile ->
      return { switch
             ; name
             ; root
             ; merlin
             ; targets
             ; profile
             }
  end

  module Default = struct
    type t =
      { profile : string
      ; targets : Target.t list
      }

    let t ~profile =
      field "targets" (list Target.t) ~default:[Target.Native]
      >>= fun targets ->
      field "profile" string ~default:profile
      >>= fun profile ->
      return { targets; profile }
  end

  type t = Default of Default.t | Opam of Opam.t

  let t ~profile = function
    | Atom (_, A "default") ->
      Default { targets = [Native]
              ; profile
              }
    | List (_, List _ :: _) as sexp -> Opam (record (Opam.t ~profile) sexp)
    | sexp ->
      sum
        [ cstr_record "default"
            (Default.t ~profile >>= fun x -> return (Default x))
        ; cstr_record "opam"
            (Opam.t ~profile >>= fun x -> return (Opam x))
        ]
        sexp

  let name = function
    | Default _ -> "default"
    | Opam    o -> o.name

  let targets = function
    | Default x -> x.targets
    | Opam    x -> x.targets

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

type item = Context of Sexp.Ast.t | Profile of Loc.t * string

let item_of_sexp =
  sum
    [ cstr "context" (raw @> nil) (fun x -> Context x)
    ; cstr_loc "profile" (string @> nil) (fun loc x -> Profile (loc, x))
    ]

let t ?x ?profile:cmdline_profile sexps =
  let defined_names = ref String.Set.empty in
  let profiles, contexts =
    List.partition_map sexps ~f:(fun sexp ->
      match item_of_sexp sexp with
      | Profile (loc, p) -> Left (loc, p)
      | Context c -> Right c)
  in
  let profile =
    match profiles, cmdline_profile with
    | _ :: (loc, _) :: _, _ ->
      Loc.fail loc "profile defined too many times"
    | _, Some p -> p
    | [], None -> "default"
    | [(_, p)], None -> p
  in
  let { merlin_context; contexts } =
    let init =
      { merlin_context = None
      ; contexts       = []
      }
    in
    List.fold_left contexts ~init ~f:(fun t sexp ->
      let ctx = Context.t ~profile sexp in
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
          | Default d ->
            Default { d with targets = add_target target d.targets }
          | Opam o ->
            Opam    { o with targets = add_target target o.targets }
      in
      let name = Context.name ctx in
      if name = "" ||
         String.is_prefix name ~prefix:"." ||
         name = "log" ||
         name = "install" ||
         String.contains name '/' ||
         String.contains name '\\' then
        of_sexp_errorf sexp "%S is not allowed as a build context name" name;
      if String.Set.mem !defined_names name then
        of_sexp_errorf sexp "second definition of build context %S" name;
      defined_names := String.Set.union !defined_names
                         (String.Set.of_list (Context.all_names ctx));
      match ctx, t.merlin_context with
      | Opam { merlin = true; _ }, Some _ ->
        of_sexp_errorf sexp "you can only have one context for merlin"
      | Opam { merlin = true; _ }, None ->
        { merlin_context = Some name; contexts = ctx :: t.contexts }
      | _ ->
        { t with contexts = ctx :: t.contexts })
  in
  let contexts =
    match contexts with
    | [] -> [Context.Default { targets = [Native]; profile }]
    | _  -> contexts
  in
  let merlin_context =
    match merlin_context with
    | Some _ -> merlin_context
    | None ->
      if List.exists contexts
           ~f:(function Context.Default _ -> true | _ -> false) then
        Some "default"
      else
        None
  in
  { merlin_context
  ; contexts = List.rev contexts
  }

let load ?x ?profile p = t ?x ?profile (Io.Sexp.load p ~mode:Many)

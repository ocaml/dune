open Stdune

type ast = Ast.t =
  | Atom of Loc.t * Atom.t
  | Quoted_string of Loc.t * string
  | Template of Template.t
  | List of Loc.t * ast list

type hint =
  { on : string
  ; candidates : string list
  }

module Name = struct
  module T = struct
    type t = string

    let compare a b =
      let alen = String.length a
      and blen = String.length b in
      match Int.compare alen blen with
      | Eq -> String.compare a b
      | ne -> ne

    let to_dyn = Dyn.Encoder.string
  end

  include T
  module Map = Map.Make (T)
end

module Fields = struct
  module Unparsed = struct
    type t =
      { values : Ast.t list
      ; entry : Ast.t
      ; prev : t option (* Previous occurrence of this field *)
      }
  end

  type t =
    { unparsed : Unparsed.t Name.Map.t
    ; known : string list
    }

  let consume name state =
    { unparsed = Name.Map.remove state.unparsed name
    ; known = name :: state.known
    }

  let add_known name state = { state with known = name :: state.known }

  let unparsed_ast { unparsed; _ } =
    let rec loop acc = function
      | [] -> acc
      | x :: xs -> (
        match x.Unparsed.prev with
        | None -> loop (x.entry :: acc) xs
        | Some p -> loop (x.entry :: acc) (p :: xs) )
    in
    loop [] (Name.Map.values unparsed)
    |> List.sort ~compare:(fun a b ->
           Int.compare (Ast.loc a).start.pos_cnum (Ast.loc b).start.pos_cnum)
end

type fields = Fields.t

type values = Ast.t list

(* Arguments are:

   - the location of the whole list - the first atom when parsing a constructor
   or a field - the universal map holding the user context *)
type 'kind context =
  | Values : Loc.t * string option * Univ_map.t -> values context
  | Fields : Loc.t * string option * Univ_map.t -> Fields.t context

type ('a, 'kind) parser = 'kind context -> 'kind -> 'a * 'kind

type 'a t = ('a, values) parser

type 'a fields_parser = ('a, Fields.t) parser

let return x _ctx state = (x, state)

let ( >>= ) t f ctx state =
  let x, state = t ctx state in
  f x ctx state

let ( >>| ) t f ctx state =
  let x, state = t ctx state in
  (f x, state)

let ( >>> ) a b ctx state =
  let (), state = a ctx state in
  b ctx state

let ( let* ) = ( >>= )

let ( let+ ) = ( >>| )

let ( and+ ) a b ctx state =
  let a, state = a ctx state in
  let b, state = b ctx state in
  ((a, b), state)

let map t ~f = t >>| f

let try_ t f ctx state = try t ctx state with exn -> f exn ctx state

let get_user_context : type k. k context -> Univ_map.t = function
  | Values (_, _, uc) -> uc
  | Fields (_, _, uc) -> uc

let get key ctx state = (Univ_map.find (get_user_context ctx) key, state)

let get_all ctx state = (get_user_context ctx, state)

let set : type a b k. a Univ_map.Key.t -> a -> (b, k) parser -> (b, k) parser =
 fun key v t ctx state ->
  match ctx with
  | Values (loc, cstr, uc) ->
    t (Values (loc, cstr, Univ_map.add uc key v)) state
  | Fields (loc, cstr, uc) ->
    t (Fields (loc, cstr, Univ_map.add uc key v)) state

let set_many : type a k. Univ_map.t -> (a, k) parser -> (a, k) parser =
 fun map t ctx state ->
  match ctx with
  | Values (loc, cstr, uc) ->
    t (Values (loc, cstr, Univ_map.superpose uc map)) state
  | Fields (loc, cstr, uc) ->
    t (Fields (loc, cstr, Univ_map.superpose uc map)) state

let loc : type k. k context -> k -> Loc.t * k =
 fun ctx state ->
  match ctx with
  | Values (loc, _, _) -> (loc, state)
  | Fields (loc, _, _) -> (loc, state)

let at_eos : type k. k context -> k -> bool =
 fun ctx state ->
  match ctx with
  | Values _ -> state = []
  | Fields _ -> Name.Map.is_empty state.unparsed

let eos ctx state = (at_eos ctx state, state)

let if_eos ~then_ ~else_ ctx state =
  if at_eos ctx state then
    then_ ctx state
  else
    else_ ctx state

let repeat : 'a t -> 'a list t =
  let rec loop t acc ctx l =
    match l with
    | [] -> (List.rev acc, [])
    | _ ->
      let x, l = t ctx l in
      loop t (x :: acc) ctx l
  in
  fun t ctx state -> loop t [] ctx state

let result : type a k. k context -> a * k -> a =
 fun ctx (v, state) ->
  match ctx with
  | Values (_, cstr, _) -> (
    match state with
    | [] -> v
    | sexp :: _ -> (
      match cstr with
      | None ->
        User_error.raise ~loc:(Ast.loc sexp) [ Pp.text "This value is unused" ]
      | Some s ->
        User_error.raise ~loc:(Ast.loc sexp)
          [ Pp.textf "Too many argument for %s" s ] ) )
  | Fields _ -> (
    match Name.Map.choose state.unparsed with
    | None -> v
    | Some (name, { entry; _ }) ->
      let name_loc =
        match entry with
        | List (_, s :: _) -> Ast.loc s
        | _ -> assert false
      in
      User_error.raise ~loc:name_loc
        ~hints:(User_message.did_you_mean name ~candidates:state.known)
        [ Pp.textf "Unknown field %s" name ] )

let parse t context sexp =
  let ctx = Values (Ast.loc sexp, None, context) in
  result ctx (t ctx [ sexp ])

let capture ctx state =
  let f t = result ctx (t ctx state) in
  (f, [])

let end_of_list (Values (loc, cstr, _)) =
  match cstr with
  | None ->
    let loc = { loc with start = loc.stop } in
    User_error.raise ~loc [ Pp.text "Premature end of list" ]
  | Some s -> User_error.raise ~loc [ Pp.textf "Not enough arguments for %s" s ]
  [@@inline never]

let next f ctx sexps =
  match sexps with
  | [] -> end_of_list ctx
  | sexp :: sexps -> (f sexp, sexps)
  [@@inline always]

let next_with_user_context f ctx sexps =
  match sexps with
  | [] -> end_of_list ctx
  | sexp :: sexps -> (f (get_user_context ctx) sexp, sexps)
  [@@inline always]

let peek _ctx sexps =
  match sexps with
  | [] -> (None, sexps)
  | sexp :: _ -> (Some sexp, sexps)
  [@@inline always]

let peek_exn ctx sexps =
  match sexps with
  | [] -> end_of_list ctx
  | sexp :: _ -> (sexp, sexps)
  [@@inline always]

let junk = next ignore

let junk_everything : type k. (unit, k) parser =
 fun ctx state ->
  match ctx with
  | Values _ -> ((), [])
  | Fields _ -> ((), { state with unparsed = Name.Map.empty })

let keyword kwd =
  next (function
    | Atom (_, s) when Atom.to_string s = kwd -> ()
    | sexp ->
      User_error.raise ~loc:(Ast.loc sexp) [ Pp.textf "'%s' expected" kwd ])

let match_keyword l ~fallback =
  peek
  >>= function
  | Some (Atom (_, A s)) -> (
    match List.assoc l s with
    | Some t -> junk >>> t
    | None -> fallback )
  | _ -> fallback

let until_keyword kwd ~before ~after =
  let rec loop acc =
    peek
    >>= function
    | None -> return (List.rev acc, None)
    | Some (Atom (_, A s)) when s = kwd ->
      junk >>> after >>= fun x -> return (List.rev acc, Some x)
    | _ -> before >>= fun x -> loop (x :: acc)
  in
  loop []

let plain_string f =
  next (function
    | Atom (loc, A s)
    | Quoted_string (loc, s) ->
      f ~loc s
    | Template { loc; _ }
    | List (loc, _) ->
      User_error.raise ~loc [ Pp.text "Atom or quoted string expected" ])

let filename =
  plain_string (fun ~loc s ->
      match s with
      | "."
      | ".." ->
        User_error.raise ~loc
          [ Pp.textf "'.' and '..' are not valid filenames" ]
      | fn -> fn)

let enter t =
  next_with_user_context (fun uc sexp ->
      match sexp with
      | List (loc, l) ->
        let ctx = Values (loc, None, uc) in
        result ctx (t ctx l)
      | sexp -> User_error.raise ~loc:(Ast.loc sexp) [ Pp.text "List expected" ])

let if_list ~then_ ~else_ =
  peek_exn
  >>= function
  | List _ -> then_
  | _ -> else_

let if_paren_colon_form ~then_ ~else_ =
  peek_exn
  >>= function
  | List (_, Atom (loc, A s) :: _) when String.is_prefix s ~prefix:":" ->
    let name = String.drop s 1 in
    enter (junk >>= fun () -> then_ >>| fun f -> f (loc, name))
  | _ -> else_

let fix f =
  let rec p = lazy (f r)
  and r ast = (Lazy.force p) ast in
  r

let loc_between_states : type k. k context -> k -> k -> Loc.t =
 fun ctx state1 state2 ->
  match ctx with
  | Values _ -> (
    match state1 with
    | sexp :: rest when rest == state2 ->
      (* common case *)
      Ast.loc sexp
    | [] ->
      let (Values (loc, _, _)) = ctx in
      { loc with start = loc.stop }
    | sexp :: rest ->
      let loc = Ast.loc sexp in
      let rec search last l =
        if l == state2 then
          { loc with stop = (Ast.loc last).stop }
        else
          match l with
          | [] ->
            let (Values (loc, _, _)) = ctx in
            { (Ast.loc sexp) with stop = loc.stop }
          | sexp :: rest -> search sexp rest
      in
      search sexp rest )
  | Fields _ -> (
    let parsed =
      Name.Map.merge state1.unparsed state2.unparsed
        ~f:(fun _key before after ->
          match (before, after) with
          | Some _, None -> before
          | _ -> None)
    in
    match
      Name.Map.values parsed
      |> List.map ~f:(fun f -> Ast.loc f.Fields.Unparsed.entry)
      |> List.sort ~compare:(fun a b ->
             Int.compare a.Loc.start.pos_cnum b.start.pos_cnum)
    with
    | [] ->
      let (Fields (loc, _, _)) = ctx in
      loc
    | first :: l ->
      let last = List.fold_left l ~init:first ~f:(fun _ x -> x) in
      { first with stop = last.stop } )

let located t ctx state1 =
  let x, state2 = t ctx state1 in
  ((loc_between_states ctx state1 state2, x), state2)

let raw = next Fn.id

let basic desc f =
  next (function
    | Template { loc; _ }
    | List (loc, _)
    | Quoted_string (loc, _) ->
      User_error.raise ~loc [ Pp.textf "%s expected" desc ]
    | Atom (loc, s) -> (
      match f (Atom.to_string s) with
      | Result.Error () -> User_error.raise ~loc [ Pp.textf "%s expected" desc ]
      | Ok x -> x ))

let string = plain_string (fun ~loc:_ x -> x)

let int =
  basic "Integer" (fun s ->
      match int_of_string s with
      | x -> Ok x
      | exception _ -> Result.Error ())

let pair a b = enter (a >>= fun a -> b >>= fun b -> return (a, b))

let triple a b c =
  enter (a >>= fun a -> b >>= fun b -> c >>= fun c -> return (a, b, c))

let option t =
  enter
    ( eos
    >>= function
    | true -> return None
    | false -> t >>| Option.some )

let find_cstr cstrs loc name ctx values =
  match List.assoc cstrs name with
  | Some t -> result ctx (t ctx values)
  | None ->
    User_error.raise ~loc
      ~hints:
        (User_message.did_you_mean name ~candidates:(List.map cstrs ~f:fst))
      [ Pp.textf "Unknown constructor %s" name ]

let sum cstrs =
  next_with_user_context (fun uc sexp ->
      match sexp with
      | Atom (loc, A s) -> find_cstr cstrs loc s (Values (loc, Some s, uc)) []
      | Template { loc; _ }
      | Quoted_string (loc, _) ->
        User_error.raise ~loc [ Pp.text "Atom expected" ]
      | List (loc, []) ->
        User_error.raise ~loc [ Pp.text "Non-empty list expected" ]
      | List (loc, name :: args) -> (
        match name with
        | Quoted_string (loc, _)
        | List (loc, _)
        | Template { loc; _ } ->
          User_error.raise ~loc [ Pp.text "Atom expected" ]
        | Atom (s_loc, A s) ->
          find_cstr cstrs s_loc s (Values (loc, Some s, uc)) args ))

let enum cstrs =
  next (function
    | Quoted_string (loc, _)
    | Template { loc; _ }
    | List (loc, _) ->
      User_error.raise ~loc [ Pp.text "Atom expected" ]
    | Atom (loc, A s) -> (
      match List.assoc cstrs s with
      | Some value -> value
      | None ->
        User_error.raise ~loc
          [ Pp.textf "Unknown value %s" s ]
          ~hints:
            (User_message.did_you_mean s ~candidates:(List.map cstrs ~f:fst)) ))

let bool = enum [ ("true", true); ("false", false) ]

let map_validate t ~f ctx state1 =
  let x, state2 = t ctx state1 in
  match f x with
  | Result.Ok x -> (x, state2)
  | Error (msg : User_message.t) ->
    let msg =
      match msg.loc with
      | Some _ -> msg
      | None -> { msg with loc = Some (loc_between_states ctx state1 state2) }
    in
    raise (User_error.E msg)

(** TODO: Improve consistency of error messages, e.g. use %S consistently for
    field names: see [field_missing] and [field_present_too_many_times]. *)
let field_missing loc name =
  User_error.raise ~loc [ Pp.textf "field %s missing" name ]
  [@@inline never]

let field_present_too_many_times _ name entries =
  match entries with
  | _ :: second :: _ ->
    User_error.raise ~loc:(Ast.loc second)
      [ Pp.textf "Field %S is present too many times" name ]
  | _ -> assert false

let multiple_occurrences ?(on_dup = field_present_too_many_times) uc name last
    =
  let rec collect acc (x : Fields.Unparsed.t) =
    let acc = x.entry :: acc in
    match x.prev with
    | None -> acc
    | Some prev -> collect acc prev
  in
  on_dup uc name (collect [] last)
  [@@inline never]

let find_single ?on_dup uc (state : Fields.t) name =
  let res = Name.Map.find state.unparsed name in
  ( match res with
  | Some ({ prev = Some _; _ } as last) ->
    multiple_occurrences uc name last ?on_dup
  | _ -> () );
  res

let field name ?default ?on_dup t (Fields (loc, _, uc)) state =
  match find_single uc state name ?on_dup with
  | Some { values; entry; _ } ->
    let ctx = Values (Ast.loc entry, Some name, uc) in
    let x = result ctx (t ctx values) in
    (x, Fields.consume name state)
  | None -> (
    match default with
    | Some v -> (v, Fields.add_known name state)
    | None -> field_missing loc name )

let field_o name ?on_dup t (Fields (_, _, uc)) state =
  match find_single uc state name ?on_dup with
  | Some { values; entry; _ } ->
    let ctx = Values (Ast.loc entry, Some name, uc) in
    let x = result ctx (t ctx values) in
    (Some x, Fields.consume name state)
  | None -> (None, Fields.add_known name state)

let field_b_gen field_gen ?check ?on_dup name =
  field_gen name ?on_dup
    (let* () = Option.value check ~default:(return ()) in
     eos
     >>= function
     | true -> return true
     | _ -> bool)

let field_b = field_b_gen (field ~default:false)

let field_o_b = field_b_gen field_o

let multi_field name t (Fields (_, _, uc)) (state : Fields.t) =
  let rec loop acc (field : Fields.Unparsed.t option) =
    match field with
    | None -> acc
    | Some { values; prev; entry } ->
      let ctx = Values (Ast.loc entry, Some name, uc) in
      let x = result ctx (t ctx values) in
      loop (x :: acc) prev
  in
  let res = loop [] (Name.Map.find state.unparsed name) in
  (res, Fields.consume name state)

let fields t (Values (loc, cstr, uc)) sexps =
  let unparsed =
    List.fold_left sexps ~init:Name.Map.empty ~f:(fun acc sexp ->
        match sexp with
        | List (_, name_sexp :: values) -> (
          match name_sexp with
          | Atom (_, A name) ->
            Name.Map.set acc name
              { Fields.Unparsed.values
              ; entry = sexp
              ; prev = Name.Map.find acc name
              }
          | List (loc, _)
          | Quoted_string (loc, _)
          | Template { loc; _ } ->
            User_error.raise ~loc [ Pp.text "Atom expected" ] )
        | _ ->
          User_error.raise ~loc:(Ast.loc sexp)
            [ Pp.text "S-expression of the form (<name> <values>...) expected" ])
  in
  let ctx = Fields (loc, cstr, uc) in
  let x = result ctx (t ctx { Fields.unparsed; known = [] }) in
  (x, [])

let leftover_fields_generic t more_fields (Fields (loc, cstr, uc)) state =
  let x =
    let ctx = Values (loc, cstr, uc) in
    result ctx (repeat t ctx (Fields.unparsed_ast state))
  in
  (x, { Fields.known = state.known @ more_fields; unparsed = Name.Map.empty })

let leftover_fields ctx (state : Fields.t) =
  leftover_fields_generic raw (Name.Map.keys state.unparsed) ctx state

let leftover_fields_as_sums cstrs =
  leftover_fields_generic (sum cstrs) (List.map cstrs ~f:fst)

type kind =
  | Values of Loc.t * string option
  | Fields of Loc.t * string option

let kind : type k. k context -> k -> kind * k =
 fun ctx state ->
  match ctx with
  | Values (loc, cstr, _) -> (Values (loc, cstr), state)
  | Fields (loc, cstr, _) -> (Fields (loc, cstr), state)

let traverse l ~f ctx state =
  Tuple.T2.swap
    (List.fold_map ~init:state l ~f:(fun state x ->
         Tuple.T2.swap (f x ctx state)))

let all = traverse ~f:(fun x -> x)

let fields_missing_need_exactly_one loc names =
  User_error.raise ~loc
    [ Pp.textf "fields %s are all missing (exactly one is needed)"
        (String.concat ~sep:", " names)
    ]
  [@@inline never]

let fields_mutual_exclusion_violation loc names =
  User_error.raise ~loc
    [ Pp.textf "fields %s are mutually exclusive"
        (String.concat ~sep:", " names)
    ]
  [@@inline never]

let fields_mutually_exclusive ?on_dup ?default fields
    ((Fields (loc, _, _) : _ context) as ctx) state =
  let res, state =
    traverse
      ~f:(fun (name, parser) ->
        field_o name ?on_dup parser >>| fun res -> (name, res))
      fields ctx state
  in
  match
    List.filter_map res ~f:(function
      | name, Some x -> Some (name, x)
      | _, None -> None)
  with
  | [] -> (
    let names = List.map fields ~f:fst in
    match default with
    | None -> fields_missing_need_exactly_one loc names
    | Some default -> (default, state) )
  | [ (_name, res) ] -> (res, state)
  | _ :: _ :: _ as results ->
    let names = List.map ~f:fst results in
    fields_mutual_exclusion_violation loc names

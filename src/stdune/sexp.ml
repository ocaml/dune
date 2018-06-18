include Usexp

module type Combinators = sig
  type 'a t
  val unit       : unit                      t
  val string     : string                    t
  val int        : int                       t
  val float      : float                     t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val triple     : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
  val list       : 'a t -> 'a list           t
  val array      : 'a t -> 'a array          t
  val option     : 'a t -> 'a option         t
  val string_set : String.Set.t              t
  val string_map : 'a t -> 'a String.Map.t   t
  val string_hashtbl : 'a t -> (string, 'a) Hashtbl.t t
end

module To_sexp = struct
  type nonrec 'a t = 'a -> t
  let unit () = List []
  let string = Usexp.atom_or_quoted_string
  let int n = Atom (Atom.of_int n)
  let float f = Atom (Atom.of_float f)
  let bool b = Atom (Atom.of_bool b)
  let pair fa fb (a, b) = List [fa a; fb b]
  let triple fa fb fc (a, b, c) = List [fa a; fb b; fc c]
  let list f l = List (List.map l ~f)
  let array f a = list f (Array.to_list a)
  let option f = function
    | None -> List []
    | Some x -> List [f x]
  let string_set set = list atom (String.Set.to_list set)
  let string_map f map = list (pair atom f) (String.Map.to_list map)
  let record l =
    List (List.map l ~f:(fun (n, v) -> List [Atom(Atom.of_string n); v]))
  let string_hashtbl f h =
    string_map f
      (Hashtbl.foldi h ~init:String.Map.empty ~f:(fun key data acc ->
         String.Map.add acc key data))

  type field = string * Usexp.t option

  let field name f ?(equal=(=)) ?default v =
    match default with
    | None -> (name, Some (f v))
    | Some d ->
      if equal d v then
        (name, None)
      else
        (name, Some (f v))
  let field_o name f v = (name, Option.map ~f v)

  let record_fields (l : field list) =
    List (List.filter_map l ~f:(fun (k, v) ->
      Option.map v ~f:(fun v -> List[Atom (Atom.of_string k); v])))

  let unknown _ = unsafe_atom_of_string "<unknown>"
end

module Of_sexp = struct
  type ast = Ast.t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
    | Template of Template.t
    | List of Loc.t * ast list

  type hint =
    { on: string
    ; candidates: string list
    }

  exception Of_sexp of Loc.t * string * hint option

  type 'a t = ast -> 'a

  let located f sexp =
    (Ast.loc sexp, f sexp)

  let of_sexp_error ?hint sexp str = raise (Of_sexp (Ast.loc sexp, str, hint))
  let of_sexp_errorf ?hint sexp fmt = Printf.ksprintf (of_sexp_error ?hint sexp) fmt

  let of_sexp_errorf_loc ?hint loc fmt =
    Printf.ksprintf (fun s -> raise (Of_sexp (loc, s, hint))) fmt

  let raw x = x

  let unit = function
    | List (_, []) -> ()
    | sexp -> of_sexp_error sexp "() expected"

  let string = function
    | Atom (_, A s) -> s
    | Quoted_string (_, s) -> s
    | (List _ | Template _) as sexp ->
      of_sexp_error sexp "Atom or quoted string expected"

  let int sexp = match sexp with
    | Atom (_, A s) -> (try int_of_string s
                        with _ -> of_sexp_error sexp "Integer expected")
    | _ -> of_sexp_error sexp "Integer expected"

  let float sexp = match sexp with
    | Atom (_, A s) -> (try float_of_string s
                        with _ -> of_sexp_error sexp "Float expected")
    | _ -> of_sexp_error sexp "Float expected"

  let bool = function
    | Atom (_, A "true") -> true
    | Atom (_, A "false") -> false
    | sexp -> of_sexp_error sexp "'true' or 'false' expected"

  let pair fa fb = function
    | List (_, [a; b]) -> (fa a, fb b)
    | sexp -> of_sexp_error sexp "S-expression of the form (_ _) expected"

  let triple fa fb fc = function
    | List (_, [a; b; c]) -> (fa a, fb b, fc c)
    | sexp -> of_sexp_error sexp "S-expression of the form (_ _ _) expected"

  let list f = function
    | (Atom _ | Quoted_string _ | Template _) as sexp ->
      of_sexp_error sexp "List expected"
    | List (_, l) -> List.map l ~f

  let array f sexp = Array.of_list (list f sexp)

  let option f = function
    | List (_, []) -> None
    | List (_, [x]) -> Some (f x)
    | sexp -> of_sexp_error sexp "S-expression of the form () or (_) expected"

  let string_set sexp = String.Set.of_list (list string sexp)
  let string_map f sexp =
    match String.Map.of_list (list (pair string f) sexp) with
    | Result.Ok x -> x
    | Error (key, _v1, _v2) ->
      of_sexp_error sexp (Printf.sprintf "key %S present multiple times" key)

  let string_hashtbl f sexp =
    let map = string_map f sexp in
    let tbl = Hashtbl.create (String.Map.cardinal map + 32) in
    String.Map.iteri map ~f:(Hashtbl.add tbl);
    tbl

  type unparsed_field =
    { values : Ast.t list
    ; entry  : Ast.t
    ; prev   : unparsed_field option (* Previous occurrence of this field *)
    }

  module Name = struct
    type t = string
    let compare a b =
      let alen = String.length a and blen = String.length b in
      match Int.compare alen blen with
      | Eq -> String.compare a b
      | ne -> ne
  end

  module Name_map = Map.Make(Name)

  (* Either:

     - [Sum (<list location>, <constructor name>, <elements>)]
     - [Record (<list location>, <unparsed fields>, <known field names>)]
  *)
  type 'kind list_parser_state =
    | Cstr
      : Loc.t * string * Ast.t list -> [`Cstr] list_parser_state
    | Record
      : Loc.t * unparsed_field Name_map.t * string list
      -> [`Record] list_parser_state

  type ('a, 'kind) list_parser
    = 'kind list_parser_state -> 'a * 'kind list_parser_state

  type 'a   cstr_parser = ('a, [`Cstr  ]) list_parser
  type 'a record_parser = ('a, [`Record]) list_parser

  let return x state = (x, state)
  let (>>=) m f state =
    let x, state = m state in
    f x state
  let (>>|) m f state =
    let x, state = m state in
    (f x, state)

  let get_loc : type k. k list_parser_state -> Loc.t = function
    | Cstr   (loc, _, _) -> loc
    | Record (loc, _, _) -> loc

  let list_loc state = (get_loc state, state)

  let consume name (Record (loc, unparsed, known)) =
    Record (loc, Name_map.remove unparsed name, name :: known)

  let add_known name (Record (loc, unparsed, known)) =
    (Record (loc, unparsed, name :: known))

  let map_validate parse ~f state1 =
    let x, state2 = parse state1 in
    match f x with
    | Result.Ok x -> x, state2
    | Error msg ->
      let (Record (_, unparsed1, _)) = state1 in
      let (Record (_, unparsed2, _)) = state2 in
      let parsed =
        Name_map.merge unparsed1 unparsed2 ~f:(fun _key before after ->
          match before, after with
          | Some _, None -> before
          | _ -> None)
      in
      let loc =
        match
          Name_map.values parsed
          |> List.map ~f:(fun f -> Ast.loc f.entry)
          |> List.sort ~compare:(fun a b ->
            Int.compare a.Loc.start.pos_cnum b.start.pos_cnum)
        with
        | [] -> get_loc state1
        | first :: l ->
          let last = List.fold_left l ~init:first ~f:(fun _ x -> x) in
          { first with stop = last.stop }
      in
      of_sexp_errorf_loc loc "%s" msg

  module Short_syntax = struct
    type 'a t =
      | Not_allowed
      | This    of 'a
      | Located of (Loc.t -> 'a)

    let parse t entry name =
      match t with
      | Not_allowed -> of_sexp_errorf entry "field %s needs a value" name
      | This    x -> x
      | Located f -> f (Ast.loc entry)
  end

  let too_many_values name field =
    of_sexp_errorf_loc (Ast.loc field.entry) "too many values for field %s" name

  let field_missing state name =
    of_sexp_errorf_loc (get_loc state) "field %s missing" name

  let rec multiple_occurrences ~name ~last ~prev =
    match prev.prev with
    | Some prev_prev ->
      (* Make the error message point to the second occurrence *)
      multiple_occurrences ~name ~last:prev ~prev:prev_prev
    | None ->
      of_sexp_errorf last.entry "Field %S is present too many times" name
  [@@inline never]

  let find_single (Record (_, unparsed, _)) name =
    let res = Name_map.find unparsed name in
    (match res with
     | Some ({ prev = Some prev; _ } as last) ->
       multiple_occurrences ~name ~last ~prev
     | _ -> ());
    res

  let field name ?(short=Short_syntax.Not_allowed)
        ?default value_of_sexp state =
    match find_single state name with
    | Some { values = [value]; _ } ->
      (value_of_sexp value, consume name state)
    | Some { values = []; entry; _ } ->
      (Short_syntax.parse short entry name,
       consume name state)
    | Some f ->
      too_many_values name f
    | None ->
      match default with
      | Some v -> (v, add_known name state)
      | None -> field_missing state name

  let field_o name ?(short=Short_syntax.Not_allowed) value_of_sexp state =
    match find_single state name with
    | Some { values = [value]; _ } ->
      (Some (value_of_sexp value), consume name state)
    | Some { values = []; entry; _ } ->
      (Some (Short_syntax.parse short entry name),
       consume name state)
    | Some f ->
      too_many_values name f
    | None -> (None, add_known name state)

  let field_b name = field name bool ~default:false ~short:(This true)

  let dup_field name ?(short=Short_syntax.Not_allowed) value_of_sexp state =
    let rec loop acc field =
      match field with
      | None -> acc
      | Some { values = [value]; prev; _ } ->
        loop (value_of_sexp value :: acc) prev
      | Some { values = []; entry; prev } ->
        loop (Short_syntax.parse short entry name :: acc) prev
      | Some f ->
        too_many_values name f
    in
    let (Record (_, unparsed, _)) = state in
    let res = loop [] (Name_map.find unparsed name) in
    (res, consume name state)

  let parse_fields m loc sexps =
    let unparsed =
      List.fold_left sexps ~init:Name_map.empty ~f:(fun acc sexp ->
        match sexp with
        | List (_, name_sexp :: values) -> begin
            match name_sexp with
            | Atom (_, A name) ->
              Name_map.add acc name
                { values
                ; entry = sexp
                ; prev  = Name_map.find acc name
                }
            | List _ | Quoted_string _ | Template _ ->
              of_sexp_error name_sexp "Atom expected"
          end
        | _ ->
          of_sexp_error sexp
            "S-expression of the form (<name> <values>...) expected")
    in
    let v, (Record (_, unparsed, known)) = m (Record (loc, unparsed, [])) in
    match Name_map.choose unparsed with
    | None -> v
    | Some (name, { entry; _ }) ->
      let name_sexp =
        match entry with
        | List (_, s :: _) -> s
        | _ -> assert false
      in
      of_sexp_errorf ~hint:({ on = name ; candidates = known})
        name_sexp "Unknown field %s" name

  let record m sexp =
    match sexp with
    | Atom _ | Quoted_string _ | Template _ ->
      of_sexp_error sexp "List expected"
    | List (loc, sexps) -> parse_fields m loc sexps

  let next t (Cstr (loc, cstr, sexps)) =
    match sexps with
    | [] -> of_sexp_errorf_loc loc "Not enough arguments for %s" cstr
    | sexp :: sexps ->
      let v = t sexp in
      (v, Cstr (loc, cstr, sexps))

  let rest t (Cstr (loc, cstr, sexps)) =
    (List.map sexps ~f:t,
     Cstr (loc, cstr, []))

  let rest_as_record m (Cstr (loc, cstr, sexps)) =
    let v = parse_fields m loc sexps in
    (v, Cstr (loc, cstr, []))

  let sum_result (v, Cstr (loc, cstr, sexps)) =
    match sexps with
    | [] -> v
    | _ :: _ -> of_sexp_errorf_loc loc "Too many arguments for %s" cstr

  let find_cstr cstrs loc name state =
    match List.assoc cstrs name with
    | Some m -> sum_result (m state)
    | None ->
      of_sexp_errorf_loc loc
        ~hint:{ on         = name
              ; candidates = List.map cstrs ~f:fst
              }
        "Unknown constructor %s" name

  let sum cstrs sexp =
    match sexp with
    | Atom (loc, A s) ->
      find_cstr cstrs loc s (Cstr (loc, s, []))
    | Template _
    | Quoted_string _ -> of_sexp_error sexp "Atom expected"
    | List (_, []) -> of_sexp_error sexp "non-empty list expected"
    | List (loc, name :: args) ->
      match name with
      | Quoted_string _ | List _ | Template _ ->
        of_sexp_error name "Atom expected"
      | Atom (s_loc, A s) ->
        find_cstr cstrs s_loc s (Cstr (loc, s, args))

  let field_multi name ?default m state =
    match find_single state name with
    | Some { values; entry; _ } ->
      (sum_result (m (Cstr (Ast.loc entry, name, values))),
       consume name state)
    | None ->
      match default with
      | Some v -> (v, add_known name state)
      | None -> field_missing state name

  let dup_field_multi name m state =
    let rec loop acc field =
      match field with
      | None -> acc
      | Some { values; entry; prev } ->
        let x = sum_result (m (Cstr (Ast.loc entry, name, values))) in
        loop (x :: acc) prev
    in
    let (Record (_, unparsed, _)) = state in
    let res = loop [] (Name_map.find unparsed name) in
    (res, consume name state)

  let enum cstrs sexp =
    match sexp with
    | Template _ | Quoted_string _ | List _ ->
      of_sexp_error sexp "Atom expected"
    | Atom (_, A s) ->
      match List.assoc cstrs s with
      | Some value -> value
      | None ->
        of_sexp_errorf sexp
          ~hint:{ on         = s
                ; candidates = List.map cstrs ~f:fst
                }
          "Unknown value %s" s
end

module type Sexpable = sig
  type t
  val t : t Of_sexp.t
  val sexp_of_t : t To_sexp.t
end

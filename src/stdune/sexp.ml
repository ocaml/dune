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
end

module Of_sexp = struct
  type ast = Ast.t =
    | Atom of Loc.t * Atom.t
    | Quoted_string of Loc.t * string
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

  let of_sexp_errorf_loc loc fmt =
    Printf.ksprintf (fun s -> raise (Of_sexp (loc, s, None))) fmt

  let raw x = x

  let unit = function
    | List (_, []) -> ()
    | sexp -> of_sexp_error sexp "() expected"

  let string = function
    | Atom (_, A s) -> s
    | Quoted_string (_, s) -> s
    | List _ as sexp -> of_sexp_error sexp "Atom or quoted string expected"

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
    | (Atom _ | Quoted_string _) as sexp -> of_sexp_error sexp "List expected"
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

  type record_parser_state =
    { loc      : Loc.t
    ; unparsed : unparsed_field Name_map.t
    ; known    : string list
    }

  type 'a record_parser = record_parser_state -> 'a * record_parser_state

  let return x state = (x, state)
  let (>>=) m f state =
    let x, state = m state in
    f x state

  let record_loc state =
    (state.loc, state)

  let consume name state =
    { state with
      unparsed = Name_map.remove state.unparsed name
    ; known    = name :: state.known
    }

  let add_known name state =
    { state with known = name :: state.known }

  let ignore_fields names state =
    let unparsed =
      List.fold_left names ~init:state.unparsed ~f:(fun acc name ->
        Name_map.remove acc name)
    in
    ((),
     { state with
       unparsed
     ; known = List.rev_append names state.known
     })

  let map_validate parse ~f state =
    let x, state' = parse state in
    match f x with
    | Result.Ok x -> x, state'
    | Error msg ->
      let parsed =
        Name_map.merge state.unparsed state'.unparsed ~f:(fun _key before after ->
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
        | [] -> state.loc
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
    of_sexp_errorf_loc state.loc "field %s missing" name

  let rec multiple_occurrences ~name ~last ~prev =
    match prev.prev with
    | Some prev_prev ->
      (* Make the error message point to the second occurrence *)
      multiple_occurrences ~name ~last:prev ~prev:prev_prev
    | None ->
      of_sexp_errorf last.entry "Field %S is present too many times" name
  [@@inline never]

  let find_single state name =
    let res = Name_map.find state.unparsed name in
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
    let res = loop [] (Name_map.find state.unparsed name) in
    (res, consume name state)

  let make_record_parser_state sexp =
    match sexp with
    | Atom _ | Quoted_string _ -> of_sexp_error sexp "List expected"
    | List (loc, sexps) ->
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
              | List _ | Quoted_string _ ->
                of_sexp_error name_sexp "Atom expected"
            end
          | _ ->
            of_sexp_error sexp
              "S-expression of the form (<name> <values>...) expected")
      in
      { loc    = loc
      ; known  = []
      ; unparsed
      }

  let record parse sexp =
    let state = make_record_parser_state sexp in
    let v, state = parse state in
    match Name_map.choose state.unparsed with
    | None -> v
    | Some (name, { entry; _ }) ->
      let name_sexp =
        match entry with
        | List (_, s :: _) -> s
        | _ -> assert false
      in
      of_sexp_errorf ~hint:({ on = name ; candidates = state.known})
        name_sexp "Unknown field %s" name

  type ('a, 'b) rest =
    | No_rest : ('a, 'a) rest
    | Many    : 'a t -> ('a list -> 'b, 'b) rest

  module Constructor_args_spec = struct
    type 'a conv = 'a t
    type ('a, 'b) t =
      | Nil  : ('a, 'a) t
      | Cons : 'a conv * ('b, 'c) t -> ('a -> 'b, 'c) t

    let rec convert : type a b c. (a, b) t -> (b, c) rest -> Ast.t -> Ast.t list -> a -> c
      = fun t rest sexp sexps f ->
        match t, rest, sexps with
        | Nil, No_rest, [] -> f
        | Nil, Many _ , [] -> f []
        | Cons _, _, [] -> of_sexp_error sexp "not enough arguments"
        | Nil, No_rest, _ :: _ -> of_sexp_error sexp "too many arguments"
        | Nil, Many conv, l -> f (List.map l ~f:conv)
        | Cons (conv, t), _, s :: sexps ->
          convert t rest sexp sexps (f (conv s))
  end

  let nil = Constructor_args_spec.Nil
  let ( @> ) a b = Constructor_args_spec.Cons (a, b)

  let field_multi name ?default args_spec f state =
    match find_single state name with
    | Some { values; entry; _ } ->
      (Constructor_args_spec.convert args_spec No_rest entry values f,
       consume name state)
    | None ->
      match default with
      | Some v -> (v, add_known name state)
      | None -> field_missing state name

  let dup_field_multi name args_spec f state =
    let rec loop acc field =
      match field with
      | None -> acc
      | Some { values; entry; prev } ->
        let x =
          Constructor_args_spec.convert args_spec No_rest entry values f
        in
        loop (x :: acc) prev
    in
    let res = loop [] (Name_map.find state.unparsed name) in
    (res, consume name state)

  module Constructor_spec = struct
    type ('a, 'b, 'c) tuple =
      { name : string
      ; args : ('a, 'b) Constructor_args_spec.t
      ; rest : ('b, 'c) rest
      ; make : Loc.t -> 'a
      }

    type 'a record =
      { name  : string
      ; parse : 'a record_parser
      }

    type 'a t =
      | Tuple  : (_, _, 'a) tuple -> 'a t
      | Record : 'a record        -> 'a t

    let name = function
      | Tuple  x -> x.name
      | Record x -> x.name
  end
  module C = Constructor_spec

  let cstr_loc name args make =
    C.Tuple { name; args; make; rest = No_rest }
  let cstr_rest_loc name args rest make =
    C.Tuple { name; args; make; rest = Many rest }

  let cstr_record name parse =
    C.Record { name; parse }

  let cstr name args make =
    cstr_loc name args (fun _ -> make)

  let cstr_rest name args rest make =
    cstr_rest_loc name args rest (fun _ -> make)

  let equal_cstr_name a b = Name.compare a b = Eq

  let find_cstr cstrs sexp name =
    match
      List.find cstrs ~f:(fun cstr ->
        equal_cstr_name (C.name cstr) name)
    with
    | Some cstr -> cstr
    | None ->
      of_sexp_errorf sexp
        ~hint:{ on = String.uncapitalize name
              ; candidates = List.map cstrs ~f:(fun c ->
                  String.uncapitalize (C.name c))
              }
        "Unknown constructor %s" name

  let sum cstrs sexp =
    match sexp with
    | Atom (loc, A s) -> begin
        match find_cstr cstrs sexp s with
        | C.Tuple  t -> Constructor_args_spec.convert t.args t.rest sexp [] (t.make loc)
        | C.Record _ -> of_sexp_error sexp "'%s' expect arguments"
      end
    | Quoted_string _ -> of_sexp_error sexp "Atom expected"
    | List (_, []) -> of_sexp_error sexp "non-empty list expected"
    | List (loc, name_sexp :: args) ->
      match name_sexp with
      | Quoted_string _ | List _ -> of_sexp_error name_sexp "Atom expected"
      | Atom (_, A s) ->
        match find_cstr cstrs sexp s with
        | C.Tuple  t -> Constructor_args_spec.convert t.args t.rest sexp args (t.make loc)
        | C.Record r -> record r.parse (List (loc, args))

  let enum cstrs sexp =
    match sexp with
    | Quoted_string _ | List _ -> of_sexp_error sexp "Atom expected"
    | Atom (_, A s) ->
      match
        List.find cstrs ~f:(fun (name, _) ->
          equal_cstr_name name s)
      with
      | Some (_, value) -> value
      | None ->
        of_sexp_errorf sexp
          ~hint:{ on = String.uncapitalize s
                ; candidates =List.map cstrs ~f:(fun (name, _) ->
                    String.uncapitalize name) }
          "Unknown value %s" s
end

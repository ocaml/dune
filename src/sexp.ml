open Import

type t =
  | Atom of string
  | List of t list

type sexp = t

let must_escape str =
  let len = String.length str in
  len = 0 ||
  let rec loop ix =
    match str.[ix] with
    | '"' | '(' | ')' | ';' | '\\' -> true
    | '|' -> ix > 0 && let next = ix - 1 in str.[next] = '#' || loop next
    | '#' -> ix > 0 && let next = ix - 1 in str.[next] = '|' || loop next
    | '\000' .. '\032' | '\127' .. '\255' -> true
    | _ -> ix > 0 && loop (ix - 1)
  in
  loop (len - 1)

let rec to_string = function
  | Atom s -> if must_escape s then sprintf "%S" s else s
  | List l -> sprintf "(%s)" (List.map l ~f:to_string |> String.concat ~sep:" ")

let code_error message vars =
  code_errorf "%s"
    (to_string
       (List (Atom message
              :: List.map vars ~f:(fun (name, value) ->
                List [Atom name; value]))))


module Ast = struct
  type t =
    | Atom of Loc.t * string
    | List of Loc.t * t list

  let loc = function
    | Atom (loc, _) -> loc
    | List (loc, _) -> loc

  let rec remove_locs : t -> sexp = function
    | Atom (_, s) -> Atom s
    | List (_, l) -> List (List.map l ~f:remove_locs)

  let to_string t = to_string (remove_locs t)
end

let rec add_loc t ~loc : Ast.t =
  match t with
  | Atom s -> Atom (loc, s)
  | List l -> List (loc, List.map l ~f:(add_loc ~loc))

module type Combinators = sig
  type 'a t
  val unit       : unit                      t
  val string     : string                    t
  val int        : int                       t
  val bool       : bool                      t
  val pair       : 'a t -> 'b t -> ('a * 'b) t
  val list       : 'a t -> 'a list           t
  val option     : 'a t -> 'a option         t
  val string_set : String_set.t              t
  val string_map : 'a t -> 'a String_map.t   t
end

module To_sexp = struct
  type nonrec 'a t = 'a -> t
  let unit () = List []
  let string s = Atom s
  let int n = Atom (string_of_int n)
  let bool b = Atom (string_of_bool b)
  let pair fa fb (a, b) = List [fa a; fb b]
  let list f l = List (List.map l ~f)
  let option f = function
    | None -> List []
    | Some x -> List [f x]
  let string_set set = list string (String_set.elements set)
  let string_map f map = list (pair string f) (String_map.bindings map)
end

module Of_sexp = struct
  type ast = Ast.t =
    | Atom of Loc.t * string
    | List of Loc.t * ast list

  type 'a t = ast -> 'a

  let of_sexp_error sexp str = raise (Loc.Error (Ast.loc sexp, str))
  let of_sexp_errorf sexp fmt = ksprintf (of_sexp_error sexp) fmt

  let unit = function
    | List (_, []) -> ()
    | sexp -> of_sexp_error sexp "() expected"

  let string = function
    | Atom (_, s) -> s
    | List _ as sexp -> of_sexp_error sexp "Atom expected"

  let int sexp =
    let s = string sexp in
    try
      int_of_string s
    with _ ->
      of_sexp_error sexp "Integer expected"

  let bool sexp =
    match string sexp with
    | "true" -> true
    | "false" -> false
    | _ -> of_sexp_error sexp "'true' or 'false' expected"

  let pair fa fb = function
    | List (_, [a; b]) -> (fa a, fb b)
    | sexp -> of_sexp_error sexp "S-expression of the form (_ _) expected"

  let list f = function
    | Atom _ as sexp -> of_sexp_error sexp "List expected"
    | List (_, l) -> List.map l ~f

  let option f = function
    | List (_, []) -> None
    | List (_, [x]) -> Some (f x)
    | sexp -> of_sexp_error sexp "S-expression of the form () or (_) expected"

  let string_set sexp = String_set.of_list (list string sexp)
  let string_map f sexp =
    match String_map.of_alist (list (pair string f) sexp) with
    | Ok x -> x
    | Error (key, _v1, _v2) ->
      of_sexp_error sexp (sprintf "key %S present multiple times" key)

  type unparsed_field =
    { value : Ast.t option
    ; entry : Ast.t
    }

  module Name = struct
    type t = string
    let compare a b =
      let alen = String.length a and blen = String.length b in
      if alen < blen then
        -1
      else if alen > blen then
        1
      else
        String.compare a b
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

  let consume name state =
    { state with
      unparsed = Name_map.remove name state.unparsed
    ; known    = name :: state.known
    }

  let add_known name state =
    { state with known = name :: state.known }

  let ignore_fields names state =
    let unparsed =
      List.fold_left names ~init:state.unparsed ~f:(fun acc name ->
        Name_map.remove name acc)
    in
    ((),
     { state with
       unparsed
     ; known = List.rev_append names state.known
     })

  let field name ?default value_of_sexp state =
    match Name_map.find name state.unparsed with
    | Some { value = Some value; _ } ->
      (value_of_sexp value, consume name state)
    | Some { value = None; _ } ->
      Loc.fail state.loc "field %s needs a value" name
    | None ->
      match default with
      | Some v -> (v, add_known name state)
      | None ->
        Loc.fail state.loc "field %s missing" name

  let field_o name value_of_sexp state =
    match Name_map.find name state.unparsed with
    | Some { value = Some value; _ } ->
      (Some (value_of_sexp value), consume name state)
    | Some { value = None; _ } ->
      Loc.fail state.loc "field %s needs a value" name
    | None -> (None, add_known name state)

  let field_b name state =
    match Name_map.find name state.unparsed with
    | Some { value = Some value; _ } ->
      (bool value, consume name state)
    | Some { value = None; _ } ->
      (true, consume name state)
    | None ->
      (false, add_known name state)

  let make_record_parser_state sexp =
    match sexp with
    | Atom _ -> of_sexp_error sexp "List expected"
    | List (loc, sexps) ->
      let unparsed =
        List.fold_left sexps ~init:Name_map.empty ~f:(fun acc sexp ->
          match sexp with
          | List (_, [Atom (_, name)]) ->
            Name_map.add acc ~key:name ~data:{ value = None; entry = sexp }
          | List (_, [name_sexp; value]) -> begin
              match name_sexp with
              | Atom (_, name) ->
                Name_map.add acc ~key:name ~data:{ value = Some value; entry = sexp }
              | List _ ->
                of_sexp_error name_sexp "Atom expected"
            end
          | _ ->
            of_sexp_error sexp "S-expression of the form (_ _) expected")
      in
      { loc    = loc
      ; known  = []
      ; unparsed
      }

  let record parse sexp =
    let state = make_record_parser_state sexp in
    let v, state = parse state in
    if Name_map.is_empty state.unparsed then
      v
    else
      let name, { entry; _ } = Name_map.choose state.unparsed in
      let name_sexp =
        match entry with
        | List (_, s :: _) -> s
        | _ -> assert false
      in
      of_sexp_errorf name_sexp
        "Unknown field %s%s" name (hint name state.known)

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

  module Constructor_spec = struct
    type ('a, 'b, 'c) unpacked =
      { name : string
      ; args : ('a, 'b) Constructor_args_spec.t
      ; rest : ('b, 'c) rest
      ; make : 'a
      }

    type 'a t = T : (_, _, 'a) unpacked -> 'a t
  end

  let cstr_rest name args rest make =
    Constructor_spec.T { name; args; make; rest = Many rest }

  let cstr name args make =
    Constructor_spec.T { name; args; make; rest = No_rest }

  let equal_cstr_name a b = Name.compare a b = 0

  let find_cstr cstrs sexp name =
    match
      List.find cstrs ~f:(fun (Constructor_spec.T cstr) ->
        equal_cstr_name cstr.name name)
    with
    | Some cstr -> cstr
    | None ->
      of_sexp_errorf sexp
        "Unknown constructor %s%s" name
        (hint
           (String.uncapitalize_ascii name)
           (List.map cstrs ~f:(fun (Constructor_spec.T c) ->
              String.uncapitalize_ascii c.name)))

  let sum cstrs sexp =
    match sexp with
    | Atom (_, s) -> begin
        let (Constructor_spec.T c) = find_cstr cstrs sexp s in
        Constructor_args_spec.convert c.args c.rest sexp [] c.make
      end
    | List (_, []) -> of_sexp_error sexp "non-empty list expected"
    | List (_, name_sexp :: args) ->
      match name_sexp with
      | List _ -> of_sexp_error name_sexp "Atom expected"
      | Atom (_, s) ->
        let (Constructor_spec.T c) = find_cstr cstrs sexp s in
        Constructor_args_spec.convert c.args c.rest sexp args c.make

  let enum cstrs sexp =
    match sexp with
    | List _ -> of_sexp_error sexp "Atom expected"
    | Atom (_, s) ->
      match
        List.find cstrs ~f:(fun (name, _) ->
          equal_cstr_name name s)
      with
      | Some (_, value) -> value
      | None ->
        of_sexp_errorf sexp
          "Unknown value %s%s" s
          (hint
             (String.uncapitalize_ascii s)
             (List.map cstrs ~f:(fun (name, _) ->
                String.uncapitalize_ascii name)))
end

open Import

type t =
  | Atom of string
  | List of t list

type sexp = t

module Locs = struct
  type t =
    | Atom of Loc.t
    | List of Loc.t * t list

  let loc = function
    | Atom loc -> loc
    | List (loc, _) -> loc
end

let locate_in_list ts ~sub ~locs =
  let rec loop ts locs =
    match ts, locs with
    | [], _ -> None
    | _, [] -> assert false
    | t::ts, loc::locs ->
      if t == sub then
        Some (Locs.loc loc)
      else
        match t, loc with
        | Atom _, _ -> loop ts locs
        | List inner_ts, List (_, inner_locs) -> begin
            match loop inner_ts inner_locs with
            | None -> loop ts locs
            | Some _ as res -> res
          end
        | _ -> assert false
  in
  loop ts locs

let locate t ~sub ~locs =
  locate_in_list [t] ~sub ~locs:[locs]

exception Of_sexp_error of t * string

let of_sexp_error t msg = raise (Of_sexp_error (t, msg))
let of_sexp_errorf t fmt = Printf.ksprintf (of_sexp_error t) fmt

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
  type nonrec 'a t = t -> 'a

  let unit = function
    | List [] -> ()
    | sexp -> of_sexp_error sexp "() expected"

  let string = function
    | Atom s -> s
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
    | List [a; b] -> (fa a, fb b)
    | sexp -> of_sexp_error sexp "S-expression of the form (_ _) expected"

  let list f = function
    | Atom _ as sexp -> of_sexp_error sexp "List expected"
    | List l -> List.map l ~f

  let option f = function
    | List [] -> None
    | List [x] -> Some (f x)
    | sexp -> of_sexp_error sexp "S-expression of the form () or (_) expected"

  let string_set sexp = String_set.of_list (list string sexp)
  let string_map f sexp =
    match String_map.of_alist (list (pair string f) sexp) with
    | Ok x -> x
    | Error (key, _v1, _v2) ->
      of_sexp_error sexp (sprintf "key %S present multiple times" key)

  type unparsed_field =
    { value : sexp option
    ; entry : sexp
    }

  module Name_map = Map.Make(struct
      type t = string
      let compare a b =
        let alen = String.length a and blen = String.length b in
        if alen < blen then
          -1
        else if alen > blen then
          1
        else
          String.compare a b
    end)

  type record_parser_state =
    { record   : sexp
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
    | Some { value = Some value } ->
      (value_of_sexp value, consume name state)
    | Some { value = None } ->
      of_sexp_error  state.record (Printf.sprintf "field %s needs a value" name)
    | None ->
      match default with
      | Some v -> (v, add_known name state)
      | None ->
        of_sexp_error state.record (Printf.sprintf "field %s missing" name)

  let field_o name value_of_sexp state =
    match Name_map.find name state.unparsed with
    | Some { value = Some value } ->
      (Some (value_of_sexp value), consume name state)
    | Some { value = None } ->
      of_sexp_error state.record (Printf.sprintf "field %s needs a value" name)
    | None -> (None, add_known name state)

  let field_b name state =
    match Name_map.find name state.unparsed with
    | Some { value = Some value } ->
      (bool value, consume name state)
    | Some { value = None } ->
      (true, consume name state)
    | None ->
      (false, add_known name state)

  let make_record_parser_state sexp =
    match sexp with
    | Atom _ -> of_sexp_error sexp "List expected"
    | List sexps ->
      let unparsed =
        List.fold_left sexps ~init:Name_map.empty ~f:(fun acc sexp ->
          match sexp with
          | List [Atom name] ->
            Name_map.add acc ~key:name ~data:{ value = None; entry = sexp }
          | List [name_sexp; value] -> begin
              match name_sexp with
              | Atom name ->
                Name_map.add acc ~key:name ~data:{ value = Some value; entry = sexp }
              | List _ ->
                of_sexp_error name_sexp "Atom expected"
            end
          | _ ->
            of_sexp_error sexp "S-expression of the form (_ _) expected")
      in
      { record = sexp
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
        | List (s :: _) -> s
        | _ -> assert false
      in
      of_sexp_errorf name_sexp
        "Unknown field %s%s" name (hint name state.known)

  module Constructor_args_spec = struct
    type 'a conv = 'a t
    type ('a, 'b) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a conv * ('b, 'c) t -> ('a -> 'b, 'c) t

    let rec convert : type a b. (a, b) t -> sexp -> sexp list -> a -> b
      = fun t sexp sexps f ->
        match t, sexps with
        | [], [] -> f
        | _ :: _, [] -> of_sexp_error sexp "not enough arguments"
        | [], _ :: _ -> of_sexp_error sexp "too many arguments"
        | conv :: t, s :: sexps ->
          convert t sexp sexps (f (conv s))
  end

  module Constructor_spec = struct
    type 'a t =
        T : { name : string
            ; args : ('a, 'b) Constructor_args_spec.t
            ; make : 'a
            } -> 'b t

    let name (T t) = t.name
  end

  let cstr name args make =
    Constructor_spec.T { name; args; make }

  let equal_cstr_name a b =
    let alen = String.length a and blen = String.length b in
    if alen <> blen then
      false
    else if alen = 0 then
      true
    else
      let is_cap s =
        match s.[0] with
        | 'A'..'Z' -> true
        | _        -> false
      in
      match is_cap a, is_cap b with
      | true, true | false, false ->
        a = b
      | true, false ->
        a = String.capitalize_ascii b
      | false, true ->
        String.capitalize_ascii a = b

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
    | Atom s -> begin
        let (Constructor_spec.T c) = find_cstr cstrs sexp s in
        Constructor_args_spec.convert c.args sexp [] c.make
      end
    | List [] -> of_sexp_error sexp "non-empty list expected"
    | List (name_sexp :: args) ->
      match name_sexp with
      | List _ -> of_sexp_error name_sexp "Atom expected"
      | Atom s ->
        let (Constructor_spec.T c) = find_cstr cstrs sexp s in
        Constructor_args_spec.convert c.args sexp args c.make

  let enum cstrs sexp =
    match sexp with
    | List _ -> of_sexp_error sexp "Atom expected"
    | Atom s ->
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
(*
module Both = struct
  type sexp = t
  type 'a t =
    { of_sexp : sexp -> 'a
    ; to_sexp : 'a -> sexp
    }

  module A = Of_sexp
  module B = To_Sexp

  let string = { of_sexp = A.string; to_sexp = B.string }
  let int = { of_sexp = A.int; to_sexp = B.int }
  let pair a b = { of_sexp = A.pair a.of_sexp b.of_sexp
                 ; to_sexp =
  let list f l = List (List.map l ~f)
  let string_set set = list string (String_set.elements set)
  let string_map f map = list (pair string f) (String_map.bindings map)
end
  functor (C : Sexp.Combinators) -> struct
    open C
   let t = string int int *)

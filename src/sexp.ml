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

exception Of_sexp_error of string * t

let of_sexp_error msg t = raise (Of_sexp_error (msg, t))

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
    | sexp -> of_sexp_error "() expected" sexp

  let string = function
    | Atom s -> s
    | List _ as sexp -> of_sexp_error "Atom expected" sexp

  let int sexp =
    let s = string sexp in
    try
      int_of_string s
    with _ ->
      of_sexp_error "Integer expected" sexp

  let bool sexp =
    match string sexp with
    | "true" -> true
    | "false" -> false
    | _ -> of_sexp_error "'true' or 'false' expected" sexp

  let pair fa fb = function
    | List [a; b] -> (fa a, fb b)
    | sexp -> of_sexp_error "S-expression of the form (_ _) expected" sexp

  let list f = function
    | Atom _ as sexp -> of_sexp_error "List expected" sexp
    | List l -> List.map l ~f

  let option f = function
    | List [] -> None
    | List [x] -> Some (f x)
    | sexp -> of_sexp_error "S-expression of the form () or (_) expected" sexp

  let string_set sexp = String_set.of_list (list string sexp)
  let string_map f sexp =
    match String_map.of_alist (list (pair string f) sexp) with
    | Ok x -> x
    | Error (key, _v1, _v2) ->
      of_sexp_error (sprintf "key %S present multiple times" key) sexp

  module Field_spec = struct
    type 'a kind =
      | Field   : (sexp -> 'a) * 'a option -> 'a kind
      | Field_o : (sexp -> 'a) -> 'a option kind

    type 'a t =
      { name : string
      ; kind : 'a kind
      }

    let field name ?default of_sexp = { name; kind = Field (of_sexp, default) }
    let field_o name of_sexp = { name; kind = Field_o of_sexp }
  end

  let field   = Field_spec.field
  let field_o = Field_spec.field_o

  module Fields_spec = struct
    type ('a, 'b) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a Field_spec.t * ('b, 'c) t -> ('a -> 'b, 'c) t

    let rec names : type a b. (a, b) t -> string list = function
      | [] -> []
      | { name; _ } :: t -> name :: names t
  end

  let compare_names a b =
    let alen = String.length a and blen = String.length b in
    if alen < blen then
      -1
    else if alen > blen then
      1
    else
      String.compare a b

  let binary_search =
    let rec loop entries name a b =
      if a >= b then
        None
      else
        let c = (a + b) lsr 1 in
        let name', position = entries.(c) in
        let d = compare_names name name' in
        if d < 0 then
          loop entries name a c
        else if d > 0 then
          loop entries name (c + 1) b
        else
          Some position
    in
    fun entries name -> loop entries name 0 (Array.length entries)

  let parse_field field_names field_values sexp =
    match sexp with
    | List [name_sexp; value_sexp] -> begin
        match name_sexp with
        | List _ -> of_sexp_error "Atom expected" name_sexp
        | Atom name ->
          match binary_search field_names name with
          | Some (-1) -> () (* ignored field *)
          | Some n -> field_values.(n) <- value_sexp
          | None -> of_sexp_error (Printf.sprintf "Unknown field %s" name) name_sexp
      end
    | _ ->
      of_sexp_error "S-expression of the form (_ _) expected" sexp

  let rec parse_fields field_names field_values sexps =
    match sexps with
    | [] -> ()
    | sexp :: sexps ->
      parse_field field_names field_values sexp;
      parse_fields field_names field_values sexps

  (* S-expression different from all others in the program, to act as a None value *)
  let none_sexp = Atom Sys.executable_name

  let parse_field_value : type a. sexp -> a Field_spec.t -> sexp -> a =
    fun full_sexp spec sexp ->
      let open Field_spec in
      let { name; kind } = spec in
      match kind, (sexp == none_sexp) with
      | Field (_, None), true ->
        of_sexp_error (Printf.sprintf "field %s missing" name) full_sexp
      | Field (_, Some default), true -> default
      | Field (f, _), _ -> f sexp
      | Field_o _, true -> None
      | Field_o f, false -> Some (f sexp)

  let rec parse_field_values
    : type a b. sexp -> (a, b) Fields_spec.t -> a -> sexp array -> int -> b =
    fun full_sexp spec k values n ->
      let open Fields_spec in
      match spec with
      | [] -> k
      | field_spec :: spec ->
        let v = parse_field_value full_sexp field_spec values.(n) in
        parse_field_values full_sexp spec (k v) values (n + 1)

  let record ?(ignore=[]) spec =
    let names =
      Fields_spec.names spec
      |> List.mapi ~f:(fun i name -> (name, i))
      |> List.rev_append (List.rev_map ignore ~f:(fun n -> (n, -1)))
      |> List.sort ~cmp:(fun (a, _) (b, _) -> compare_names a b)
      |> Array.of_list
    in
    fun record_of_fields sexp ->
      match sexp with
      | Atom _ -> of_sexp_error "List expected" sexp
      | List sexps ->
        let field_values = Array.make (Array.length names) none_sexp in
        parse_fields names field_values sexps;
        parse_field_values sexp spec record_of_fields field_values 0

  module Constructor_args_spec = struct
    type 'a conv = 'a t
    type ('a, 'b) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a conv * ('b, 'c) t -> ('a -> 'b, 'c) t

    let rec convert : type a b. (a, b) t -> sexp -> sexp list -> a -> b
      = fun t sexp sexps f ->
        match t, sexps with
        | [], [] -> f
        | _ :: _, [] -> of_sexp_error "not enough arguments" sexp
        | [], _ :: _ -> of_sexp_error "too many arguments" sexp
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

  let find_cstr names sexp s =
    match binary_search names s with
    | Some cstr -> cstr
    | None -> of_sexp_error (sprintf "Unknown constructor %s" s) sexp

  let sum cstrs =
    let names =
      List.concat_map cstrs ~f:(fun cstr ->
        let name = Constructor_spec.name cstr in
        [ String.capitalize_ascii   name, cstr
        ; String.uncapitalize_ascii name, cstr
        ])
      |> List.sort ~cmp:(fun (a, _) (b, _) -> compare_names a b)
      |> Array.of_list
    in
    fun sexp ->
      match sexp with
      | Atom s -> begin
          let (Constructor_spec.T c) = find_cstr names sexp s in
          Constructor_args_spec.convert c.args sexp [] c.make
        end
      | List [] -> of_sexp_error "non-empty list expected" sexp
      | List (name_sexp :: args) ->
        match name_sexp with
        | List _ -> of_sexp_error "Atom expected" name_sexp
        | Atom s ->
          let (Constructor_spec.T c) = find_cstr names sexp s in
          Constructor_args_spec.convert c.args sexp args c.make
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

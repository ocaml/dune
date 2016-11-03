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

  let rec sub_exn t ~path =
    match path with
    | [] -> t
    | x :: path ->
      match t with
      | Atom _ -> failwith "Sexp.Locs.sub_exn"
      | List (_, l) ->
        match List.nth l x with
        | t -> sub_exn t ~path
        | exception _ -> failwith "Sexp.Locs.sub_exn"
end

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

module To_sexp = struct
  type nonrec 'a t = 'a -> t
  let string s = Atom s
  let int n = Atom (string_of_int n)
  let pair fa fb (a, b) = List [fa a; fb b]
  let list f l = List (List.map l ~f)
  let string_set set = list string (String_set.elements set)
end

module Of_sexp = struct
  type nonrec 'a t = t -> 'a

  let string = function
    | Atom s -> s
    | List _ as sexp -> of_sexp_error "Atom expected" sexp

  let int sexp =
    let s = string sexp in
    try
      int_of_string s
    with _ ->
      of_sexp_error "Integer expected" sexp

  let pair fa fb = function
    | List [a; b] -> (fa a, fb b)
    | sexp -> of_sexp_error "S-expression of the form (_ _) expected" sexp

  let list f = function
    | Atom _ as sexp -> of_sexp_error "List expected" sexp
    | List l -> List.map l ~f

  let string_set sexp = String_set.of_list (list string sexp)

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
    let rec loop entries sexp name a b =
      if a >= b then
        of_sexp_error (Printf.sprintf "Unknown field %s" name) sexp
      else
        let c = (a + b) lsr 1 in
        let name', position = entries.(c) in
        let d = compare_names name name' in
        if d < 0 then
          loop entries sexp name a c
        else if d > 0 then
          loop entries sexp name (c + 1) b
        else
          position
    in
    fun entries sexp name -> loop entries sexp name 0 (Array.length entries)

  let parse_field field_names field_values sexp =
    match sexp with
    | List [name_sexp; value_sexp] -> begin
        match name_sexp with
        | List _ -> of_sexp_error "Atom expected" name_sexp
        | Atom name ->
          let n =
            binary_search field_names name_sexp name
          in
          field_values.(n) <- value_sexp
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

  let record spec record_of_fields =
    let names =
      Fields_spec.names spec
      |> List.mapi ~f:(fun i name -> (name, i))
      |> List.sort ~cmp:(fun (a, _) (b, _) -> compare_names a b)
      |> Array.of_list
    in
    fun sexp ->
      match sexp with
      | Atom _ -> of_sexp_error "List expected" sexp
      | List sexps ->
        let field_values = Array.make (Array.length names) none_sexp in
        parse_fields names field_values sexps;
        parse_field_values sexp spec record_of_fields field_values 0
end

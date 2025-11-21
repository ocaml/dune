open Stdune
open Ast

let[@inline] loc_compare_start_pos_cnum a b =
  let a = Loc.start_pos_cnum a in
  let b = Loc.start_pos_cnum b in
  Int.compare a b
;;

module Name = struct
  module T = struct
    type t = string

    let compare a b =
      let open Ordering.O in
      let= () = Int.compare (String.length a) (String.length b) in
      String.compare a b
    ;;

    let to_dyn = Dyn.string
  end

  include T
  module Map = Map.Make (T)
end

module Fields : sig
  module Unparsed : sig
    type t = private
      { values : Ast.t list
      ; entry : Ast.t
      ; prev : t option (* Previous occurrence of this field *)
      }
  end

  type t = private
    { unparsed : Unparsed.t Name.Map.t
    ; known : string list
    }

  val of_values : Ast.t list -> t
  val add_known : t -> string -> t
  val consume : t -> string -> t
  val unparsed_ast : t -> Ast.t list
  val junk_unparsed : t -> t
  val leftover_fields : t -> string list -> t
end = struct
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

  let leftover_fields t fields = { unparsed = Name.Map.empty; known = t.known @ fields }
  let junk_unparsed t = { t with unparsed = Name.Map.empty }

  let of_values sexps =
    let unparsed =
      List.fold_left sexps ~init:Name.Map.empty ~f:(fun acc sexp ->
        match sexp with
        | List (_, name_sexp :: values) ->
          (match name_sexp with
           | Atom (_, A name) ->
             Name.Map.set
               acc
               name
               { Unparsed.values; entry = sexp; prev = Name.Map.find acc name }
           | List (loc, _) | Quoted_string (loc, _) | Template { loc; _ } ->
             User_error.raise ~loc [ Pp.text "Atom expected" ])
        | _ ->
          User_error.raise
            ~loc:(Ast.loc sexp)
            [ Pp.text "S-expression of the form (<name> <values>...) expected" ])
    in
    { unparsed; known = [] }
  ;;

  let consume state name =
    { unparsed = Name.Map.remove state.unparsed name; known = name :: state.known }
  ;;

  let add_known state name = { state with known = name :: state.known }

  let unparsed_ast =
    let rec loop acc = function
      | [] -> acc
      | (x : Unparsed.t) :: xs ->
        loop
          (x.entry :: acc)
          (match x.prev with
           | None -> xs
           | Some p -> p :: xs)
    in
    fun { unparsed; _ } ->
      loop [] (Name.Map.values unparsed)
      |> List.sort ~compare:(fun a b ->
        loc_compare_start_pos_cnum (Ast.loc a) (Ast.loc b))
  ;;
end

type fields = Fields.t
type values = Ast.t list

(* Arguments are:

   - the location of the whole list - the first atom when parsing a constructor
     or a field - the universal map holding the user context *)
type 'kind context =
  | Values : Loc.t * string option * Univ_map.t -> values context
  | Fields : Loc.t * string option * Univ_map.t -> Fields.t context

module Kind = struct
  type t =
    | Values of Loc.t * string option
    | Fields of Loc.t * string option
end

type (_, _) parser =
  | Return : 'a -> ('a, 'kind) parser
  | Bind : ('a, 'kind) parser * ('a -> ('b, 'kind) parser) -> ('b, 'kind) parser
  | Map : ('a, 'kind) parser * ('a -> 'b) -> ('b, 'kind) parser
  | Map_validate :
      ('a, 'b) parser * ('a -> ('c, User_message.t) result)
      -> ('c, 'b) parser
  | Seq : (unit, 'kind) parser * ('a, 'kind) parser -> ('a, 'kind) parser
  | And : ('a, 'kind) parser * ('b, 'kind) parser -> ('a * 'b, 'kind) parser
  | Lazy : ('a, 'kind) parser Lazy.t -> ('a, 'kind) parser
  | Try : ('a, 'b) parser * (exn -> ('a, 'b) parser) -> ('a, 'b) parser
  | Get : 'a Univ_map.Key.t -> ('a option, 'b) parser
  | Get_all : (Univ_map.t, 'b) parser
  | Set : 'a Univ_map.Key.t * 'a * ('b, 'k) parser -> ('b, 'k) parser
  | Update_var :
      'a Univ_map.Key.t * ('a option -> 'a option) * ('b, 'k) parser
      -> ('b, 'k) parser
  | Set_many : Univ_map.t * ('a, 'k) parser -> ('a, 'k) parser
  | Loc : (Loc.t, 'k) parser
  | Eos : (bool, 'k) parser
  | Repeat : 'a t -> ('a list, values) parser
  | Capture : (('a, 'b list) parser -> 'a, 'b list) parser
  | Next : (Ast.t -> 'a) -> ('a, values) parser
  | Next_with_user_context : (Univ_map.t -> Ast.t -> 'a) -> ('a, values) parser
  | Peek : (Ast.t option, values) parser
  | Peek_exn : (Ast.t, values) parser
  | Junk_everything : (unit, 'k) parser
  | Keyword : string -> (unit, values) parser
  | Either : ('a, 'b) parser * ('c, 'b) parser -> (('a, 'c) either, 'b) parser
  | Located : ('a, 'b) parser -> (Loc.t * 'a, 'b) parser
  | Sum :
      { force_parens : bool
      ; cstrs : (string * ('a, values) parser) list
      }
      -> ('a, values) parser
  | Enum : (string * ('a, values) parser) list -> ('a, values) parser
  | Field :
      { name : string
      ; default : 'a option
      ; on_dup : (Univ_map.t -> string -> Ast.t list -> unit) option
      ; t : 'a t
      }
      -> ('a, fields) parser
  | Field_opt :
      { name : string
      ; on_dup : (Univ_map.t -> string -> Ast.t list -> unit) option
      ; t : 'a t
      }
      -> ('a option, fields) parser
  | Multi_field :
      { name : string
      ; t : 'a t
      }
      -> ('a list, fields) parser
  | Fields_parser : ('a, fields) parser -> ('a, values) parser
  | Leftover_fields_generic :
      { t : 'a t
      ; names : string list
      }
      -> ('a list, fields) parser
  | Leftover_fields : (Ast.t list, Fields.t) parser
  | Traverse : 'a list * ('a -> ('b, 'c) parser) -> ('b list, 'c) parser
  | Kind : (Kind.t, 'a) parser
  | Fields_mutually_exclusive :
      { on_dup : (Univ_map.t -> string -> Ast.t list -> unit) option
      ; default : 'a option
      ; fields : (string * 'a t) list
      }
      -> ('a, fields) parser
  | Fix : (('a, 'b) parser -> ('a, 'b) parser) -> ('a, 'b) parser

and 'a t = ('a, values) parser

type 'a fields_parser = ('a, Fields.t) parser

let get_user_context : type k. k context -> Univ_map.t = function
  | Values (_, _, uc) -> uc
  | Fields (_, _, uc) -> uc
;;

let result : type a k. k context -> a * k -> a =
  fun ctx (v, state) ->
  match ctx with
  | Values (_, cstr, _) ->
    (match state with
     | [] -> v
     | sexp :: _ ->
       (match cstr with
        | None -> User_error.raise ~loc:(Ast.loc sexp) [ Pp.text "This value is unused" ]
        | Some s ->
          User_error.raise ~loc:(Ast.loc sexp) [ Pp.textf "Too many arguments for %S" s ]))
  | Fields _ ->
    (match Name.Map.choose state.unparsed with
     | None -> v
     | Some (name, { entry; _ }) ->
       let name_loc =
         match entry with
         | List (_, s :: _) -> Ast.loc s
         | _ -> assert false
       in
       User_error.raise
         ~loc:name_loc
         ~hints:(User_message.did_you_mean name ~candidates:state.known)
         [ Pp.textf "Unknown field %S" name ])
;;

let return x = Return x
let ( >>= ) t f = Bind (t, f)
let ( >>| ) t f = Map (t, f)
let ( >>> ) a b = Seq (a, b)
let ( let* ) = ( >>= )
let ( let+ ) = ( >>| )
let ( and+ ) a b = And (a, b)
let map t ~f = t >>| f
let try_ t f = Try (t, f)
let get key = Get key
let get_all = Get_all
let set k v t = Set (k, v, t)
let update_var k ~f t = Update_var (k, f, t)
let set_many m t = Set_many (m, t)
let loc = Loc
let eos = Eos
let repeat t = Repeat t
let capture = Capture
let field_o name ?on_dup t = Field_opt { name; on_dup; t }
let fields t = Fields_parser t
let traverse l ~f = Traverse (l, f)

let end_of_list : type a. values context -> a =
  fun (Values (loc, cstr, _)) ->
  match cstr with
  | None ->
    let loc = Loc.set_start_to_stop loc in
    User_error.raise ~loc [ Pp.text "Premature end of list" ]
  | Some s -> User_error.raise ~loc [ Pp.textf "Not enough arguments for %S" s ]
;;

let loc_between_states : type k. k context -> k -> k -> Loc.t =
  let rec search ctx state2 loc last l =
    if l == state2
    then Loc.set_stop loc (Ast.loc last |> Loc.stop)
    else (
      match l with
      | [] ->
        let (Values (loc', _, _)) = ctx in
        Loc.set_stop loc (Loc.stop loc')
      | sexp :: rest -> search ctx state2 loc sexp rest)
  in
  fun ctx state1 state2 ->
    match ctx with
    | Values _ ->
      (match state1 with
       | sexp :: rest when rest == state2 ->
         (* common case *)
         Ast.loc sexp
       | [] ->
         let (Values (loc, _, _)) = ctx in
         Loc.set_start_to_stop loc
       | sexp :: rest ->
         let loc = Ast.loc sexp in
         search ctx state2 loc sexp rest)
    | Fields _ ->
      let parsed =
        Name.Map.merge state1.unparsed state2.unparsed ~f:(fun _key before after ->
          match before, after with
          | Some _, None -> before
          | _ -> None)
      in
      (match
         Name.Map.to_list_map parsed ~f:(fun _ f -> Ast.loc f.entry)
         |> List.sort ~compare:loc_compare_start_pos_cnum
       with
       | [] ->
         let (Fields (loc, _, _)) = ctx in
         loc
       | first :: l ->
         let last = List.fold_left l ~init:first ~f:(fun _ x -> x) in
         Loc.set_stop first (Loc.stop last))
;;

(* Polymorphic record to allow polymorphic recursion with GADTs *)
type eval_fn = { f : 'a 'k. ('a, 'k) parser -> 'k context -> 'k -> 'a * 'k }

let field_missing ?hints loc name =
  User_error.raise ~loc ?hints [ Pp.textf "Field %S is missing" name ]
[@@inline never] [@@specialise never] [@@local never]
;;

let field_present_too_many_times _ name entries =
  match entries with
  | _ :: second :: _ ->
    User_error.raise
      ~loc:(Ast.loc second)
      [ Pp.textf "Field %S is present too many times" name ]
  | _ -> assert false
;;

let multiple_occurrences =
  let rec collect acc (x : Fields.Unparsed.t) =
    let acc = x.entry :: acc in
    match x.prev with
    | None -> acc
    | Some prev -> collect acc prev
  in
  fun ?(on_dup = field_present_too_many_times) uc name last ->
    on_dup uc name (collect [] last)
;;

let find_single ?on_dup uc (state : Fields.t) name =
  let res = Name.Map.find state.unparsed name in
  (match res with
   | Some ({ prev = Some _; _ } as last) -> multiple_occurrences uc name last ?on_dup
   | _ -> ());
  res
;;

let fields_missing_need_exactly_one loc names =
  User_error.raise
    ~loc
    [ Pp.textf
        "fields %s are all missing (exactly one is needed)"
        (String.concat ~sep:", " names)
    ]
[@@inline never] [@@specialise never] [@@local never]
;;

let fields_mutual_exclusion_violation loc names =
  let names = List.map names ~f:String.quoted in
  User_error.raise
    ~loc
    [ Pp.textf "fields %s are mutually exclusive." (String.enumerate_and names) ]
[@@inline never] [@@specialise never] [@@local never]
;;

let rec eval_rec : eval_fn = { f = eval }

and eval : type a k. (a, k) parser -> k context -> k -> a * k =
  fun desc ctx state ->
  match desc with
  | Return x -> x, state
  | Bind (p, f) ->
    let x, state = eval p ctx state in
    eval (f x) ctx state
  | Map (p, f) ->
    let x, state = eval p ctx state in
    f x, state
  | Seq (p1, p2) ->
    let (), state = eval p1 ctx state in
    eval p2 ctx state
  | And (p1, p2) ->
    let x, state = eval p1 ctx state in
    let y, state = eval p2 ctx state in
    (x, y), state
  | Try (t, f) ->
    (try eval t ctx state with
     | exn -> eval (f exn) ctx state)
  | Get key -> Univ_map.find (get_user_context ctx) key, state
  | Get_all -> get_user_context ctx, state
  | Set (key, v, t) ->
    (match ctx with
     | Values (loc, cstr, uc) -> eval t (Values (loc, cstr, Univ_map.set uc key v)) state
     | Fields (loc, cstr, uc) -> eval t (Fields (loc, cstr, Univ_map.set uc key v)) state)
  | Update_var (key, f, t) ->
    (match ctx with
     | Values (loc, cstr, uc) ->
       eval t (Values (loc, cstr, Univ_map.update uc key ~f)) state
     | Fields (loc, cstr, uc) ->
       eval t (Fields (loc, cstr, Univ_map.update uc key ~f)) state)
  | Set_many (map, t) ->
    (match ctx with
     | Values (loc, cstr, uc) ->
       eval t (Values (loc, cstr, Univ_map.superpose map uc)) state
     | Fields (loc, cstr, uc) ->
       eval t (Fields (loc, cstr, Univ_map.superpose map uc)) state)
  | Loc ->
    (match ctx with
     | Values (loc, _, _) -> loc, state
     | Fields (loc, _, _) -> loc, state)
  | Eos ->
    (match ctx with
     | Values _ -> state = [], state
     | Fields _ -> Name.Map.is_empty state.unparsed, state)
  | Repeat t -> eval_repeat t [] ctx state
  | Capture ->
    let f t = result ctx (eval t ctx state) in
    f, []
  | Next f ->
    (match state with
     | [] -> end_of_list ctx
     | sexp :: sexps -> f sexp, sexps)
  | Next_with_user_context f ->
    (match state with
     | [] -> end_of_list ctx
     | sexp :: sexps -> f (get_user_context ctx) sexp, sexps)
  | Peek ->
    (match state with
     | [] -> None, state
     | sexp :: _ -> Some sexp, state)
  | Peek_exn ->
    (match state with
     | [] -> end_of_list ctx
     | sexp :: _ -> sexp, state)
  | Junk_everything ->
    (match ctx with
     | Values _ -> (), []
     | Fields _ -> (), Fields.junk_unparsed state)
  | Keyword kwd ->
    eval
      (Next
         (function
           | Atom (_, A s) when s = kwd -> ()
           | sexp -> User_error.raise ~loc:(Ast.loc sexp) [ Pp.textf "'%s' expected" kwd ]))
      ctx
      state
  | Either (a, b) -> eval_either a b ctx state
  | Located t ->
    let x, state2 = eval t ctx state in
    (loc_between_states ctx state state2, x), state2
  | Sum { force_parens; cstrs } ->
    eval
      (Next_with_user_context
         (fun uc sexp ->
           match sexp with
           | Atom (loc, A s) when not force_parens ->
             find_cstr cstrs loc s (Values (loc, Some s, uc)) []
           | Atom (loc, _) | Template { loc; _ } | Quoted_string (loc, _) | List (loc, [])
             ->
             User_error.raise
               ~loc
               [ Pp.textf
                   "S-expression of the form %s expected"
                   (if force_parens then "(<atom> ...)" else "(<atom> ...) or <atom>")
               ]
           | List (loc, name :: args) ->
             (match name with
              | Quoted_string (loc, _) | List (loc, _) | Template { loc; _ } ->
                User_error.raise ~loc [ Pp.text "Atom expected" ]
              | Atom (s_loc, A s) ->
                find_cstr cstrs s_loc s (Values (loc, Some s, uc)) args)))
      ctx
      state
  | Enum cstrs ->
    eval
      (Next_with_user_context
         (fun uc sexp ->
           match sexp with
           | Quoted_string (loc, _) | Template { loc; _ } | List (loc, _) ->
             User_error.raise ~loc [ Pp.text "Atom expected" ]
           | Atom (loc, A s) ->
             (match List.assoc cstrs s with
              | Some k ->
                let ctx = Values (loc, Some s, uc) in
                result ctx (eval k ctx [])
              | None ->
                User_error.raise
                  ~loc
                  [ Pp.textf "Unknown value %s" s ]
                  ~hints:(User_message.did_you_mean s ~candidates:(List.map cstrs ~f:fst)))))
      ctx
      state
  | Field { name; default; on_dup; t } ->
    let (Fields (loc, _, uc)) = ctx in
    (match find_single uc state name ?on_dup with
     | Some { values; entry; _ } ->
       let ctx = Values (Ast.loc entry, Some name, uc) in
       let x = result ctx (eval t ctx values) in
       x, Fields.consume state name
     | None ->
       (match default with
        | Some v -> v, Fields.add_known state name
        | None -> field_missing loc name))
  | Field_opt { name; on_dup; t } ->
    let (Fields (_, _, uc)) = ctx in
    (match find_single uc state name ?on_dup with
     | Some { values; entry; _ } ->
       let ctx = Values (Ast.loc entry, Some name, uc) in
       let x = result ctx (eval t ctx values) in
       Some x, Fields.consume state name
     | None -> None, Fields.add_known state name)
  | Multi_field { name; t } -> eval_multi_field name t ctx state
  | Fields_parser t ->
    let (Values (loc, cstr, uc)) = ctx in
    let ctx = Fields (loc, cstr, uc) in
    let x = result ctx (eval t ctx (Fields.of_values state)) in
    x, []
  | Leftover_fields_generic { names = more_fields; t } ->
    let (Fields (loc, cstr, uc)) = ctx in
    let x =
      let ctx = Values (loc, cstr, uc) in
      result ctx (eval (Repeat t) ctx (Fields.unparsed_ast state))
    in
    x, Fields.leftover_fields state more_fields
  | Traverse (l, f) ->
    Tuple.T2.swap
      (List.fold_map ~init:state l ~f:(fun state x ->
         Tuple.T2.swap (eval (f x) ctx state)))
  | Map_validate (t, f) ->
    let x, state2 = eval t ctx state in
    (match f x with
     | Result.Ok x -> x, state2
     | Error (msg : User_message.t) ->
       let msg =
         match msg.loc with
         | Some _ -> msg
         | None -> { msg with loc = Some (loc_between_states ctx state state2) }
       in
       raise (User_error.E msg))
  | Kind ->
    (match ctx with
     | Values (loc, cstr, _) -> Kind.Values (loc, cstr), state
     | Fields (loc, cstr, _) -> Fields (loc, cstr), state)
  | Leftover_fields ->
    eval
      (Leftover_fields_generic { t = Next Fun.id; names = Name.Map.keys state.unparsed })
      ctx
      state
  | Fields_mutually_exclusive { on_dup; default; fields } ->
    let (Fields (loc, _, _) : _ context) = ctx in
    let res, state =
      eval
        (traverse fields ~f:(fun (name, parser) ->
           field_o name ?on_dup parser >>| fun res -> name, res))
        ctx
        state
    in
    (match
       List.filter_map res ~f:(function
         | name, Some x -> Some (name, x)
         | _, None -> None)
     with
     | [] ->
       let names = List.map fields ~f:fst in
       (match default with
        | None -> fields_missing_need_exactly_one loc names
        | Some default -> default, state)
     | [ (_name, res) ] -> res, state
     | _ :: _ :: _ as results ->
       let names = List.map ~f:fst results in
       fields_mutual_exclusion_violation loc names)
  | Lazy p -> eval (Lazy.force p) ctx state
  | Fix f ->
    let rec p = lazy (f r)
    and r = Lazy p in
    eval r ctx state

and eval_repeat : type a. a t -> a list -> values context -> values -> a list * values =
  fun t acc ctx state ->
  match state with
  | [] -> List.rev acc, []
  | _ ->
    let x, state = eval_rec.f t ctx state in
    eval_repeat t (x :: acc) ctx state

(* Before you read this code, close your eyes and internalise the fact that
   this code is temporary. It is a temporary state as part of a larger work to
   turn [Decoder.t] into a pure applicative. Once this is done, this function
   will be implemented in a better way and with a much cleaner semantic. *)
and eval_either
  : type a b k. (a, k) parser -> (b, k) parser -> k context -> k -> (a, b) either * k
  =
  let approximate_how_much_input_a_failing_branch_consumed (exn : Exn_with_backtrace.t) =
    Printexc.raw_backtrace_length exn.backtrace
  in
  let compare_input_consumed exn1 exn2 =
    Int.compare
      (approximate_how_much_input_a_failing_branch_consumed exn1)
      (approximate_how_much_input_a_failing_branch_consumed exn2)
  in
  fun a b ctx state ->
    try eval_rec.f (Map (a, Either.left)) ctx state with
    | exn_a ->
      let exn_a = Exn_with_backtrace.capture exn_a in
      (try eval_rec.f (Map (b, Either.right)) ctx state with
       | exn_b ->
         let exn_b = Exn_with_backtrace.capture exn_b in
         Exn_with_backtrace.reraise
           (match compare_input_consumed exn_a exn_b with
            | Gt -> exn_a
            | Eq | Lt -> exn_b))

and find_cstr
  : type a. (string * a t) list -> Loc.t -> string -> values context -> values -> a
  =
  fun cstrs loc name ctx values ->
  match List.assoc cstrs name with
  | Some t -> result ctx (eval_rec.f t ctx values)
  | None ->
    User_error.raise
      ~loc
      ~hints:(User_message.did_you_mean name ~candidates:(List.map cstrs ~f:fst))
      [ Pp.textf "Unknown constructor %s" name ]

and eval_multi_field
  : type a. string -> (a, values) parser -> fields context -> fields -> a list * fields
  =
  let rec loop t uc name acc (field : Fields.Unparsed.t option) =
    match field with
    | None -> acc
    | Some { values; prev; entry } ->
      let ctx = Values (Ast.loc entry, Some name, uc) in
      let x = result ctx (eval t ctx values) in
      loop t uc name (x :: acc) prev
  in
  fun name t (Fields (_, _, uc)) (state : Fields.t) ->
    let res = loop t uc name [] (Name.Map.find state.unparsed name) in
    res, Fields.consume state name
;;

let repeat1 p =
  let+ x = p
  and+ xs = repeat p in
  x :: xs
;;

let parse t context sexp =
  let ctx = Values (Ast.loc sexp, None, context) in
  result ctx (eval t ctx [ sexp ])
;;

let lazy_ t =
  let+ f = capture in
  lazy (f t)
;;

let next f = Next f
let next_with_user_context f = Next_with_user_context f
let peek = Peek
let peek_exn = Peek_exn
let junk = next ignore
let junk_everything = Junk_everything
let keyword kwd = Keyword kwd

let atom_matching f ~desc =
  next (fun sexp ->
    match
      match sexp with
      | Atom (_, A s) -> f s
      | _ -> None
    with
    | Some x -> x
    | None -> User_error.raise ~loc:(Ast.loc sexp) [ Pp.textf "%s expected" desc ])
;;

let until_keyword =
  let rec loop kwd before after acc =
    peek
    >>= function
    | None -> return (List.rev acc, None)
    | Some (Atom (_, A s)) when s = kwd ->
      let+ x = junk >>> after in
      List.rev acc, Some x
    | _ ->
      let* x = before in
      loop kwd before after (x :: acc)
  in
  fun kwd ~before ~after -> loop kwd before after []
;;

let plain_string f =
  next (function
    | Atom (loc, A s) | Quoted_string (loc, s) -> f ~loc s
    | Template { loc; _ } | List (loc, _) ->
      User_error.raise ~loc [ Pp.text "Atom or quoted string expected" ])
;;

let filename =
  plain_string (fun ~loc s ->
    match s with
    | "." | ".." ->
      User_error.raise ~loc [ Pp.textf "'.' and '..' are not valid filenames" ]
    | fn -> fn)
;;

let extension =
  plain_string (fun ~loc s ->
    if String.is_prefix ~prefix:"." s
    then User_error.raise ~loc [ Pp.textf "extension must not start with '.'" ];
    "." ^ s)
;;

let relative_file =
  plain_string (fun ~loc fn ->
    if Filename.is_relative fn
    then fn
    else User_error.raise ~loc [ Pp.textf "relative filename expected" ])
;;

let enter t =
  next_with_user_context (fun uc sexp ->
    match sexp with
    | List (loc, l) ->
      let ctx = Values (loc, None, uc) in
      result ctx (eval t ctx l)
    | sexp -> User_error.raise ~loc:(Ast.loc sexp) [ Pp.text "List expected" ])
;;

let either l r = Either (l, r)

let ( <|> ) x y =
  either x y
  >>| function
  | Left x -> x
  | Right x -> x
;;

let fix f = Fix f
let located t = Located t
let raw = next Fun.id

let basic_loc desc f =
  next (function
    | Template { loc; _ } | List (loc, _) | Quoted_string (loc, _) ->
      User_error.raise ~loc [ Pp.textf "%s expected" desc ]
    | Atom (loc, s) ->
      (match f ~loc (Atom.to_string s) with
       | None -> User_error.raise ~loc [ Pp.textf "%s expected" desc ]
       | Some x -> x))
;;

let basic desc f = basic_loc desc (fun ~loc:_ -> f)
let string = plain_string (fun ~loc:_ x -> x)
let int = basic "Integer" Int.of_string
let float = basic "Float" Float.of_string
let pair a b = enter (a >>= fun a -> b >>= fun b -> return (a, b))
let triple a b c = enter (a >>= fun a -> b >>= fun b -> c >>= fun c -> return (a, b, c))

let unit_number_generic ~of_string ~mul name suffixes =
  let unit_number_of_string ~loc s =
    let possible_suffixes () =
      (* We take the first suffix in the list to be the suggestion *)
      String.concat ~sep:", " (List.map ~f:(fun x -> List.hd @@ fst x) suffixes)
    in
    let n, suffix =
      let f c = not (Char.is_digit c) in
      match String.findi s ~f with
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf "missing suffix, use one of %s" (possible_suffixes ()) ]
      | Some i -> String.split_n s i
    in
    let suffixes =
      List.concat_map suffixes ~f:(fun (xs, y) -> List.map ~f:(fun x -> x, y) xs)
    in
    let factor =
      match List.assoc suffixes suffix with
      | Some f -> f
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf "invalid suffix, use one of %s" (possible_suffixes ()) ]
    in
    Option.map ~f:(mul factor) (of_string n)
  in
  basic_loc name unit_number_of_string
;;

let unit_number = unit_number_generic ~of_string:Int.of_string ~mul:( * )

let unit_number_int64 =
  (* This can go into a separate module [stdune/int64.ml]. *)
  let of_string s = Int64.of_string_opt s in
  unit_number_generic ~of_string ~mul:Int64.mul
;;

let duration = unit_number "Duration" [ [ "s" ], 1; [ "m" ], 60; [ "h" ], 60 * 60 ]
let bytes_unit = unit_number_int64 "Byte amount" Bytes_unit.conversion_table
let maybe t = t >>| Option.some <|> return None
let sum ?(force_parens = false) cstrs = Sum { force_parens; cstrs }
let enum' cstrs = Enum cstrs
let enum cstrs = enum' (List.map cstrs ~f:(fun (name, v) -> name, return v))
let bool = enum [ "true", true; "false", false ]
let map_validate t ~f = Map_validate (t, f)
let field name ?default ?on_dup t = Field { name; default; on_dup; t }

let field_b_gen field_gen ?check ?on_dup name =
  field_gen
    name
    ?on_dup
    (let* () = Option.value check ~default:(return ()) in
     eos
     >>= function
     | true -> return true
     | _ -> bool)
;;

let field_b = field_b_gen (field ~default:false)
let field_o_b = field_b_gen field_o
let multi_field name t = Multi_field { name; t }

let leftover_fields_generic t more_fields =
  Leftover_fields_generic { t; names = more_fields }
;;

let leftover_fields : (Ast.t list, Fields.t) parser = Leftover_fields

let leftover_fields_as_sums cstrs =
  leftover_fields_generic (sum cstrs) (List.map cstrs ~f:fst)
;;

let kind : type k. (Kind.t, k) parser = Kind
let all l = traverse l ~f:Fun.id

let fields_mutually_exclusive ?on_dup ?default fields =
  Fields_mutually_exclusive { on_dup; default; fields }
;;

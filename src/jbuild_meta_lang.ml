open Import
open Sexp.Of_sexp

module To = Sexp.To_sexp

module Prim = struct
  module Spec = struct
    [@@@warning "-37"]

    type ('a, 'b) t =
      | Ret : ('b -> Sexp.t) -> ('b, 'b) t
      | Rest : (Sexp.Ast.t -> 'a) * ('b -> Sexp.t) -> ('a list -> 'b, 'b) t
      | Abs : (Sexp.Ast.t -> 'a) * ('b, 'c) t -> ('a -> 'b, 'c) t

    let ( @> ) a b = Abs (a, b)

    let rec apply
      : type a b. (a, b) t -> loc:Loc.t -> Sexp.Ast.t list -> a -> Sexp.Ast.t
      = fun t ~loc l f ->
        match t, l with
        | Ret conv, [] -> Sexp.add_loc (conv f) ~loc
        | Ret _, _ :: _ -> Loc.fail loc "too many arguments"
        | Rest (conv, ret_conv), l ->
          Sexp.add_loc (ret_conv (f (List.map l ~f:conv))) ~loc
        | Abs _, [] -> Loc.fail loc "not enough arguments"
        | Abs (conv, t), x :: l ->
          apply t ~loc l (f (conv x))
  end

  type ('a, 'b) unpacked =
    { spec : ('a, 'b) Spec.t
    ; exec : 'a
    }

  type t = T : (_, _) unpacked -> t

  let make spec exec = T { spec; exec }

  let exec (T { spec; exec }) ~loc args =
    Spec.apply spec ~loc args exec
end

type env =
  { context : Context.t
  ; macros  : macro String_map.t
  }

and macro =
  | Value    of Sexp.Ast.t list
  | Closure of
      { env     : env
      ; pattern : Sexp.Ast.t
      ; form    : Sexp.Ast.t list
      }
  | Prim of Prim.t

let prims =
  let open Prim.Spec in
  let mk name spec exec = (name, Prim (Prim.make spec exec)) in
  let cmp name f = mk name (string @> string @> Ret To.bool) f in
  (* CR-someday jdimino: implement proper version comparison *)
  [ cmp ":ver<"   (<)
  ; cmp ":ver<="  (<=)
  ; cmp ":ver="   (=)
  ; cmp ":ver>="  (>=)
  ; cmp ":ver>"   (>)
  ; mk  ":concat" (string @> list string @> Ret To.string)
      (fun sep l -> String.concat ~sep l)
  ] |> String_map.of_alist_exn

let make_env context =
  { context; macros = prims }

let bind env var macro =
  { env with macros = String_map.add env.macros ~key:var ~data:macro }

let rec match_pattern env ~pattern ~value =
  match pattern, value with
  | Atom (_, var), _ when String.is_prefix var ~prefix:":" ->
    bind env var (Value [value])
  | Atom (_, a), Atom (_, b) when a = b -> env
  | List (_, a), List (_, b) when List.length a = List.length b ->
    List.fold_left2 a b ~init:env ~f:(fun env pattern value ->
      match_pattern env ~pattern ~value)
  | _ ->
    Loc.fail (Sexp.Ast.loc pattern)
      "Failed to match value against pattern:\n\
       - pattern: %s\n\
       - value:   %s"
      (Sexp.Ast.to_string pattern)
      (Sexp.Ast.to_string value)

let unexpected sexp values ~expected =
  Loc.fail (Sexp.Ast.loc sexp)
    "%s expected here, got:\n\
    \  %s"
    expected
    (String.concat ~sep:", "
       (List.map values ~f:Sexp.Ast.to_string))

let rec eval env t =
  match t with
  | Atom _ -> (env, [t])
  | List (_, Atom (_, ":quote") :: args) -> (env, args)
  | List (loc, Atom (_, ":if") :: args) -> begin
      let cond, then_, else_ =
        match args with
        | [cond; then_] ->
          let loc = Sexp.Ast.loc then_ in
          (cond, then_, List ({ loc with start = loc.stop }, []))
        | [cond; then_; else_] ->
          (cond, then_, else_)
        | _ ->
          Loc.fail loc "invalid (:if ...) form"
      in
      match eval_bool env cond with
      | true  -> eval env else_
      | false -> eval env then_
    end
  | List (loc, Atom (_, ":foreach") :: args) -> begin
      match args with
      | pattern :: vals :: form -> begin
          let vals = eval_list env vals in
          (env,
           List.concat_map vals ~f:(fun value ->
               let env = match_pattern env ~pattern ~value in
               eval_seq env form))
        end
      | _ ->
        Loc.fail loc "invalid (:foreach ...) form"
    end
    | List (loc, Atom (_, ":let") :: args) -> begin
        match args with
        | [pattern; value] ->
          let value = eval_one env value in
          (match_pattern env ~pattern ~value, [])
        | _ ->
          Loc.fail loc "invalid (:let ...) form"
      end
    | List (loc, Atom (_, ":let-macro") :: args) -> begin
        match args with
        | List (_, Atom (_, s) :: _) as pattern :: form when s <> "" && s.[0] = ':' ->
          (bind env s (Closure { env; pattern; form }),
           [])
        | _ ->
          Loc.fail loc "invalid (:let-macro ...) form"
      end
    | List (loc, (Atom (loc_s, s) :: args)) when s <> "" && s.[0] = ':' -> begin
      match String_map.find s env.macros with
      | None -> Loc.fail loc_s "Unknown macro %S" s
      | Some (Value x) -> (env, x)
      | Some (Closure { env = closure_env; pattern; form }) ->
        let args = eval_seq env args in
        let t = List (loc, Atom (loc_s, s) :: args) in
        (env,
         let env = match_pattern closure_env ~pattern ~value:t in
         eval_seq env form)
      | Some (Prim prim) ->
        let args = eval_seq env args in
        (env,
         [Prim.exec prim args ~loc])
      end
    | List (loc, l) ->
      (env, [List (loc, eval_seq env l)])

and eval_seq env l =
  match l with
  | [] -> []
  | sexp :: rest ->
    let env, res = eval env sexp in
    res @ eval_seq env rest

and eval_one env sexp =
  match snd (eval env sexp) with
  | [sexp] -> sexp
  | l -> unexpected sexp l ~expected:"single value"

and eval_list env sexp =
  match eval_one env sexp with
  | List (_, l) -> l
  | l -> unexpected sexp [l] ~expected:"list"

and eval_bool env sexp : bool =
  match eval_one env sexp with
  | Atom (_, "true" ) -> true
  | Atom (_, "false") -> false
  | l -> unexpected sexp [l] ~expected:"true or false"

let expand sexps ~context =
  let env = make_env context in
  eval_seq env sexps

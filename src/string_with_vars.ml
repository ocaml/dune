open! Import

open Usexp.Template

type t = Usexp.Template.t

let literal ~quoted ~loc s =
  { parts = [Text s]
  ; quoted
  ; loc
  }

module Token = struct
  type brace = Parens | Braces
  type t =
    | String of string
    | Open   of brace
    | Close  of brace

  let tokenise s =
    let len = String.length s in
    let sub i j = String.sub s ~pos:i ~len:(j - i) in
    let cons_str i j acc = if i = j then acc else String (sub i j) :: acc in
    let rec loop i j =
      if j = len
      then cons_str i j []
      else
        match s.[j] with
        | '}' -> cons_str i j (Close Braces :: loop (j + 1) (j + 1))
        | ')' -> cons_str i j (Close Parens :: loop (j + 1) (j + 1))
        | '$' when j + 1 < len -> begin
            match s.[j + 1] with
            | '{' -> cons_str i j (Open Braces :: loop (j + 2) (j + 2))
            | '(' -> cons_str i j (Open Parens :: loop (j + 2) (j + 2))
            | _   -> loop i (j + 1)
          end
        | _ -> loop i (j + 1)
    in
    loop 0 0

  let to_string = function
    | String s     -> s
    | Open  Braces -> "${"
    | Open  Parens -> "$("
    | Close Braces -> "}"
    | Close Parens -> ")"
end

(* Remark: Consecutive [Text] items are concatenated. *)
let rec of_tokens
  : Loc.t -> Token.t list -> part list = fun loc -> function
  | [] -> []
  | Open a :: String s :: Close b :: rest when a = b ->
    let (name, payload) =
      match String.lsplit2 s ~on:':' with
      | None -> ("", s)
      | Some v -> v
    in
    Var { loc
        ; name
        ; payload
        ; syntax =
            begin match a with
            | Parens -> Dollar_paren
            | Braces -> Dollar_brace
            end
        } :: of_tokens loc rest
  | token :: rest ->
    let s = Token.to_string token in
    match of_tokens loc rest with
    | Text s' :: l -> Text (s ^ s') :: l
    | l -> Text s :: l

let items_of_string loc s = of_tokens loc (Token.tokenise s)

let _t_dune : Sexp.Of_sexp.ast -> t = function
  | Template t -> t
  | Atom(loc, A s) -> literal ~loc ~quoted:false s
  | Quoted_string (loc, s) -> literal ~loc ~quoted:true s
  | List _ as sexp ->
    Sexp.Of_sexp.of_sexp_error sexp "Atom expected"

let t_jbuild : Sexp.Of_sexp.ast -> t = function
  | Template _ -> assert false
  | Atom(loc, A s) -> { parts = items_of_string loc s; loc; quoted = false }
  | Quoted_string (loc, s) ->
    { parts = items_of_string loc s;  loc;  quoted = true }
  | List _ as sexp -> Sexp.Of_sexp.of_sexp_error sexp "Atom expected"

let t = t_jbuild

let loc t = t.loc

let virt ?(quoted=false) pos s =
  let loc = Loc.of_pos pos in
  { parts = items_of_string loc s;  loc = Loc.of_pos pos;  quoted }
let virt_var ?(quoted=false) pos s =
  assert (String.for_all s ~f:(function ':' -> false | _ -> true));
  let loc = Loc.of_pos pos in
  { parts =
      [Var { payload = s
           ; name = ""
           ; syntax = Percent
           ; loc
           }]
  ; loc
  ; quoted
  }
let virt_text pos s =
  { parts = [Text s];  loc = Loc.of_pos pos;  quoted = true }

let sexp_of_ast = Usexp.Template.sexp_of_t

let string_of_var v =
  to_string { quoted = false; parts = [Var v]; loc = v.loc }

let concat_rev = function
  | [] -> ""
  | [s] -> s
  | l -> String.concat (List.rev l) ~sep:""

module Mode = struct
  type 'a t =
    | Single : Value.t t
    | Many : Value.t list t

  let string
    : type a. a t -> string -> a
    = fun t s ->
      match t with
      | Single -> Value.String s
      | Many -> [Value.String s]

  let value
    : type a. a t -> Value.t list -> a option
    = fun t s ->
      match t, s with
      | Many, s -> Some s
      | Single, [s] -> Some s
      | Single, _ -> None
end

module Partial = struct
  type nonrec 'a t =
    | Expanded of 'a
    | Unexpanded of t
end

let invalid_multivalue (v : var) x =
  Loc.fail v.loc "Variable %s expands to %d values, \
                  however a single value is expected here. \
                  Please quote this atom."
    (string_of_var v) (List.length x)

(* let rec rev_append acc = function
 *   | Nil -> acc
 *   | Text (s, xs) -> rev_append (Text (s, acc)) xs
 *   | Var v -> rev_append (Var { v with parts = acc }) v.parts *)

let full_var_name (v : var) =
  match v.name with
  | "" -> v.payload
  | n -> n ^ ":" ^ v.payload

let partial_expand
  : 'a.t
  -> mode:'a Mode.t
  -> dir:Path.t
  -> f:(var -> string -> Value.t list option)
  -> 'a Partial.t
  = fun t ~mode ~dir ~f ->
    let commit_text acc_text acc =
      let s = concat_rev acc_text in
      if s = "" then acc else Text s :: acc
    in
    let f var = f var (full_var_name var) in
    let rec loop acc_text acc items =
      match items with
      | [] ->
        begin match acc with
        | [] ->
          Partial.Expanded (Mode.string mode (concat_rev acc_text))
        | _  ->
          Unexpanded { t with parts = List.rev (commit_text acc_text acc) }
        end
      | Text s :: items -> loop (s :: acc_text) acc items
      | Var var as it :: items ->
        begin match f var with
        | Some ([] | _::_::_ as e) when not t.quoted ->
          invalid_multivalue var e
        | Some t ->
          loop (Value.L.concat ~dir t :: acc_text) acc items
        | None -> loop [] (it :: commit_text acc_text acc) items
        end
    in
    match t.parts with
    | [] -> Partial.Expanded (Mode.string mode "")
    | [Text s] -> Expanded (Mode.string mode s)
    | [Var var] when not t.quoted ->
      begin match f var with
      | None -> Partial.Unexpanded t
      | Some e -> Expanded (
        match Mode.value mode e with
        | None -> invalid_multivalue var e
        | Some s -> s)
      end
    | _ -> loop [] [] t.parts

let expand t ~mode ~dir ~f =
  match
    partial_expand t ~mode ~dir ~f:(fun var var_val ->
      match f var.loc var_val with
      | None ->
        begin match var.syntax with
        | Percent -> Loc.fail var.loc "Failed to expand %s" (string_of_var var)
        | Dollar_brace
        | Dollar_paren -> Some [Value.String (string_of_var var)]
        end
      | s -> s)
  with
  | Partial.Expanded s -> s
  | Unexpanded _ -> assert false (* we are expanding every variable *)

let partial_expand t ~mode ~dir ~f =
  partial_expand t ~mode ~dir ~f:(fun v val_ -> f v.loc val_)

let sexp_of_t t = Usexp.Template.sexp_of_t t

let is_var { parts ; quoted = _; loc = _ } ~name =
  match parts with
  | [Var n] -> name = full_var_name n
  | _ -> false

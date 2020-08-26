open! Stdune
open! Import
open Dune_lang.Template

type t =
  { template : Dune_lang.Template.t
  ; syntax_version : Dune_lang.Syntax.Version.t
  }

let compare_no_loc t1 t2 =
  match
    Dune_lang.Syntax.Version.compare t1.syntax_version t2.syntax_version
  with
  | (Ordering.Lt | Gt) as a -> a
  | Eq -> Dune_lang.Template.compare_no_loc t1.template t2.template

let equal_no_loc t1 t2 = Ordering.is_eq (compare_no_loc t1 t2)

let make_syntax = (1, 0)

let make template = { template; syntax_version = make_syntax }

let make_text ?(quoted = false) loc s = make { parts = [ Text s ]; quoted; loc }

let make_var ?(quoted = false) loc ?payload name =
  let var = { loc; name; payload } in
  make { parts = [ Var var ]; quoted; loc }

let literal ~quoted ~loc s = { parts = [ Text s ]; quoted; loc }

let decode =
  let open Dune_lang.Decoder in
  let template_parser =
    raw >>| function
    | Template t -> t
    | Atom (loc, A s) -> literal ~quoted:false ~loc s
    | Quoted_string (loc, s) -> literal ~quoted:true ~loc s
    | List (loc, _) -> User_error.raise ~loc [ Pp.text "Unexpected list" ]
  in
  let+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax
  and+ template = template_parser in
  { template; syntax_version }

let loc t = t.template.loc

let syntax_version t = t.syntax_version

let virt_var ?(quoted = false) pos s =
  assert (
    String.for_all s ~f:(function
      | ':' -> false
      | _ -> true) );
  let loc = Loc.of_pos pos in
  let template =
    { parts = [ Var { payload = None; name = s; loc } ]; loc; quoted }
  in
  { template; syntax_version = make_syntax }

let virt_text pos s =
  let template = { parts = [ Text s ]; loc = Loc.of_pos pos; quoted = true } in
  { template; syntax_version = make_syntax }

let concat_rev = function
  | [] -> ""
  | [ s ] -> s
  | l -> String.concat (List.rev l) ~sep:""

module Mode = struct
  type _ t =
    | Single : Value.t t
    | Many : Value.t list t

  let string : type a. a t -> string -> a =
   fun t s ->
    match t with
    | Single -> Value.String s
    | Many -> [ Value.String s ]

  let value : type a. a t -> Value.t list -> a option =
   fun t s ->
    match (t, s) with
    | Many, s -> Some s
    | Single, [ s ] -> Some s
    | Single, _ -> None
end

let invalid_multivalue (v : var) x =
  User_error.raise ~loc:v.loc
    [ Pp.textf
        "Variable %s expands to %d values, however a single value is expected \
         here. Please quote this atom."
        (string_of_var v) (List.length x)
    ]

module Var = struct
  type t = var

  let loc (t : t) = t.loc

  let name { name; _ } = name

  let full_name t =
    match t.payload with
    | None -> t.name
    | Some v -> t.name ^ ":" ^ v

  let payload t = t.payload

  let to_string = string_of_var

  let to_dyn t = Dyn.Encoder.string (to_string t)

  let with_name t ~name = { t with name }

  let is_macro t = Option.is_some t.payload

  let describe t =
    to_string
      ( match t.payload with
      | None -> t
      | Some _ -> { t with payload = Some ".." } )
end

type known_suffix =
  | Full of string
  | Partial of (Var.t * string)

let known_suffix =
  let rec go t acc =
    match t with
    | Text s :: rest -> go rest (s :: acc)
    | [] -> Full (String.concat ~sep:"" acc)
    | Var v :: _ -> Partial (v, String.concat ~sep:"" acc)
  in
  fun t -> go (List.rev t.template.parts) []

type known_prefix =
  | Full of string
  | Partial of (string * Var.t)

let known_prefix =
  let rec go t acc =
    match t with
    | Text s :: rest -> go rest (s :: acc)
    | [] -> Full (String.concat ~sep:"" (List.rev acc))
    | Var v :: _ -> Partial (String.concat ~sep:"" (List.rev acc), v)
  in
  fun t -> go t.template.parts []

let fold_vars =
  let rec loop parts acc f =
    match parts with
    | [] -> acc
    | Text _ :: parts -> loop parts acc f
    | Var v :: parts -> loop parts (f v acc) f
  in
  fun t ~init ~f -> loop t.template.parts init f

type 'a expander = Var.t -> Dune_lang.Syntax.Version.t -> 'a

type yes_no_unknown =
  | Yes
  | No
  | Unknown of Var.t

let is_suffix t ~suffix:want =
  match known_suffix t with
  | Full s ->
    if String.is_suffix ~suffix:want s then
      Yes
    else
      No
  | Partial (v, have) ->
    if String.is_suffix ~suffix:want have then
      Yes
    else if String.is_suffix ~suffix:have want then
      Unknown v
    else
      No

let is_prefix t ~prefix:want =
  match known_prefix t with
  | Full s ->
    if String.is_prefix ~prefix:want s then
      Yes
    else
      No
  | Partial (have, v) ->
    if String.is_prefix ~prefix:want have then
      Yes
    else if String.is_prefix ~prefix:have want then
      Unknown v
    else
      No

module Private = struct
  module Partial = struct
    type nonrec 'a t =
      | Expanded of 'a
      | Unexpanded of t

    let map t ~f =
      match t with
      | Expanded t -> Expanded (f t)
      | Unexpanded t -> Unexpanded t

    let is_suffix t ~suffix =
      match t with
      | Expanded s ->
        if String.is_suffix ~suffix s then
          Yes
        else
          No
      | Unexpanded t -> is_suffix t ~suffix

    let is_prefix t ~prefix =
      match t with
      | Expanded s ->
        if String.is_prefix ~prefix s then
          Yes
        else
          No
      | Unexpanded t -> is_prefix t ~prefix
  end
end

open Private

module type S = sig
  type 'a app

  val expand :
       t
    -> mode:'a Mode.t
    -> dir:Path.t
    -> f:Value.t list option app expander
    -> 'a app

  val partial_expand :
       t
    -> mode:'a Mode.t
    -> dir:Path.t
    -> f:Value.t list option app expander
    -> 'a Partial.t app
end

module Make (A : Applicative_intf.S1) = struct
  (* We parameterize expansion over an applicative to be able to accumulate
     dependencies as we expand. *)
  module App = Applicative.Make (A)

  let partial_expand :
        'a.    t -> mode:'a Mode.t -> dir:Path.t
        -> f:Value.t list option A.t expander -> 'a Partial.t A.t =
   fun ({ template; syntax_version } as t) ~mode ~dir ~f ->
    match template.parts with
    (* Optimizations for some common cases *)
    | [] -> A.return (Partial.Expanded (Mode.string mode ""))
    | [ Text s ] -> A.return (Partial.Expanded (Mode.string mode s))
    | [ Var var ] when not template.quoted -> (
      let open App.O in
      let+ exp = f var syntax_version in
      match exp with
      | None -> Partial.Unexpanded t
      | Some e ->
        Expanded
          ( match Mode.value mode e with
          | None -> invalid_multivalue var e
          | Some s -> s ) )
    | _ ->
      let expanded_parts =
        List.map template.parts ~f:(fun part ->
            match part with
            | Text s -> App.return (Text s)
            | Var var -> (
              let open App.O in
              let+ exp = f var syntax_version in
              match exp with
              | Some (([] | _ :: _ :: _) as e) when not template.quoted ->
                invalid_multivalue var e
              | Some t -> Text (Value.L.concat ~dir t)
              | None -> Var var ))
      in
      let open App.O in
      let+ expanded = App.all expanded_parts in
      let commit_text acc_text acc =
        let s = concat_rev acc_text in
        if s = "" then
          acc
        else
          Text s :: acc
      in
      (* This pass merges all consecutive [Text] constructors *)
      let rec loop acc_text acc items =
        match items with
        | [] -> (
          match acc with
          | [] -> Partial.Expanded (Mode.string mode (concat_rev acc_text))
          | _ ->
            let template =
              { template with parts = List.rev (commit_text acc_text acc) }
            in
            Unexpanded { template; syntax_version } )
        | Text s :: items -> loop (s :: acc_text) acc items
        | it :: items -> loop [] (it :: commit_text acc_text acc) items
      in
      loop [] [] expanded

  let expand t ~mode ~dir ~f =
    let open App.O in
    let+ exp =
      partial_expand t ~mode ~dir ~f:(fun var syntax_version ->
          let+ exp = f var syntax_version in
          match exp with
          | None ->
            if Var.is_macro var then
              User_error.raise ~loc:var.loc
                [ Pp.textf "Unknown macro %s" (Var.describe var) ]
            else
              User_error.raise ~loc:var.loc
                [ Pp.textf "Unknown variable %S" (Var.name var) ]
          | s -> s)
    in
    match exp with
    | Partial.Expanded s -> s
    | Unexpanded _ -> assert false
end

include Make (Applicative.Id)

let is_var { template; syntax_version = _ } ~name =
  match template.parts with
  | [ Var n ] -> name = Var.full_name n
  | _ -> false

let text_only t =
  match t.template.parts with
  | [ Text s ] -> Some s
  | _ -> None

let has_vars t = Option.is_none (text_only t)

let encode t =
  match text_only t with
  | Some s -> Dune_lang.atom_or_quoted_string s
  | None -> Dune_lang.Template t.template

let to_dyn t = Dune_lang.to_dyn (encode t)

let remove_locs t =
  { t with template = Dune_lang.Template.remove_locs t.template }

module Partial = struct
  include Private.Partial

  let to_dyn f =
    let open Dyn.Encoder in
    function
    | Expanded x -> constr "Expander" [ f x ]
    | Unexpanded t -> constr "Unexpanded" [ to_dyn t ]

  let elim t ~exp ~unexp =
    match t with
    | Expanded e -> exp e
    | Unexpanded t -> unexp t

  let expanded t = Expanded t
end

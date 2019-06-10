open! Stdune
open! Import

open Dune_lang.Template

type t =
  { template : Dune_lang.Template.t
  ; syntax_version : Syntax.Version.t
  }

let compare_no_loc t1 t2 =
  match Syntax.Version.compare t1.syntax_version t2.syntax_version with
  | Ordering.Lt | Gt as a -> a
  | Eq -> Dune_lang.Template.compare_no_loc t1.template t2.template

let make_syntax = (1, 0)

let make ?(quoted=false) loc part =
  { template =
      { parts = [part]
      ; quoted
      ; loc
      }
  ; syntax_version = make_syntax
  }

let make_text ?quoted loc s =
  make ?quoted loc (Text s)

let make_var ?quoted loc ?payload name =
  let var =
    { loc
    ; name
    ; payload
    ; syntax = Percent
  }
  in
  make ?quoted loc (Var var)

let literal ~quoted ~loc s =
  { parts = [Text s]
  ; quoted
  ; loc
  }

(* This module implements the "old" template parsing that is only used in jbuild
   files *)
module Jbuild : sig
  val parse : string -> loc:Loc.t -> quoted:bool -> Dune_lang.Template.t
end = struct
  type var_syntax = Parens | Braces
  module Token = struct
    type t =
      | String of string
      | Open   of var_syntax
      | Close  of var_syntax

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
        | None -> (s, None)
        | Some (n, p) -> (n, Some p)
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

  let parse s ~loc ~quoted =
    { parts = of_tokens loc (Token.tokenise s)
    ; loc
    ; quoted
    }
end

let decode =
  let open Dune_lang.Decoder in
  let jbuild =
    raw >>| function
    | Template _ as t ->
      Errors.code_error "Unexpected dune template from a jbuild file"
        [ "t", Dune_lang.to_sexp (Dune_lang.Ast.remove_locs t)
        ]
    | Atom(loc, A s) -> Jbuild.parse s ~loc ~quoted:false
    | Quoted_string (loc, s) -> Jbuild.parse s ~loc ~quoted:true
    | List (loc, _) -> Dune_lang.Decoder.of_sexp_error loc "Atom expected"
  in
  let dune =
    raw >>| function
    | Template t -> t
    | Atom(loc, A s) -> literal ~quoted:false ~loc s
    | Quoted_string (loc, s) -> literal ~quoted:true ~loc s
    | List (loc, _) -> Dune_lang.Decoder.of_sexp_error loc "Unexpected list"
  in
  let template_parser = Stanza.Decoder.switch_file_kind ~jbuild ~dune in
  let+ syntax_version = Syntax.get_exn Stanza.syntax
  and+ template = template_parser
  in
  {template; syntax_version}

let loc t = t.template.loc

let syntax_version t = t.syntax_version

let virt ?(quoted=false) pos s =
  let template = Jbuild.parse ~quoted ~loc:(Loc.of_pos pos) s in
  {template; syntax_version = make_syntax}

let virt_var ?(quoted=false) pos s =
  assert (String.for_all s ~f:(function ':' -> false | _ -> true));
  let loc = Loc.of_pos pos in
  let template =
    { parts =
        [Var { payload = None
             ; name = s
             ; syntax = Percent
             ; loc
             }]
    ; loc
    ; quoted
    }
  in
  {template; syntax_version = make_syntax}

let virt_text pos s =
  let template = { parts = [Text s];  loc = Loc.of_pos pos;  quoted = true } in
  {template; syntax_version = make_syntax}

let concat_rev = function
  | [] -> ""
  | [s] -> s
  | l -> String.concat (List.rev l) ~sep:""

module Mode = struct
  type _ t =
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

let invalid_multivalue (v : var) x =
  Errors.fail v.loc "Variable %s expands to %d values, \
                   however a single value is expected here. \
                   Please quote this atom."
    (string_of_var v) (List.length x)

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

  let to_sexp t = Sexp.Encoder.string (to_string t)

  let with_name t ~name =
    { t with name }

  let is_macro t = Option.is_some t.payload

  let describe t =
    to_string
      (match t.payload with
       | None   -> t
       | Some _ -> { t with payload = Some ".." })
end

type known_suffix =
  | Full of string
  | Partial of (Var.t * string)

let known_suffix =
  let rec go t acc = match t with
    | Text s :: rest -> go rest (s :: acc)
    | [] -> Full (String.concat ~sep:"" acc)
    | Var v :: _ -> Partial (v, String.concat ~sep:"" acc)
  in
  fun t -> go (List.rev t.template.parts) []

type known_prefix =
  | Full of string
  | Partial of (string * Var.t)

let known_prefix =
  let rec go t acc = match t with
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
  fun t ~init ~f ->
    loop t.template.parts init f

type 'a expander = Var.t -> Syntax.Version.t -> 'a

type yes_no_unknown =
  | Yes | No | Unknown of Var.t

let is_suffix t ~suffix:want =
  match known_suffix t with
  | Full s -> if String.is_suffix ~suffix:want s then Yes else No
  | Partial (v, have) ->
    if String.is_suffix ~suffix:want have then Yes
    else
    if String.is_suffix ~suffix:have want then Unknown v
    else
      No

let is_prefix t ~prefix:want =
  match known_prefix t with
  | Full s -> if String.is_prefix ~prefix:want s then Yes else No
  | Partial (have, v) ->
    if String.is_prefix ~prefix:want have then Yes
    else
    if String.is_prefix ~prefix:have want then Unknown v
    else
      No

module Private = struct
  module Partial = struct
    type nonrec 'a t =
      | Expanded of 'a
      | Unexpanded of t

    let map t ~f = match t with
      | Expanded t -> Expanded (f t)
      | Unexpanded t -> Unexpanded t

    let is_suffix t ~suffix =
      match t with
      | Expanded s ->
        if String.is_suffix ~suffix s then Yes else No
      | Unexpanded t ->
        is_suffix t ~suffix

    let is_prefix t ~prefix =
      match t with
      | Expanded s ->
        if String.is_prefix ~prefix s then Yes else No
      | Unexpanded t ->
        is_prefix t ~prefix

  end
end
open Private

let partial_expand
  : 'a.t
  -> mode:'a Mode.t
  -> dir:Path.t
  -> f:Value.t list option expander
  -> 'a Partial.t
  = fun ({template; syntax_version} as t) ~mode ~dir ~f ->
    let commit_text acc_text acc =
      let s = concat_rev acc_text in
      if s = "" then acc else Text s :: acc
    in
    let rec loop acc_text acc items =
      match items with
      | [] ->
        begin match acc with
        | [] ->
          Partial.Expanded (Mode.string mode (concat_rev acc_text))
        | _  ->
          let template = {template with parts = List.rev (commit_text acc_text acc)} in
          Unexpanded {template; syntax_version}
        end
      | Text s :: items -> loop (s :: acc_text) acc items
      | Var var as it :: items ->
        begin match f var syntax_version with
        | Some ([] | _::_::_ as e) when not template.quoted ->
          invalid_multivalue var e
        | Some t ->
          loop (Value.L.concat ~dir t :: acc_text) acc items
        | None -> loop [] (it :: commit_text acc_text acc) items
        end
    in
    match template.parts with
    | [] -> Partial.Expanded (Mode.string mode "")
    | [Text s] -> Expanded (Mode.string mode s)
    | [Var var] when not template.quoted ->
      begin match f var syntax_version with
      | None -> Partial.Unexpanded t
      | Some e -> Expanded (
        match Mode.value mode e with
        | None -> invalid_multivalue var e
        | Some s -> s)
      end
    | _ -> loop [] [] template.parts

let expand t ~mode ~dir ~f =
  match
    partial_expand t ~mode ~dir ~f:(fun var syntax_version ->
      match f var syntax_version with
      | None ->
        begin match var.syntax with
        | Percent ->
          if Var.is_macro var then
            Errors.fail var.loc "Unknown macro %s" (Var.describe var)
          else
            Errors.fail var.loc "Unknown variable %S" (Var.name var)
        | Dollar_brace
        | Dollar_paren -> Some [Value.String (string_of_var var)]
        end
      | s -> s)
  with
  | Partial.Expanded s -> s
  | Unexpanded _ -> assert false (* we are expanding every variable *)

let partial_expand t ~mode ~dir ~f = partial_expand t ~mode ~dir ~f

let is_var { template; syntax_version = _ } ~name =
  match template.parts with
  | [Var n] -> name = Var.full_name n
  | _ -> false

let text_only t =
  match t.template.parts with
  | [Text s] -> Some s
  | _ -> None

let has_vars t = Option.is_none (text_only t)

let encode t =
  match text_only t with
  | Some s -> Dune_lang.atom_or_quoted_string s
  | None -> Dune_lang.Template t.template

let to_sexp t = Dune_lang.to_sexp (encode t)

let remove_locs t =
  { t with template = Dune_lang.Template.remove_locs t.template
  }

module Upgrade_var = struct
  type info =
    | Keep
    | Deleted of string
    | Renamed_to of string

  let map =
    let macros =
      [ "exe", Keep
      ; "bin", Keep
      ; "lib", Keep
      ; "libexec", Keep
      ; "lib-available", Keep
      ; "version", Keep
      ; "read", Keep
      ; "read-lines", Keep
      ; "read-strings", Keep
      ; "path", Renamed_to "dep"
      ; "findlib", Renamed_to "lib"
      ; "path-no-dep", Deleted ""
      ; "ocaml-config", Keep
      ]
    in
    let static_vars =
      [ "<", Deleted
               "Use a named dependency instead:\
                \n\
                \n  (deps (:x <dep>) ...)\
                \n   ... %{x} ..."
      ; "@", Renamed_to "targets"
      ; "^", Renamed_to "deps"
      ; "SCOPE_ROOT", Renamed_to "project_root"
      ]
    in
    let lowercased =
      [ "cpp"            , Keep
      ; "pa_cpp"         , Keep
      ; "cc"             , Keep
      ; "cxx"            , Keep
      ; "ocaml"          , Keep
      ; "ocamlc"         , Keep
      ; "ocamlopt"       , Keep
      ; "arch_sixtyfour" , Keep
      ; "make"           , Keep
      ]
    in
    let uppercased =
      List.map lowercased ~f:(fun (k, _) ->
        (String.uppercase k, Renamed_to k))
    in
    let other =
      [ "-verbose"       , Keep
      ; "ocaml_bin"      , Keep
      ; "ocaml_version"  , Keep
      ; "ocaml_where"    , Keep
      ; "null"           , Keep
      ; "ext_obj"        , Keep
      ; "ext_asm"        , Keep
      ; "ext_lib"        , Keep
      ; "ext_dll"        , Keep
      ; "ext_exe"        , Keep
      ; "profile"        , Keep
      ; "workspace_root" , Keep
      ; "context_name"   , Keep
      ; "ROOT"           , Renamed_to "workspace_root"
      ; "corrected-suffix", Keep
      ; "library-name"   , Keep
      ; "impl-files"     , Keep
      ; "intf-files"     , Keep
      ]
    in
    String.Map.of_list_exn
      (List.concat
         [ macros
         ; static_vars
         ; lowercased
         ; uppercased
         ; other
         ])
end

let upgrade_to_dune t ~allow_first_dep_var =
  if t.syntax_version >= make_syntax then
    t
  else begin
    let map_var (v : Var.t) =
      match String.Map.find Upgrade_var.map v.name with
      | None -> None
      | Some info ->
        match info with
        | Deleted repl ->
          if v.name = "<" && allow_first_dep_var then
            Some v.name
          else
            Errors.fail v.loc "%s is not supported in dune files.%s"
              (Var.describe v) repl
        | Keep -> Some v.name
        | Renamed_to new_name ->
          Some new_name
    in
    let map_part = function
      | Text _ as part -> part
      | Var v ->
        match map_var v with
        | None -> Text (string_of_var v)
        | Some name ->
          Var { v with name; syntax = Percent }
    in
    { syntax_version = make_syntax
    ; template =
        { t.template with parts = List.map t.template.parts ~f:map_part }
    }
  end

module Partial = struct
  include Private.Partial

  let to_sexp f t =
    match t with
    | Expanded x ->
      Sexp.List [
        Sexp.Atom "Expanded"; f x
      ]
    | Unexpanded t ->
      Sexp.List [
        Sexp.Atom "Unexpanded"; to_sexp t
      ]
end

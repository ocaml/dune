open! Stdune
open! Dune_engine
open Import

type t =
  { name : Lib_name.t option
  ; entries : entry list
  }

and entry =
  | Comment of string
  | Rule of rule
  | Package of t

and rule =
  { var : string
  ; predicates : predicate list
  ; action : action
  ; value : string
  }

and action =
  | Set
  | Add

and predicate =
  | Pos of string
  | Neg of string

let add_versions t ~get_version =
  let rec map_entries ~rev_path ~has_version ~has_rules = function
    | [] -> (
      if has_version || not has_rules then
        []
      else
        match get_version (List.rev rev_path) with
        | None -> []
        | Some v ->
          [ Rule { var = "version"; predicates = []; action = Set; value = v } ]
      )
    | entry :: entries -> (
      match entry with
      | Comment _ ->
        entry :: map_entries entries ~rev_path ~has_version ~has_rules
      | Rule rule ->
        entry
        :: map_entries entries ~rev_path
             ~has_version:(has_version || String.equal rule.var "version")
             ~has_rules:true
      | Package t ->
        Package (map_package t ~rev_path)
        :: map_entries entries ~rev_path ~has_version ~has_rules )
  and map_package t ~rev_path =
    let rev_path =
      match t.name with
      | None -> rev_path
      | Some n -> n :: rev_path
    in
    { t with
      entries =
        map_entries t.entries ~rev_path ~has_version:false ~has_rules:false
    }
  in
  map_package t ~rev_path:[]

module Parse = struct
  let error lexbuf msg =
    User_error.raise ~loc:(Loc.of_lexbuf lexbuf) [ Pp.text msg ]

  let next = Meta_lexer.token

  let package_name lb =
    match next lb with
    | String s ->
      if String.contains s '.' then
        error lb "'.' not allowed in sub-package names";
      let loc = Loc.of_lexbuf lb in
      Lib_name.parse_string_exn (loc, s)
    | _ -> error lb "package name expected"

  let string lb =
    match next lb with
    | String s -> s
    | _ -> error lb "string expected"

  let lparen lb =
    match next lb with
    | Lparen -> ()
    | _ -> error lb "'(' expected"

  let action lb =
    match next lb with
    | Equal -> Set
    | Plus_equal -> Add
    | _ -> error lb "'=' or '+=' expected"

  let rec predicates_and_action lb acc =
    match next lb with
    | Rparen -> (List.rev acc, action lb)
    | Name n -> after_predicate lb (Pos n :: acc)
    | Minus ->
      let n =
        match next lb with
        | Name p -> p
        | _ -> error lb "name expected"
      in
      after_predicate lb (Neg n :: acc)
    | _ -> error lb "name, '-' or ')' expected"

  and after_predicate lb acc =
    match next lb with
    | Rparen -> (List.rev acc, action lb)
    | Comma -> predicates_and_action lb acc
    | _ -> error lb "')' or ',' expected"

  let rec entries lb depth acc =
    match next lb with
    | Rparen ->
      if depth > 0 then
        List.rev acc
      else
        error lb "closing parenthesis without matching opening one"
    | Eof ->
      if depth = 0 then
        List.rev acc
      else
        error lb (sprintf "%d closing parentheses missing" depth)
    | Name "package" ->
      let name = package_name lb in
      lparen lb;
      let sub_entries = entries lb (depth + 1) [] in
      entries lb depth
        (Package { name = Some name; entries = sub_entries } :: acc)
    | Name var ->
      let predicates, action =
        match next lb with
        | Equal -> ([], Set)
        | Plus_equal -> ([], Add)
        | Lparen -> predicates_and_action lb []
        | _ -> error lb "'=', '+=' or '(' expected"
      in
      let value = string lb in
      entries lb depth (Rule { var; predicates; action; value } :: acc)
    | _ -> error lb "'package' or variable name expected"
end

let dyn_of_action =
  let open Dyn.Encoder in
  function
  | Set -> constr "Set" []
  | Add -> constr "Add" []

let dyn_of_predicate =
  let open Dyn.Encoder in
  function
  | Pos s -> constr "Pos" [ String s ]
  | Neg s -> constr "Neg" [ String s ]

let dyn_of_rule { var; predicates; action; value } =
  let open Dyn.Encoder in
  record
    [ ("var", string var)
    ; ("predicates", list dyn_of_predicate predicates)
    ; ("action", dyn_of_action action)
    ; ("value", string value)
    ]

module Simplified = struct
  module Rules = struct
    type t =
      { set_rules : rule list
      ; add_rules : rule list
      }

    let to_dyn { set_rules; add_rules } =
      let open Dyn.Encoder in
      record
        [ ("set_rules", list dyn_of_rule set_rules)
        ; ("add_rules", list dyn_of_rule add_rules)
        ]
  end

  type t =
    { name : Lib_name.t option
    ; vars : Rules.t String.Map.t
    ; subs : t list
    }

  let rec to_dyn { name; vars; subs } =
    let open Dyn.Encoder in
    record
      [ ("name", option Lib_name.to_dyn name)
      ; ("vars", String.Map.to_dyn Rules.to_dyn vars)
      ; ("subs", list to_dyn subs)
      ]
end

let rec simplify t =
  List.fold_right t.entries
    ~init:{ name = t.name; vars = String.Map.empty; subs = [] }
    ~f:(fun entry (pkg : Simplified.t) ->
      match entry with
      | Comment _ -> pkg
      | Package sub -> { pkg with subs = simplify sub :: pkg.subs }
      | Rule rule ->
        let rules =
          Option.value
            (String.Map.find pkg.vars rule.var)
            ~default:{ set_rules = []; add_rules = [] }
        in
        let rules =
          match rule.action with
          | Set -> { rules with set_rules = rule :: rules.set_rules }
          | Add -> { rules with add_rules = rule :: rules.add_rules }
        in
        { pkg with vars = String.Map.set pkg.vars rule.var rules })

let parse_entries lb = Parse.entries lb 0 []

let load p ~name =
  let name = Option.map name ~f:Lib_name.of_package_name in
  { name; entries = Io.with_lexbuf_from_file p ~f:parse_entries } |> simplify

let rule var predicates action value = Rule { var; predicates; action; value }

let requires ?(preds = []) pkgs =
  rule "requires" preds Set (String.concat ~sep:" " pkgs)

let version s = rule "version" [] Set s

let directory s = rule "directory" [] Set s

let archive p s = rule "archive" [ Pos p ] Set s

let plugin p s = rule "plugin" [ Pos p ] Set s

let archives name =
  [ archive "byte" (name ^ Mode.compiled_lib_ext Byte)
  ; archive "native" (name ^ Mode.compiled_lib_ext Native)
  ; plugin "byte" (name ^ Mode.compiled_lib_ext Byte)
  ; plugin "native" (name ^ Mode.plugin_ext Native)
  ]

let builtins ~stdlib_dir ~version:ocaml_version =
  let version = version "[distributed with Ocaml]" in
  let simple name ?dir ?archive_name deps =
    let archive_name =
      match archive_name with
      | None -> name
      | Some a -> a
    in
    let name = Lib_name.of_string name in
    let archives = archives archive_name in
    { name = Some name
    ; entries =
        requires deps :: version
        ::
        ( match dir with
        | None -> archives
        | Some d -> directory d :: archives )
    }
  in
  let dummy name =
    { name = Some (Lib_name.of_string name); entries = [ version ] }
  in
  let compiler_libs =
    let sub name deps =
      Package (simple name deps ~archive_name:("ocaml" ^ name))
    in
    { name = Some (Lib_name.of_string "compiler-libs")
    ; entries =
        [ requires []
        ; version
        ; directory "+compiler-libs"
        ; sub "common" []
        ; sub "bytecomp" [ "compiler-libs.common" ]
        ; sub "optcomp" [ "compiler-libs.common" ]
        ; sub "toplevel" [ "compiler-libs.bytecomp" ]
        ]
    }
  in
  let stdlib = dummy "stdlib" in
  let str = simple "str" [] ~dir:"+" in
  let unix = simple "unix" [] ~dir:"+" in
  let bigarray =
    if
      Ocaml_version.stdlib_includes_bigarray ocaml_version
      && not (Path.exists (Path.relative stdlib_dir "bigarray.cma"))
    then
      dummy "bigarray"
    else
      simple "bigarray" [ "unix" ] ~dir:"+"
  in
  let dynlink = simple "dynlink" [] ~dir:"+" in
  let bytes = dummy "bytes" in
  let result = dummy "result" in
  let uchar = dummy "uchar" in
  let seq = dummy "seq" in
  let threads =
    { name = Some (Lib_name.of_string "threads")
    ; entries =
        [ version
        ; requires ~preds:[ Pos "mt"; Pos "mt_vm" ] [ "threads.vm" ]
        ; requires ~preds:[ Pos "mt"; Pos "mt_posix" ] [ "threads.posix" ]
        ; directory "+"
        ; rule "type_of_threads" [] Set "posix"
        ; rule "error" [ Neg "mt" ] Set "Missing -thread or -vmthread switch"
        ; rule "error"
            [ Neg "mt_vm"; Neg "mt_posix" ]
            Set "Missing -thread or -vmthread switch"
        ; Package
            (simple "vm" [ "unix" ] ~dir:"+vmthreads" ~archive_name:"threads")
        ; Package
            (simple "posix" [ "unix" ] ~dir:"+threads" ~archive_name:"threads")
        ]
    }
  in
  let num =
    { name = Some (Lib_name.of_string "num")
    ; entries =
        [ requires [ "num.core" ]
        ; version
        ; Package (simple "core" [] ~dir:"+" ~archive_name:"nums")
        ]
    }
  in
  let libs =
    let base =
      [ stdlib; compiler_libs; str; unix; bigarray; threads; dynlink; bytes ]
    in
    let base =
      if Ocaml_version.pervasives_includes_result ocaml_version then
        result :: base
      else
        base
    in
    let base =
      if Ocaml_version.stdlib_includes_uchar ocaml_version then
        uchar :: base
      else
        base
    in
    let base =
      if Ocaml_version.stdlib_includes_seq ocaml_version then
        seq :: base
      else
        base
    in
    (* We do not rely on an "exists_if" ocamlfind variable, because it would
       produce an error message mentioning a "hidden" package (which could be
       confusing). *)
    if Path.exists (Path.relative stdlib_dir "nums.cma") then
      num :: base
    else
      base
  in
  List.filter_map libs ~f:(fun t ->
      Option.map t.name ~f:(fun name ->
          (Lib_name.package_name name, simplify t)))
  |> Package.Name.Map.of_list_exn

let string_of_action = function
  | Set -> "="
  | Add -> "+="

let pp_predicate p =
  Pp.verbatim
    ( match p with
    | Pos p -> p
    | Neg p -> "-" ^ p )

let pp_print_text s =
  let open Pp.O in
  Pp.verbatim "\""
  ++ Pp.hvbox (Pp.text (String.escape_only '"' s))
  ++ Pp.verbatim "\""

let pp_print_string s =
  let open Pp.O in
  Pp.verbatim "\""
  ++ Pp.hvbox (Pp.verbatim (String.escape_only '"' s))
  ++ Pp.verbatim "\""

let pp_quoted_value var =
  match var with
  | "archive"
  | "plugin"
  | "requires"
  | "ppx_runtime_deps"
  | "linkopts"
  | "jsoo_runtime" ->
    pp_print_text
  | _ -> pp_print_string

let rec pp entries = Pp.vbox (Pp.concat_map entries ~sep:Pp.newline ~f:pp_entry)

and pp_entry entry =
  match entry with
  | Comment s -> Pp.verbatim (sprintf "# %s" s)
  | Rule { var; predicates = []; action; value } ->
    Pp.box
      (Pp.concat ~sep:Pp.space
         [ Pp.verbatim var
         ; Pp.verbatim (string_of_action action)
         ; pp_quoted_value var value
         ])
  | Rule { var; predicates; action; value } ->
    let open Pp.O in
    Pp.box
      (Pp.concat ~sep:(Pp.verbatim " ")
         [ Pp.textf "%s(" var
           ++ Pp.concat_map predicates ~sep:(Pp.verbatim ",") ~f:pp_predicate
           ++ Pp.verbatim ")"
         ; Pp.verbatim (string_of_action action)
         ; pp_quoted_value var value
         ])
  | Package { name; entries } ->
    let name =
      match name with
      | None -> ""
      | Some l -> Lib_name.to_string l
    in
    let open Pp.O in
    Pp.vbox ~indent:2
      (Pp.verbatim (sprintf "package %S (" name) ++ Pp.cut ++ pp entries)
    ++ Pp.cut ++ Pp.verbatim ")"

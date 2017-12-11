open Import

type t =
  { name    : string
  ; entries : entry list
  }

and entry =
  | Comment of string
  | Rule    of rule
  | Package of t

and rule =
  { var        : string
  ; predicates : predicate list
  ; action     : action
  ; value      : string
  }

and action = Set | Add

and predicate =
  | Pos of string
  | Neg of string

module Parse = struct
  let error = Loc.fail_lex

  let next = Meta_lexer.token

  let package_name lb =
    match next lb with
    | String s ->
      if String.contains s '.' then
        error lb "'.' not allowed in sub-package names";
      s
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
    | Equal      -> Set
    | Plus_equal -> Add
    | _          -> error lb "'=' or '+=' expected"

  let rec predicates_and_action lb acc =
    match next lb with
    | Rparen -> (List.rev acc, action lb)
    | Name n -> after_predicate lb (Pos n :: acc)
    | Minus  ->
      let n =
        match next lb with
        | Name p -> p
        | _      -> error lb "name expected"
      in
      after_predicate lb (Neg n :: acc)
    | _          -> error lb "name, '-' or ')' expected"

  and after_predicate lb acc =
    match next lb with
    | Rparen -> (List.rev acc, action lb)
    | Comma  -> predicates_and_action lb acc
    | _      -> error lb "')' or ',' expected"

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
        error lb "%d closing parentheses missing" depth
    | Name "package" ->
      let name = package_name lb in
      lparen lb;
      let sub_entries = entries lb (depth + 1) [] in
      entries lb depth (Package { name; entries = sub_entries } :: acc)
    | Name var ->
      let predicates, action =
        match next lb with
        | Equal      -> ([], Set)
        | Plus_equal -> ([], Add)
        | Lparen     -> predicates_and_action lb []
        | _          -> error lb "'=', '+=' or '(' expected"
      in
      let value = string lb in
      entries lb depth (Rule { var; predicates; action; value } :: acc)
    | _ ->
      error lb "'package' or variable name expected"
end

let load fn =
  Io.with_lexbuf_from_file fn ~f:(fun lb ->
      Parse.entries lb 0 [])

module Simplified = struct
  module Rules = struct
    type t =
      { set_rules : rule list
      ; add_rules : rule list
      }
  end

  type t =
    { name : string
    ; vars : Rules.t String_map.t
    ; subs : t list
    }
end

let rec simplify t =
  List.fold_right t.entries
    ~init:
      { name = t.name
      ; vars = String_map.empty
      ; subs = []
      }
    ~f:(fun entry (pkg : Simplified.t) ->
      match entry with
      | Comment _ -> pkg
      | Package sub ->
        { pkg with subs = simplify sub :: pkg.subs }
      | Rule rule ->
        let rules =
          String_map.find_default rule.var pkg.vars
            ~default:{ set_rules = []; add_rules = [] }
        in
        let rules =
          match rule.action with
          | Set -> { rules with set_rules = rule :: rules.set_rules }
          | Add -> { rules with add_rules = rule :: rules.add_rules }
        in
        { pkg with vars = String_map.add pkg.vars ~key:rule.var ~data:rules })

let rule var predicates action value =
  Rule { var; predicates; action; value }
let requires ?(preds=[]) pkgs =
  rule "requires" preds Set (String.concat ~sep:" " pkgs)
let version     s = rule "version"    []      Set s
let directory   s = rule "directory"  []      Set s
let archive p s   = rule "archive"    [Pos p] Set s
let plugin  p s   = rule "plugin"     [Pos p] Set s
let archives name =
  [ archive "byte"   (name ^ ".cma" )
  ; archive "native" (name ^ ".cmxa")
  ; plugin  "byte"   (name ^ ".cma" )
  ; plugin  "native" (name ^ ".cmxs")
  ]

let builtins ~stdlib_dir =
  let version = version "[distributed with Ocaml]" in
  let simple name ?dir ?(archive_name=name) deps =
    let archives = archives archive_name in
    { name
    ; entries =
        (requires deps ::
         version       ::
         match dir with
         | None -> archives
         | Some d -> directory d :: archives)
    }
  in
  let compiler_libs =
    let sub name deps =
      Package (simple name deps ~archive_name:("ocaml" ^ name))
    in
    { name = "compiler-libs"
    ; entries =
        [ requires []
        ; version
        ; directory "+compiler-libs"
        ; sub "common" []
        ; sub "bytecomp" ["compiler-libs.common"  ]
        ; sub "optcomp"  ["compiler-libs.common"  ]
        ; sub "toplevel" ["compiler-libs.bytecomp"]
        ]
    }
  in
  let str = simple "str" [] ~dir:"+" in
  let unix = simple "unix" [] ~dir:"+" in
  let bigarray = simple "bigarray" ["unix"] ~dir:"+" in
  let threads =
    { name = "threads"
    ; entries =
        [ version
        ; requires ~preds:[Pos "mt"; Pos "mt_vm"   ] ["threads.vm"]
        ; requires ~preds:[Pos "mt"; Pos "mt_posix"] ["threads.posix"]
        ; directory "+"
        ; rule "type_of_threads" [] Set "posix"
        ; rule "error" [Neg "mt"] Set "Missing -thread or -vmthread switch"
        ; rule "error" [Neg "mt_vm"; Neg "mt_posix"] Set "Missing -thread or -vmthread switch"
        ; Package (simple "vm" ["unix"] ~dir:"+vmthreads" ~archive_name:"threads")
        ; Package (simple "posix" ["unix"] ~dir:"+threads" ~archive_name:"threads")
        ]
    }
  in
  let num =
    { name = "num"
    ; entries =
        [ requires ["num.core"]
        ; version
        ; Package (simple "core" [] ~dir:"+" ~archive_name:"nums")
        ]
    }
  in
  let libs =
    (* We do not rely on an "exists_if" ocamlfind variable,
       because it would produce an error message mentioning
       a "hidden" package (which could be confusing). *)
    if Path.exists (Path.relative stdlib_dir "nums.cma") then
      [ compiler_libs; str; unix; bigarray; threads; num ]
    else
      [ compiler_libs; str; unix; bigarray; threads ]
  in
  List.map libs ~f:(fun t -> t.name, t)
  |> String_map.of_alist_exn

let string_of_action = function
  | Set -> "="
  | Add -> "+="

let string_of_predicate = function
  | Pos p -> p
  | Neg p -> "-" ^ p

let pp_list f ppf l =
  match l with
  | [] -> ()
  | x :: l ->
    f ppf x;
    List.iter l ~f:(fun x ->
      Format.pp_print_cut ppf ();
      f ppf x)

let pp_print_text ppf s =
  Format.fprintf ppf "\"@[<hv>";
  Format.pp_print_text ppf (String.escape_double_quote s);
  Format.fprintf ppf "@]\""

let pp_print_string ppf s =
  Format.fprintf ppf "\"@[<hv>";
  Format.pp_print_string ppf (String.escape_double_quote s);
  Format.fprintf ppf "@]\""

let pp_quoted_value var =
  match var with
  | "archive" | "plugin" | "requires"
  | "ppx_runtime_deps" | "linkopts" | "jsoo_runtime" ->
     pp_print_text
  | _ ->
     pp_print_string

let rec pp ppf entries =
  Format.fprintf ppf "@[<v>%a@]" (pp_list pp_entry) entries

and pp_entry ppf entry =
  let open Format in
  match entry with
  | Comment s ->
    fprintf ppf "# %s" s
  | Rule { var; predicates = []; action; value } ->
    fprintf ppf "@[%s %s %a@]"
      var (string_of_action action) (pp_quoted_value var) value
  | Rule { var; predicates; action; value } ->
    fprintf ppf "@[%s(%s) %s %a@]"
      var (String.concat ~sep:"," (List.map predicates ~f:string_of_predicate))
      (string_of_action action) (pp_quoted_value var) value
  | Package { name; entries } ->
    fprintf ppf "@[<v 2>package %S (@,%a@]@,)"
      name pp entries

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)
module Re = Dune_re

open OpamTypes
open OpamTypesBase
open OpamStd.Op

module OpamParser = OpamParser.FullPos
module OpamPrinter = OpamPrinter.FullPos

let log ?level fmt =
  OpamConsole.log "FILTER" ?level fmt
let slog = OpamConsole.slog

type env = full_variable -> variable_contents option

type fident = name option list * variable * (string * string) option

let to_string ?custom t =
  let custom ~context ~paren t =
    match custom with
    | None -> None
    | Some f -> f ~context ~paren t
  in
  let rec aux ?(context=`Or) t =
    let paren ?(cond=false) f =
      if cond || OpamFormatConfig.(!r.all_parens)
      then Printf.sprintf "(%s)" f else f
    in
    match custom ~context ~paren t with
    | Some str -> str
    | None ->
      match t with
      | FBool b    -> string_of_bool b
      | FString s  -> Printf.sprintf "%S" s
      | FIdent (pkgs,var,converter) ->
        OpamStd.List.concat_map "+"
          (function None -> "_" | Some p -> OpamPackage.Name.to_string p) pkgs
        ^ (if pkgs <> [] then ":" else "")
        ^ OpamVariable.to_string var
        ^ (match converter with
            | Some (it,ifu) -> "?"^it^":"^ifu
            | None -> "")
      | FOp(e,s,f) ->
        paren ~cond:(context <> `Or && context <> `And)
          (Printf.sprintf "%s %s %s"
             (aux ~context:`Relop e)
             (OpamPrinter.relop_kind s)
             (aux ~context:`Relop f))
      | FAnd (e,f) ->
        paren ~cond:(context <> `Or && context <> `And)
          (Printf.sprintf "%s & %s" (aux ~context:`And e) (aux ~context:`And f))
      | FOr (e,f)  ->
        paren ~cond:(context <> `Or)
          (Printf.sprintf "%s | %s" (aux e) (aux f))
      | FNot e     ->
        paren ~cond:(context = `Relop)
          (Printf.sprintf "!%s" (aux ~context:`Not e))
      | FDefined e ->
        paren ~cond:(context = `Relop)
          (Printf.sprintf "?%s" (aux ~context:`Defined e))
      | FUndef f -> Printf.sprintf "#undefined(%s)" (aux f)
  in
  aux t

let rec fold_down_left f acc filter = match filter with
  | FOp(l,_,r) | FAnd(l,r) | FOr(l,r) ->
    fold_down_left f (fold_down_left f (f acc filter) l) r
  | FNot(x) -> fold_down_left f (f acc filter) x
  | x -> f acc x

let rec map_up f = function
  | FOp (l, op, r) -> f (FOp (map_up f l, op, map_up f r))
  | FAnd (l, r) -> f (FAnd (map_up f l, map_up f r))
  | FOr (l, r) -> f (FOr (map_up f l, map_up f r))
  | FNot x -> f (FNot (map_up f x))
  | FUndef x -> f (FUndef (map_up f x))
  | (FBool _ | FString _ | FIdent _ | FDefined _) as flt -> f flt

(* ["%%"], ["%{xxx}%"], or ["%{xxx"] if unclosed *)
let string_interp_regex =
  let open Re in
  let notclose =
    rep (alt [
        diff notnl (set "}");
        seq [char '}'; alt [diff notnl (set "%"); stop] ]
      ])
  in
  compile (alt [
      str "%%";
      seq [str "%{"; group (greedy notclose); opt (group (str "}%"))];
    ])

let escape_value =
  let rex = Re.(compile @@ set "\\\"") in
  Re.Pcre.substitute ~rex ~subst:(fun s -> "\\"^s)

let escape_expansions =
  Re.replace_string Re.(compile @@ char '%') ~by:"%%"

let escape_strings = map_up @@ function
  | FString s -> FString (escape_expansions s)
  | flt -> flt

let fident_variables = function
  | [], var, _ -> [OpamVariable.Full.global var]
  | pkgs, var, _ ->
    List.map (function
        | Some n -> OpamVariable.Full.create n var
        | None -> OpamVariable.Full.self var) pkgs

(* extracts variables appearing in interpolations in a string*)
let string_variables s =
  let matches =
    let rec aux acc pos =
      try
        let ss = Re.exec ~pos string_interp_regex s in
        if Re.Group.test ss 2 then
          aux (Re.Group.get ss 1 :: acc)
            (fst (Re.Group.offset ss 0) + String.length (Re.Group.get ss 0))
        else
          aux acc (pos+1)
      with Not_found -> acc
    in
    aux [] 0
  in
  List.fold_left (fun acc s ->
      try fident_variables (filter_ident_of_string s) @ acc
      with Failure _ -> acc)
    [] matches

let variables filter =
  fold_down_left (fun acc -> function
      | FString s -> string_variables s @ acc
      | FIdent f -> fident_variables f @ acc
      | _ -> acc)
    [] filter

(* Some cast functions on values *)

let value ?default = function
  | FBool b -> B b
  | FString s -> S s
  | FUndef f ->
    (match default with
     | Some d -> d
     | None -> failwith ("Undefined filter value: "^to_string f))
  | e -> raise (Invalid_argument ("filter value: "^to_string e))

let value_string ?default = function
  | FBool b -> string_of_bool b
  | FString s -> s
  | FUndef f ->
    (match default with
     | Some d -> d
     | None -> failwith ("Undefined string filter value: "^to_string f))
  | e -> raise (Invalid_argument ("value_string: "^to_string e))

let value_bool ?default = function
  | FBool b -> b
  | FString "true" -> true
  | FString "false" -> false
  | FUndef f ->
    (match default with
     | Some d -> d
     | None -> failwith ("Undefined boolean filter value: "^to_string f))
  | e ->
    (match default with
     | Some d -> d
     | None -> raise (Invalid_argument ("value_bool: "^to_string e)))

(* Desugars the "enable" pseudo-variable *)
let desugar_fident ((packages,var,converter) as fident) =
  let enable = OpamVariable.of_string "enable" in
  if packages <> [] && var = enable && converter = None then
    packages, OpamVariable.of_string "installed", Some ("enable","disable")
  else fident

(* Resolves an ident to variable contents *)
let resolve_ident_raw ?(no_undef_expand=false) env fident =
  let open OpamStd.Option.Op in
  let packages,var,converter = desugar_fident fident in
  let bool_of_value = function
    | B b -> Some b
    | S s | L [s] ->
      (try Some (bool_of_string s) with Invalid_argument _ -> None)
    | L _ -> None
  in
  let resolve name =
    let var = match name with
      | Some n -> OpamVariable.Full.create n var
      | None -> OpamVariable.Full.self var
    in
    env var
  in
  let value_opt : variable_contents option = match packages with
  | [] -> env (OpamVariable.Full.global var)
  | [name] -> resolve name
  | names ->
    List.fold_left (fun acc name ->
        if acc = Some false then acc else
        match resolve name with
        | Some (B true) -> acc
        | v -> v >>= bool_of_value)
      (Some true) names
    >>| fun b -> B b
  in
  match converter with
  | Some (iftrue, iffalse) ->
    (match value_opt >>= bool_of_value with
     | Some true -> Some (S iftrue)
     | Some false -> Some (S iffalse)
     | None ->
         if no_undef_expand then value_opt else Some (S iffalse)
    )
  | None -> value_opt

(* Resolves [FIdent] to string or bool, using its package and converter
   specification *)
let resolve_ident ?no_undef_expand env fident =
  match resolve_ident_raw ?no_undef_expand env fident with
  | Some (B b) -> FBool b
  | Some (S s) -> FString s
  | Some (L l) -> FString (String.concat " " l)
  | None -> FUndef (FIdent fident)

(* Resolves ["%{x}%"] string interpolations *)
let expand_string_aux ?(partial=false) ?(escape_value=fun x -> x) ?default env text =
  let default fident = match default, partial with
    | None, false -> None
    | Some df, false -> Some (df fident)
    | None, true -> Some (Printf.sprintf "%%{%s}%%" fident)
    | Some df, true -> Some (Printf.sprintf "%%{%s}%%" (df fident))
  in
  let env v =
    if partial then
      match env v with
      | Some (S s) -> Some (S (escape_expansions s))
      | x -> x
    else env v
  in
  let f g =
    let str = Re.Group.get g 0 in
    if str = "%%" then (if partial then "%%" else "%")
    else if not (OpamStd.String.ends_with ~suffix:"}%" str) then
      (log "ERR: Unclosed variable replacement in %S\n" str;
       str)
    else
    let fident = String.sub str 2 (String.length str - 4) in
    resolve_ident ~no_undef_expand:partial env (filter_ident_of_string fident)
    |> value_string ?default:(default fident) |> escape_value
  in
  Re.replace string_interp_regex ~f text

let expand_string = expand_string_aux ?escape_value:None

let unclosed_expansions text =
  let re =
    Re.(
      compile (alt [
          str "%%";
          seq [str "%{";
               group (greedy (rep (diff notnl (char '}'))));
               opt (group (str "}%"))];
        ])
    )
  in
  Re.all re text |> OpamStd.List.filter_map @@ fun gr ->
  if Re.Group.test gr 1 && not (Re.Group.test gr 2) then
    Some (Re.Group.offset gr 0, Re.Group.get gr 0)
  else None

let map_variables_in_fident f (_,_,conv as fid) =
  let vars = fident_variables fid in
  match List.map f vars with
  | [] -> assert false
  | v::vars ->
    let var_name = OpamVariable.Full.variable v in
    match OpamVariable.Full.scope v with
    | OpamVariable.Full.Global ->
      if vars <> [] then invalid_arg "OpamFilter.map_variables";
      [], var_name, conv
    | OpamVariable.Full.Package _ | OpamVariable.Full.Self ->
      if (List.exists (fun v -> OpamVariable.Full.variable v <> var_name)
            vars)
      then invalid_arg "OpamFilter.map_variables";
      List.map (fun v -> match OpamVariable.Full.scope v with
          | OpamVariable.Full.Package name -> Some name
          | OpamVariable.Full.Self -> None
          | OpamVariable.Full.Global ->
            invalid_arg "OpamFilter.map_variables")
        (v::vars),
      var_name,
      conv

let map_variables_in_string f =
  expand_string
    ~partial:true
    ~default:(fun fid_string ->
        try
          fid_string |>
          filter_ident_of_string |>
          map_variables_in_fident f |>
          string_of_filter_ident
        with Failure _ -> fid_string)
    (fun _ -> None)

let map_variables f =
  map_up @@ function
  | FIdent fid -> FIdent (map_variables_in_fident f fid)
  | FString s -> FString (map_variables_in_string f s)
  | flt -> flt

let rec distribute_negations ?(neg=false) = function
  | FAnd (f1, f2) ->
    let f1 = distribute_negations ~neg f1 in
    let f2 = distribute_negations ~neg f2 in
    if neg then FOr (f1, f2) else FAnd (f1, f2)
  | FOr (f1, f2) ->
    let f1 = distribute_negations ~neg f1 in
    let f2 = distribute_negations ~neg f2 in
    if neg then FAnd (f1, f2) else FOr (f1, f2)
  | FBool b -> FBool (if neg then not b else b)
  | FOp (f1, op, f2) ->
    FOp (distribute_negations ~neg:false f1,
         (if neg then OpamFormula.neg_relop op else op),
         distribute_negations ~neg:false f2)
  | FNot f -> distribute_negations ~neg:(not neg) f
  | f -> if neg then FNot f else f

let logop1 cstr op = function
  | FUndef f -> FUndef (cstr f)
  | e ->
    try FBool (op (value_bool e))
    with Invalid_argument s -> log "ERR: %s" s; FUndef (cstr e)

let logop2 cstr op absorb e f = match e, f with
  | _, FBool x when x = absorb -> FBool x
  | FBool x, _ when x = absorb -> FBool x
  | FUndef x, FUndef y | FUndef x, y | x, FUndef y -> FUndef (cstr x y)
  | f, g ->
    try FBool (op (value_bool f) (value_bool g))
    with Invalid_argument s -> log "ERR: %s" s; FUndef (cstr f g)

(* Reduce expressions to values *)

let rec reduce_aux ?no_undef_expand ~default_str env =
  let reduce = reduce ?no_undef_expand ~default_str env in
  function
  | FUndef x -> FUndef x
  | FBool b -> FBool b
  | FString s -> FString s
  | FIdent i -> resolve_ident ?no_undef_expand env i
  | FOp (e,relop,f) ->
    (match reduce e, reduce f with
     | FUndef x, FUndef y -> FUndef (FOp (x, relop, y))
     | FUndef x, y -> FUndef (FOp (x, relop, escape_strings y))
     | x, FUndef y -> FUndef (FOp (escape_strings x, relop, y))
     | e,f ->
       FBool (OpamFormula.check_relop relop
                (OpamVersionCompare.compare (value_string e) (value_string f))))
  | FAnd (e,f) ->
    logop2 (fun e f -> FAnd (e,f)) (&&) false (reduce e) (reduce f)
  | FOr (e,f) ->
    logop2 (fun e f -> FOr (e,f)) (||) true (reduce e) (reduce f)
  | FNot e ->
    logop1 (fun e -> FNot e) not (reduce e)
  | FDefined e ->
    match reduce e with
    | FUndef _ -> FBool false
    | _ -> FBool true

and reduce ?no_undef_expand ?(default_str = Some (fun _ -> "")) env e =
  match reduce_aux ?no_undef_expand ~default_str env e with
  | FString s ->
    (try FString (expand_string ?default:default_str env s)
     with Failure _ -> FUndef (FString (expand_string ~partial:true env s)))
  | e -> e

let eval ?default env e = value ?default (reduce env e)

let eval_to_bool ?default env e = value_bool ?default (reduce env e)

let opt_eval_to_bool env opt =
  match opt with
  | None -> true
  | Some e -> value_bool ~default:false (reduce env e)

let eval_to_string ?default env e = value_string ?default (reduce env e)

let partial_eval env flt =
  match reduce ~no_undef_expand:true ~default_str:None env flt with
  | FUndef f -> f
  | f -> escape_strings f

let ident_of_var v =
  (match OpamVariable.Full.scope v with
   | OpamVariable.Full.Global -> []
   | OpamVariable.Full.Self -> [None]
   | OpamVariable.Full.Package p -> [Some p]),
  OpamVariable.Full.variable v, None

let ident_of_string s =
  ident_of_var (OpamVariable.Full.of_string s)

let ident_value ?default env id = value ?default (resolve_ident env id)

let ident_string ?default env id = value_string ?default (resolve_ident env id)

let ident_bool ?default env id = value_bool ?default (resolve_ident env id)

(* Substitute the file contents and specify the source and destination *)
let expand_interpolations_in_file_full env ~src ~dst =
  let ic = OpamFilename.open_in_bin src in
  let oc = OpamFilename.open_out_bin dst in
  (* Determine if the input file parses in opam-file-format *)
  let is_opam_format =
    try
      let _ =
        OpamParser.channel ic (OpamFilename.to_string src)
      in
      true
    with e ->
      OpamStd.Exn.fatal e;
      false
  in
  (* Reset the input for processing *)
  seek_in ic 0;
  let default _ = "" in
  let write = output_string oc in
  let unquoted s = write @@ expand_string ~default env s in
  let quoted s = write @@ expand_string_aux ~escape_value ~default env s in
  let process =
    if is_opam_format then
      fun () -> OpamInterpLexer.main unquoted quoted (Lexing.from_channel ic)
    else
      let rec aux () =
        match input_line ic with
        | s -> unquoted s; output_char oc '\n'; aux ()
        | exception End_of_file -> ()
      in
        aux
  in
  process ();
  close_in ic;
  close_out oc

(* Substitute the file contents *)
let expand_interpolations_in_file env file =
  let file = OpamFilename.of_basename file in
  let src = OpamFilename.add_extension file "in" in
  expand_interpolations_in_file_full env ~src ~dst:file

(* Apply filters and interpolations to package commands *)

let arguments env (a,f) =
  if opt_eval_to_bool env f then
    match a with
    | CString s -> [expand_string ~default:(fun _ -> "") env s]
    | CIdent i ->
      let fident = filter_ident_of_string i in
      match resolve_ident_raw env fident with
      | Some (S s) -> [s]
      | Some (B b) -> [string_of_bool b]
      | Some (L sl) -> sl
      | None -> log "ERR in replacement: undefined ident %S" i; [""]
  else
    []

let command env (l, f) =
  if opt_eval_to_bool env f then
    match List.concat (List.map (arguments env) l) with
    | [] -> None
    | l  -> Some l
  else
    None

let commands env l = OpamStd.List.filter_map (command env) l

let single_command env l = List.concat (List.map (arguments env) l)

let simple_arg_variables = function
  | CString s -> string_variables s
  | CIdent i ->
    try fident_variables (filter_ident_of_string i)
    with Failure _ -> []

let filter_opt_variables = function
  | None -> []
  | Some f -> variables f
let argument_variables (a,f) =
  simple_arg_variables a @ filter_opt_variables f
let command_variables (l,f) =
  List.fold_left (fun acc a -> argument_variables a @ acc)
    (filter_opt_variables f) l
let commands_variables l =
  List.fold_left (fun acc c -> command_variables c @ acc) [] l

let rec of_formula atom_f = function
  | Empty -> FBool true
  | Atom at -> atom_f at
  | Block f -> of_formula atom_f f
  | And (a, b) -> FAnd (of_formula atom_f a, of_formula atom_f b)
  | Or (a, b) -> FOr (of_formula atom_f a, of_formula atom_f b)

let filter_constraints ?default_version ?default env filtered_constraint =
  OpamFormula.partial_eval
    (function
      | Filter flt ->
        if eval_to_bool ?default env flt then `True else `False
      | Constraint (relop, v) ->
        try
          let v = eval_to_string env v in
          `Formula (Atom (relop, OpamPackage.Version.of_string v))
        with Failure msg -> match default_version with
          | None ->
            log "Warn: ignoring version constraint %a: %s"
              (slog to_string) v msg;
            `Formula (Empty)
          | Some v -> `Formula (Atom (relop, v)))
    filtered_constraint

(* { build & "%{skromuk}%" = "flib%" } *)
(* { build & "flib%%" = "flib%" } *)

let partial_filter_constraints env filtered_constraint =
  OpamFormula.partial_eval
    (function
      | Filter flt ->
        (match partial_eval env flt with
         | FBool true -> `True
         | FBool false -> `False
         | FUndef f | f -> `Formula (Atom (Filter f)))
      | Constraint (relop, flt_v) ->
        (match partial_eval env flt_v with
         | FBool b ->
           `Formula (Atom (Constraint (relop, FString (string_of_bool b))))
         | FUndef f | f ->
           `Formula (Atom (Constraint (relop, f)))))
    filtered_constraint

let gen_filter_formula constraints filtered_formula =
  OpamFormula.map
    (fun (name, fc) -> match constraints fc with
       | `True -> Atom (name, Empty)
       | `False -> Empty
       | `Formula c -> Atom (name, c))
    filtered_formula

let filter_formula ?default_version ?default env ff =
  gen_filter_formula
    (filter_constraints ?default_version ?default env) ff

let partial_filter_formula env ff =
  gen_filter_formula (partial_filter_constraints env) ff

let string_of_filtered_formula =
  let string_of_constraint =
    OpamFormula.string_of_formula (function
        | Constraint (op, FString s) ->
          Printf.sprintf "%s \"%s\"" (OpamPrinter.relop_kind op) s
        | Constraint (op, (FIdent _ as v)) ->
          Printf.sprintf "%s %s" (OpamPrinter.relop_kind op) (to_string v)
        | Constraint (op, v) ->
          Printf.sprintf "%s (%s)" (OpamPrinter.relop_kind op) (to_string v)
        | Filter f -> to_string f)
  in
  OpamFormula.string_of_formula (function
      | n, Empty -> OpamPackage.Name.to_string n
      | n, c ->
        let paren = match c with Atom (Constraint _) -> false | _ -> true in
        Printf.sprintf "%s %s%s%s"
          (OpamPackage.Name.to_string n)
          (if paren then "{" else "")
          (string_of_constraint c)
          (if paren then "}" else ""))

let variables_of_filtered_formula ff =
  OpamFormula.fold_left
    (fun acc (_, f) ->
       OpamFormula.fold_left (fun acc -> function
           | Constraint _ -> acc
           | Filter f -> variables f @ acc)
         acc f)
    [] ff

let deps_var_env ~build ~post ?test ?doc ?dev_setup ?dev var =
  let get_opt = function
    | Some b -> Some (B b)
    | None -> invalid_arg "filter_deps"
  in
  match OpamVariable.Full.to_string var with
  | "build" -> Some (B build)
  | "post" -> Some (B post)
  | "with-test" -> get_opt test
  | "with-doc" -> get_opt doc
  | "with-dev-setup" -> get_opt dev_setup
  | "dev" -> get_opt dev
  | _ -> None

let filter_deps
    ~build ~post ?test ?doc ?dev_setup ?dev ?default_version ?default
    deps =
  filter_formula ?default_version ?default
    (deps_var_env ~build ~post ?test ?doc ?dev_setup ?dev) deps

let rec simplify_extended_version_formula ef =
  let to_pure ef =
    try
      Some (OpamFormula.map (function
          | Constraint (op, FString s) when string_variables s = [] ->
            Atom (op, OpamPackage.Version.of_string s)
          | _ -> failwith "Impure")
          ef)
    with Failure _ -> None
  in
  let to_filtered =
    OpamFormula.map (fun (op, v) ->
        Atom (Constraint (op, FString (OpamPackage.Version.to_string v))))
  in
  match to_pure ef with
  | Some f ->
    OpamStd.Option.map to_filtered (OpamFormula.simplify_version_formula f)
  | None -> match ef with
    | And _ | Or _ ->
      let conj = match ef with And _ -> true | _ -> false in
      let l = OpamFormula.(if conj then ands_to_list else ors_to_list) ef in
      (try
         let filtered, pure =
           List.fold_left (fun (filtered, pure) ef1 ->
               match to_pure ef1 with
               | Some f -> filtered, f::pure
               | None ->
                 let ef1 = simplify_extended_version_formula ef1 in
                 match ef1 with
                 | None -> (* Always false *)
                   if conj then failwith "false" else filtered, pure
                 | Some ef1 ->
                   (match to_pure ef1 with
                    | Some f -> filtered, f::pure
                    | None -> ef1::filtered, pure))
             ([], []) l
         in
         let mk = OpamFormula.(if conj then ands else ors) in
         match OpamFormula.simplify_version_formula (mk pure) with
         | None -> if conj then None else Some (mk (List.rev filtered))
         | Some pure ->
           Some (mk (List.rev_append filtered [to_filtered pure]))
       with Failure _ -> None)
    | Block ef -> simplify_extended_version_formula ef
    | atom -> Some atom

let atomise_extended =
  OpamFormula.map (fun (x, c) ->
      match c with
      | Empty -> Atom (x, (FBool true, None))
      | cs ->
        let rec aux filters = function
          | Atom (Filter f) -> Atom (x, (FAnd (f,filters), None))
          | Atom (Constraint c) -> Atom (x, (filters, Some c))
          | Empty ->
            (match filters with FBool true -> Empty | f -> Atom (x, (f, None)))
          | Block f -> aux filters f
          | And _ as f ->
            let filters, constraints =
              let rec split filters conj = function
                | Atom (Filter f) :: r -> split (FAnd (f,filters)) conj r
                | cstr :: r -> split filters (cstr::conj) r
                | [] -> filters, conj
              in
              split filters [] (OpamFormula.ands_to_list f)
            in
            OpamFormula.ands (List.rev_map (aux filters) constraints)
          | Or (a, b) -> Or (aux filters a, aux filters b)
        in
        aux (FBool true) cs)

let sort_filtered_formula compare ff =
  let f = OpamFormula.sort compare ff in
  let rec vc_sort = function
    | Empty -> Empty
    | Atom (n,vf) ->
      Atom (n, (OpamStd.Option.default vf
                  (simplify_extended_version_formula vf)))
    | Block f -> Block (vc_sort f)
    | And (f,f') -> And (vc_sort f, vc_sort f')
    | Or (f,f') -> Or (vc_sort f, vc_sort f')
  in
  vc_sort f

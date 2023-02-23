(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

open OpamParserTypes.FullPos
open OpamTypes
open OpamTypesBase
open OpamStd.Op
open OpamPp
open OpamPp.Op

let item_pos (i: opamfile_item) = i.pos

let value_pos (v: value) = v.pos

let values_pos = function
  | [] -> None
  | x::_ -> Some (value_pos x)

let optelem = OpamStd.Option.map (fun x -> x.pelem)
let nullposelem = OpamStd.Option.map nullify_pos

(** low-level Pps for the Lines parser ([string list list]) *)

type lines = string list list

let lines_set ~empty ~add ~fold pp1 =
  pp
    ~name:(Printf.sprintf "(%s) lines" pp1.ppname)
    (fun ~pos lines ->
       List.fold_left (fun (i,acc) -> function
           | [] -> i + 1, acc
           | line ->
             (* XXX POSCHECK *)
             let pos = { pos with start = i,0 ; stop = i,0 } in
             i + 1,
             add (parse pp1 ~pos line) acc)
         (1, empty) lines
       |> snd)
    (fun x ->
       List.rev (fold (fun v acc -> print pp1 v::acc) x []))

let lines_map ~empty ~add ~fold pp1 =
  pp
    ~name:(Printf.sprintf "(%s) lines" pp1.ppname)
    (fun ~pos lines ->
       List.fold_left (fun (i,acc) -> function
           | [] -> i + 1, acc
           | line ->
             let pos = { pos with start = i,0 ; stop = i,0 } in
             (* XXX POSCHECK *)
             let k,v = parse pp1 ~pos line in
             i + 1, add k v acc)
         (1, empty) lines
       |> snd)
    (fun x ->
       List.rev (fold (fun k v acc -> print pp1 (k,v)::acc) x []))

(*
  let list2 pp1 pp2 =
    pp ~name:(Printf.sprintf "%s %s" pp1.ppname pp2.ppname)
      (function [a; b] -> parse pp1 a, parse pp2 b
              | _ -> unexpected ())
      (fun (x,y) -> [print pp1 x; print pp2 y])
*)

(** All Pps dealing with the [value] type *)
module V = struct

  (** Low-level Pps *)

  let bool =
    pp ~name:"bool"
      (fun ~pos:_ v -> match v.pelem with Bool b -> b | _ -> unexpected ())
      (fun b -> nullify_pos (Bool b))

  let int =
    pp ~name:"int"
      (fun ~pos:_ v -> match v.pelem with Int i -> i | _ -> unexpected ())
      (fun i -> nullify_pos (Int i))

  let pos_int = int -| check ~name:"positive-int" (fun i -> i >= 0)

  let ident =
    pp ~name:"ident"
      (fun ~pos:_ v -> match v.pelem with Ident i -> i | _ -> unexpected ())
      (fun str -> nullify_pos (Ident str))

  let string =
    pp ~name:"string"
      (fun ~pos:_ v -> match v.pelem with String s -> s | _ -> unexpected ())
      (fun str -> nullify_pos (String str))

  let string_tr = string -| pp (fun ~pos:_ -> OpamStd.String.strip) (fun x -> x)

  let simple_arg =
    pp ~name:"ident-or-string"
      (fun ~pos:_ v ->
         match v.pelem with
         | Ident i -> CIdent i
         | String s -> CString s
         | _ -> unexpected ())
      (function
        | CIdent i -> nullify_pos (Ident i)
        | CString s -> nullify_pos (String s))

  let variable_contents =
    pp ~name:"string-or-stringlist-or-bool"
      (fun ~pos:_ v ->
         match v.pelem with
         | String s -> S s
         | Bool b -> B b
         | List l ->
           L (List.map (fun v ->
               match v.pelem with String s -> s | _ -> unexpected ())
               l.pelem)
         | _ -> unexpected ())
      (function
        | S s -> nullify_pos (String s)
        | B b -> nullify_pos (Bool b)
        | L l ->
          nullify_pos (List (nullify_pos (List.map (fun s ->
              nullify_pos (String s)) l))))

  let list =
    pp ~name:"list" ~name_constr:(Printf.sprintf "[%s]")
      (fun ~pos:_ v ->
         match v.pelem with
         | List l -> l.pelem
         | _ -> [v])
      (fun l -> nullify_pos (List (nullify_pos l)))

  let group =
    pp ~name:"group" ~name_constr:(Printf.sprintf "(%s)")
      (fun ~pos:_ v ->
         match v.pelem with
         | Group l -> l.pelem
         | _ -> [v])
      (fun l -> nullify_pos (Group (nullify_pos l)))

  let option =
    pp ~name:"option"
      (fun ~pos:_ v ->
         match v.pelem with
         | Option (k,l) -> k, l.pelem
         | _ -> v, [])
      (function
        | (v, []) -> v
        | (v, l) -> nullify_pos (Option (v, nullify_pos l)))

  let option_strict =
    pp ~name:"option"
      (fun ~pos v ->
         match v.pelem with
         | Option (k,l) -> k, l.pelem
         | _ -> bad_format ~pos "Expected an option")
      (function (v, l) -> nullify_pos (Option (v, nullify_pos l)))

  let map_group pp1 = group -| map_list ~posf:value_pos pp1

  let list_depth expected_depth =
    let rec depth v =
      match v.pelem with
      | List { pelem = []; _} -> 1
      | List { pelem = v::_; _} -> 1 + depth v
      | Option (v,_) -> depth v
      | _ -> 0
    in
    let rec wrap n v =
      if n <= 0 then v else wrap (n-1) (nullify_pos (List (nullify_pos [v])))
    in
    let rec lift n v =
      if n <= 0 then v else
      match v.pelem with
      | List { pelem = [v]; _} -> lift (n-1) v
      | _ -> v
    in
    pp
      (fun ~pos:_ v -> wrap (expected_depth - depth v) v)
      (fun v -> lift expected_depth v)

  let option_depth expected_depth =
    let rec depth = function
      | { pelem = Option (v,_); _} -> 1 + depth v
      | _ -> 0
    in
    let rec wrap n v =
      if n <= 0 then v else
        wrap (n-1) (nullify_pos (Option (v, nullify_pos [])))
    in
    let rec lift n v =
      if n <= 0 then v else
      match v.pelem with
      | Option (v, {pelem = []; _}) -> lift (n-1) v
      | _ -> v
    in
    pp
      (fun ~pos:_ v -> wrap (expected_depth - depth v) v)
      (fun v -> lift expected_depth v)

  let map_list ?(depth=0) pp1 =
    list_depth depth -|
    pp ~name:(Printf.sprintf "[%s]" pp1.ppname)
      (fun ~pos:_ v ->
         match v.pelem with
         | List l ->
           List.rev @@
           List.rev_map (fun v -> parse pp1 ~pos:(value_pos v) v) l.pelem
         | _ -> unexpected ())
      (fun l ->
         nullify_pos (List (nullify_pos @@
                            List.rev @@ List.rev_map (print pp1) l)))

  let map_option_contents pp1 pp2 =
    map_pair ~name:(Printf.sprintf "%s ?{%s}" pp1.ppname pp2.ppname)
      ~posf1:value_pos
      ~posf2:(fun v -> OpamStd.Option.default pos_null (values_pos v))
      pp1 pp2

  let map_option pp1 pp2 = option -| map_option_contents pp1 pp2

  let map_options_2 pp1 pp2 pp3 =
    option_depth 2 -|
    option_strict -| map_option_contents
      (option_strict -| map_option_contents
         pp1 pp2)
      pp3 -|
    pp (fun ~pos:_ ((a,b),c) -> a,b,c) (fun (a,b,c) -> (a,b),c)

  let map_options_3 pp1 pp2 pp3 pp4 =
    option_depth 3 -|
    option_strict -| map_option_contents
      (option_strict -| map_option_contents
         (option_strict -| map_option_contents
            pp1 pp2)
         pp3)
      pp4 -|
    pp (fun ~pos:_ (((a,b),c),d) -> a,b,c,d) (fun (a,b,c,d) -> ((a,b),c),d)

  let map_pair pp1 pp2 =
    pp ~name:(Printf.sprintf "[%s %s]" pp1.ppname pp2.ppname)
      (fun ~pos:_ v -> match v.pelem with
         | List { pelem = [a; b]; _} ->
           parse pp1 ~pos:(value_pos a) a, parse pp2 ~pos:(value_pos b) b
         | _ -> unexpected ())
      (fun (a, b) ->
         nullify_pos @@ List (nullify_pos @@ [pp1.print a; pp2.print b]))

  let map_triple pp1 pp2 pp3 =
    pp ~name:(Printf.sprintf "[%s %s %s]" pp1.ppname pp2.ppname pp3.ppname)
      (fun ~pos:_ v -> match v.pelem with
         | List { pelem = [a; b; c]; _} ->
           parse pp1 ~pos:(value_pos a) a,
           parse pp2 ~pos:(value_pos b) b,
           parse pp3 ~pos:(value_pos c) c
         | _ -> unexpected ())
      (fun (a, b, c) ->
         nullify_pos @@ List (nullify_pos @@
                              [pp1.print a; pp2.print b; pp3.print c]))

  (** Pps for the [value] type to higher level types *)

  let url = string -| of_module "url" (module OpamUrl)

  let url_with_backend backend =
    string -|
    pp ~name:"url"
      (fun ~pos:_ s -> OpamUrl.parse ~backend ~handle_suffix:false s)
      (fun url -> OpamUrl.to_string url)

  (* a hack to allow "system" compiler as ident rather than string. For
     backwards-compat. Deprecated, for migration only *)
  let compiler_version =
    let system_compiler = "system" in
    let parse ~pos:_ = fun v ->
      match v.pelem with
      | Ident v when v = system_compiler -> v
      | String v -> v
      | _ -> unexpected ()
    in
    let print v =
      if v = system_compiler then print ident v
      else print string v
    in
    pp ~name:"compiler-version" parse print

  let filter_ident =
    ident -|
    pp ~name:"filter-ident"
      (fun ~pos:_ -> filter_ident_of_string)
      string_of_filter_ident

  let filter =
    let rec parse_filter ~pos l =
      let rec aux = fun v ->
        match v.pelem with
        | Bool b -> FBool b
        | String s -> FString s
        | Int i -> FString (string_of_int i)
        | Ident _ -> FIdent (parse ~pos:v.pos filter_ident v)
        | Group g -> parse_filter ~pos:v.pos g.pelem
        | Relop (op,e,f) -> FOp (aux e, op.pelem, aux f)
        | Pfxop ({ pelem = `Not; _}, e) -> FNot (aux e)
        | Pfxop ({ pelem = `Defined; _}, e) -> FDefined (aux e)
        | Logop ({ pelem = `And; _}, e, f)-> FAnd (aux e, aux f)
        | Logop ({ pelem = `Or; _}, e, f)-> FOr (aux e, aux f)
        | _ -> unexpected ()
      in
      match l with
      | [] -> FBool true
      | [{ pelem = Group { pelem = [] | _::_::_ ; _}; _}]
      | _::_::_ ->
        bad_format ~pos "expected a single filter expression"
      | [{ pelem = Group { pelem = [f]; _}; _}] | [f] -> aux f
    in
    let print_filter f =
      let rec aux ?(context=`Or) f =
        let group_if ?(cond=false) f =
          if cond || OpamFormatConfig.(!r.all_parens)
          then nullify_pos @@ Group (nullify_pos [f])
          else f
        in
        match f with
        | FString s  -> print string s
        | FIdent fid -> print filter_ident fid
        | FBool b    -> print bool b
        | FOp (e,s,f) ->
          group_if ~cond:(context <> `Or && context <> `And) @@ nullify_pos @@
          Relop (nullify_pos s, aux ~context:`Relop e, aux ~context:`Relop f)
        | FOr (e,f) ->
          group_if ~cond:(context <> `Or) @@ nullify_pos @@
          Logop (nullify_pos `Or, aux ~context:`Or e, aux ~context:`Or f)
        | FAnd (e,f) ->
          group_if ~cond:(context <> `Or && context <> `And) @@ nullify_pos @@
          Logop (nullify_pos `And, aux ~context:`And e, aux ~context:`And f)
        | FNot f ->
          group_if ~cond:(context = `Relop) @@ nullify_pos @@
          Pfxop (nullify_pos `Not, aux ~context:`Not f)
        | FDefined f ->
          group_if ~cond:(context = `Relop) @@ nullify_pos @@
          Pfxop (nullify_pos `Defined, aux ~context:`Defined f)
        | FUndef _ -> assert false
      in
      match f with
      | FBool true -> []
      | f -> [aux f]
    in
    pp ~name:"filter-expression" parse_filter print_filter

  let arg = map_option simple_arg (opt filter)

  let command = map_option (map_list arg) (opt filter)

  let constraints version =
    let rec parse_constraints ~pos:_ l =
      let rec aux v = match v.pelem with
        | Prefix_relop (op, v) -> Atom (op.pelem, parse version ~pos:v.pos v)
        | Logop ({ pelem = `And; _}, l, r) -> And (aux l, aux r)
        | Logop ({ pelem = `Or; _}, l, r) -> Or (aux l, aux r)
        | Pfxop ({ pelem = `Not; _}, v) ->
          OpamFormula.neg (fun (op, s) ->
              (OpamFormula.neg_relop op, s)) (aux v)
        | Group g -> Block (parse_constraints ~pos:v.pos g.pelem)
        | _ -> unexpected ~pos:(value_pos v) ()
      in
      OpamFormula.ands (List.map aux l)
    in
    let rec print_constraints cs =
      let rec aux ?(in_and=false) cs =
        let group_if ?(cond=false) f =
          if cond || OpamFormatConfig.(!r.all_parens)
          then nullify_pos @@ Group (nullify_pos [f])
          else f
        in
        match cs with
        | Empty       -> assert false
        | Atom (r, v) ->
          group_if @@ nullify_pos @@
          Prefix_relop (nullify_pos r, print version v)
        | And (x, y)  ->
          group_if @@ nullify_pos @@
          Logop (nullify_pos `And, aux ~in_and:true x, aux ~in_and:true y)
        | Or (x, y)   ->
          group_if ~cond:in_and @@ nullify_pos @@
          Logop (nullify_pos `Or, aux x, aux y)
        | Block g     ->
          nullify_pos @@ Group (nullify_pos @@ print_constraints g)
      in
      match cs with
      | Empty -> []
      | cs -> [aux cs]
    in
    pp ~name:(version.ppname ^ "-constraints")
      parse_constraints print_constraints

  let filtered_constraints version =
    let rec parse_cs ~pos:_ items =
      let rec aux_parse v = match v.pelem with
        | Prefix_relop (op, v) ->
          Atom (Constraint (op.pelem, parse version ~pos:v.pos v))
        | Logop ({ pelem = `And; _}, a, b) ->
          OpamFormula.ands [aux_parse a; aux_parse b]
        | Logop ({ pelem = `Or; _}, a, b) ->
          OpamFormula.ors [aux_parse a; aux_parse b]
        | Group g -> OpamFormula.Block (parse_cs ~pos:v.pos g.pelem)
        | Pfxop ({ pelem = `Not; _}, v') ->
          parse_cs ~pos:v.pos [v'] |>
          OpamFormula.neg (function
              | Constraint (op, v) -> Constraint (OpamFormula.neg_relop op, v)
              | Filter f -> Filter (FNot f))
        | _ ->
          Atom (Filter (filter.parse ~pos:(value_pos v) [v]))
      in
      OpamFormula.ands (List.map aux_parse items)
    in
    let rec print_cs cs = (* form -> val list *)
      let rec aux ?(in_and=false) cs =
        let group_if ?(cond=false) f =
          if cond || OpamFormatConfig.(!r.all_parens)
          then nullify_pos @@ Group (nullify_pos [f])
          else f
        in
        match cs with
        | Empty -> assert false
        | And (x, y) ->
          group_if @@ nullify_pos @@
          Logop (nullify_pos `And, aux ~in_and:true x, aux ~in_and:true y)
        | Or (x, y) ->
          group_if ~cond:in_and @@ nullify_pos @@
          Logop (nullify_pos `Or, aux x, aux y)
        | Block g -> nullify_pos @@ Group (nullify_pos @@ print_cs g)
        | Atom (Constraint (op,v)) ->
          group_if @@ nullify_pos @@
          Prefix_relop (nullify_pos op, print version v)
        | Atom (Filter flt) ->
          (match filter.print flt with
           | f1::fr ->
             group_if (List.fold_left (fun a b ->
                 nullify_pos @@ Logop (nullify_pos `And, a, b))
                 f1 fr)
           | [] -> nullify_pos @@ Group (nullify_pos []))
      in
      match cs with
      | Empty -> []
      | cs -> [aux cs]
    in
    pp ~name:"filtered-constraints" parse_cs print_cs

  let version =
    string -| of_module "version" (module OpamPackage.Version)

  let ext_version =
    pp ~name:"version-expr"
      (fun ~pos:_ v -> match v.pelem with
         | String s ->
           let _ =
             try
               OpamPackage.Version.of_string
                 (OpamFilter.expand_string (fun _ -> Some (S "-")) s)
             with Failure msg ->
               bad_format ~pos:v.pos "Invalid version string %S: %s" s msg
           in
           FString s
         | Ident s -> FIdent (filter_ident_of_string s)
         | _ -> unexpected ())
      (function
        | FString s -> nullify_pos (String s)
        | FIdent id -> nullify_pos @@ Ident (string_of_filter_ident id)
        | _ -> assert false)

  let pkgname =
    string -| of_module "pkg-name" (module OpamPackage.Name)

  let package_atom constraints =
    map_option pkgname constraints

  (* These two functions are duplicated from [OpamFormula] but we need to have a
     it here because of a change on [Block] handling: to have a coherent
     printing, we must not always discard them *)
  let rec ands_to_list = function
    | Empty -> []
    | And (e,f) ->
      List.rev_append (rev_ands_to_list e) (ands_to_list f)
    | x -> [x]
  and rev_ands_to_list = function
    | Empty -> []
    | And (e,f) ->
      List.rev_append (ands_to_list f) (rev_ands_to_list e)
    | x -> [x]

  let rec ors_to_list = function
    | Empty -> []
    | Or (e,f)
    | Block (Or (e,f)) ->
      List.rev_append (rev_ors_to_list e) (ors_to_list f)
    | x -> [x]
  and rev_ors_to_list = function
    | Empty -> []
    | Or (e,f)
    | Block (Or (e,f)) ->
      List.rev_append (ors_to_list f) (rev_ors_to_list e)
    | x -> [x]

  let package_formula_items kind constraints =
    let split, join = match kind with
      | `Conj -> ands_to_list, OpamFormula.ands
      | `Disj -> ors_to_list, OpamFormula.ors
    in
    let rec parse_formula ~pos:_ l =
      let rec aux v = match v.pelem with
        | String _ | Option (_,_) ->
          Atom (parse (package_atom constraints) ~pos:v.pos v)
        | Group g -> Block (parse_formula ~pos:v.pos g.pelem)
        | Logop ({ pelem = `Or; _}, e1, e2) ->
          let left = aux e1 in Or (left, aux e2)
        | Logop ({ pelem = `And; _}, e1, e2) ->
          let left = aux e1 in And (left, aux e2)
        | _ -> unexpected ~pos:(value_pos v) ()
      in
      join (List.map aux l)
    in
    let rec print_formula ?(inner=false) f =
      let rec aux ?(in_and=false) f =
        let group_if ?(cond=false) f =
          if cond || OpamFormatConfig.(!r.all_parens)
          then nullify_pos @@ Group (nullify_pos [f])
          else f
        in
        match f with
        | Empty -> assert false
        | Block f ->
          nullify_pos @@ Group (nullify_pos @@ print_formula ~inner:true f)
        | And (e,f) ->
          group_if @@ nullify_pos @@
          Logop (nullify_pos `And, aux ~in_and:true e, aux ~in_and:true f)
        | Or (e,f) ->
          group_if ~cond:in_and @@ nullify_pos @@
          Logop (nullify_pos `Or, aux e, aux f)
        | Atom at -> group_if (print (package_atom constraints) at)
      in
      let fl = if inner then [f] else split f in
      List.map (aux ~in_and:false) fl
    in
    pp ~name:"pkg-formula" parse_formula print_formula

  let package_formula kind constraints =
    list -| package_formula_items kind constraints

  let env_binding =
    let parse ~pos:_ v = match v.pelem with
      | Relop ({ pelem = `Eq;_}, { pelem = Ident i;_}, { pelem = String s;_}) ->
        i, OpamParserTypes.Eq, s, None
      | Env_binding ({ pelem = Ident i; _}, op, { pelem = String s; _}) ->
        i, op.pelem, s, None
      | _ -> unexpected ()
    in
    let print (id, op, str, _) =
      nullify_pos @@
      Env_binding (print ident id, nullify_pos op, print string str)
    in
    list -| singleton -| pp ~name:"env-binding" parse print

  (* Only used by the deprecated "os" field *)
  let os_constraint =
    let rec parse_osc ~pos:_ l =
      let rec aux v = match v.pelem with
        | Group g -> Block (parse_osc ~pos:g.pos g.pelem)
        | String os -> Atom (true, os)
        | Logop ({ pelem = `And; _}, l, r) -> And (aux l, aux r)
        | Logop ({ pelem = `Or; _}, l, r) -> Or (aux l, aux r)
        | Pfxop ({ pelem = `Not; _}, v) ->
          OpamFormula.neg (fun (b, s) -> (not b, s)) (aux v)
        | _ -> unexpected ~pos:(value_pos v) ()
      in
      OpamFormula.ors (List.map aux l)
    in
    let print_osc f =
      let rec aux = function
        | Empty -> assert false
        | Atom (true , os) -> print string os
        | Atom (false, os) ->
          nullify_pos @@ Pfxop (nullify_pos `Not, print string os)
        | Block g -> nullify_pos @@ Group (nullify_pos [aux g])
        | And (e,f) -> nullify_pos @@ Logop (nullify_pos `And, aux e, aux f)
        | Or (e,f) -> nullify_pos @@ Logop (nullify_pos `Or, aux e, aux f)
      in
      match f with
      | Empty -> []
      | f -> [aux f]
    in
    list -| pp ~name:"os-constraint" parse_osc print_osc

end

(** Parsers for item lists (standard opam file contents: list of field
    bindings). *)
module I = struct

  let file =
    pp ~name:"opam-file"
      (fun ~pos:_ file ->
         OpamFilename.of_string file.file_name,
         file.file_contents)
      (fun (file_name, file_contents) ->
         { file_name = OpamFilename.to_string file_name;
           file_contents })

  let map_file pp1 = file -| map_snd pp1

  let item =
    pp ~name:"field-binding"
      (fun ~pos:_ v -> match v.pelem with
         | Section (sec) ->
           bad_format ~pos:v.pos "Unexpected section %s" sec.section_kind.pelem
         | Variable (k,v) -> k.pelem, v)
      (fun (k,v) -> nullify_pos @@ Variable (nullify_pos k, v))

  let items = map_list ~posf:item_pos item

  let anonymous_section pp1 =
    pp ~name:pp1.ppname
      (fun ~pos -> function
         | [ None, contents ] -> pp1.parse ~pos contents
         | [ Some _, _ ] ->
           bad_format ~pos "Unexpected section title"
         | [] ->
           bad_format ~pos "Missing section"
         | _::_::_ ->
           bad_format ~pos "Duplicate section")
      (fun l -> [None, pp1.print l])

  let section kind =
    pp ~name:"file-section"
      (fun ~pos:_ v -> match v.pelem with
         | Section ({section_kind; _} as s) when section_kind.pelem = kind ->
           optelem s.section_name, s.section_items.pelem
         | Section sec ->
           bad_format ~pos:v.pos "Unexpected section %s" sec.section_kind.pelem
         | Variable (k,_) ->
           bad_format ~pos:v.pos "Unexpected field %s" k.pelem)
      (fun (name, items) ->
         nullify_pos @@ Section {
             section_kind = nullify_pos kind;
             section_name = nullposelem name;
             section_items = nullify_pos items })

  type ('a, 'value) fields_def = (string * ('a, 'value) field_parser) list

  let fields ?name ~empty ?(sections=[]) ?(mandatory_fields=[]) ppas =
    let parse ~pos items =
      (* For consistency, always read fields in ppa order, ignoring file
         order. Some parsers may depend on it. *)
      let module SEM = OpamStd.Map.Make(struct
          type t = string * string option
          let compare = compare
          let to_string (s,o) = s ^ OpamStd.Option.to_string ((^) "^") o
          let to_json (s,o) =
            `O (("kind", `String s) ::
                match o with None -> [] | Some s -> ["name", `String s])
          (* these serializers/deserializers are not accessible
             from the OpamFormat.mli interface, so there are not currently
             tested -- it's not clear if usage of the SEM functor touches
             them in any way... *)
          let of_json = function
            | `O dict ->
              begin try
                  match OpamStd.List.assoc String.equal "kind" dict with
                  | `String s ->
                    begin
                      let o =
                        if not (OpamStd.List.mem_assoc String.equal
                                  "name" dict) then None else
                        match OpamStd.List.assoc String.equal "name" dict with
                        | `String s -> Some s
                        | _ -> raise Not_found
                      in Some (s, o)
                    end
                  | _ -> raise Not_found
                with Not_found -> None
              end
            | _ -> None
        end)
      in
      (* XXX trasnform field map into set *)
      let errs, section_map, field_map =
        List.fold_left
          (fun (errs, section_map, field_map) it -> match it.pelem with
             | Section sec ->
               let k = sec.section_kind.pelem in
               let v = sec.section_items.pelem in
               let n = optelem sec.section_name in
               if OpamStd.List.mem_assoc String.equal k sections then
                 try
                   errs,
                   SEM.safe_add (k, n) (pos,v) section_map, field_map
                 with Failure _ ->
                   (k,(Some pos,"Duplicate section "^k)) :: errs,
                   section_map, field_map
               else
                 (k,(Some pos,"Invalid section "^k)) :: errs,
                 section_map, field_map
             | Variable (k, v) ->
               let k = k.pelem in
               let v = v.pelem in
               let pos = it.pos in
               if OpamStd.List.mem_assoc String.equal k ppas then
                 try
                   errs,
                   section_map, OpamStd.String.Map.safe_add k (pos,v) field_map
                 with Failure _ ->
                   (k,(Some pos,"Duplicate field "^k)) :: errs,
                   section_map, field_map
               else
                 (k,(Some pos,"Invalid field "^k))::errs,
                 section_map, field_map)
          ([], SEM.empty, OpamStd.String.Map.empty) items
      in
      let errs, r =
        List.fold_left
          (fun (errs,acc) (field,ppa) ->
             try
               let pos, v = OpamStd.String.Map.find field field_map in
               try errs, parse ppa ~pos (acc, Some { pelem = v; pos }) with
               | Bad_format (pos, msg) ->
                 (field, (pos, msg)) :: errs, acc
             with Not_found ->
               (if List.mem field mandatory_fields
                then (field, (Some pos, "Missing field "^field)) :: errs
                else errs),
               acc)
          (errs, empty) ppas
      in
      let errs, r =
        List.fold_left
          (fun (errs,acc) (section_kind, ppa) ->
             let secs =
               SEM.fold
                 (fun (kind, name) (_, items) acc ->
                    if kind = section_kind then (name, items) :: acc else acc)
                 section_map []
               |> List.rev
             in
             if secs = [] then errs, acc else
             try errs, parse ppa ~pos (acc, Some secs) with
             | Bad_format (pos, msg) ->
               (section_kind,(pos, msg)) :: errs, acc)
          (errs, r) sections
      in
      r, errs
    in
    let print (acc, _) =
      OpamStd.List.filter_map
        (fun (field,ppa) ->
           match snd (ppa.print acc) with
           | None | Some ({ pelem = List { pelem = []; _}; _}
                         | { pelem = Group { pelem = []; _}; _}) -> None
           | Some value ->
             Some (nullify_pos @@ Variable (nullify_pos field, value)))
        ppas
      @
      (List.flatten @@ List.map
         (fun (kind, ppa) ->
            OpamStd.Option.default [] (snd (ppa.print acc)) |>
            List.map (fun (name, items) ->
                nullify_pos @@ Section {
                  section_kind = nullify_pos kind;
                  section_name = nullposelem name;
                  section_items = nullify_pos items }))
         sections)
    in
    pp ?name parse print

  let show_errors ?name
      ?(strict=OpamFormatConfig.(!r.strict))
      ?(condition=fun _ -> true)
      () =
    let parse ~pos (t, errs) =
      let file = pos.filename in
      if errs = [] then t
      else if strict then raise (Bad_format_list (List.rev_map snd errs))
      else
        (if condition t then
           OpamConsole.warning
             "Errors in %s, some fields have been ignored:\n%s"
             file
             (OpamStd.Format.itemize
                (fun e -> OpamPp.string_of_bad_format (Bad_format e))
                (List.rev_map snd errs))
         else
           OpamConsole.log "FORMAT" "File errors in %s, ignored fields: %s"
             file
             (OpamStd.List.concat_map "; "
                (fun e -> OpamPp.string_of_bad_format (Bad_format e))
                (List.rev_map snd errs));
         t)
    in
    let print t = t, [] in
    pp ?name parse print

  let on_errors ?name f =
    let parse ~pos:_ (t, errs) =
      List.fold_left f t errs
    in
    let print t = (t, []) in
    pp ?name parse print

  let partition filter =
    pp
      (fun ~pos:_ -> List.partition filter)
      (fun (a,b) -> a @ b)

  let partition_fields ?(section=false) filter =
    partition @@ fun v -> match v.pelem with
    | Variable (k,_) -> filter k.pelem
    | Section _ -> section

  let field name parse =
    pp
      (fun ~pos items ->
         match
           OpamStd.List.filter_map (fun v -> match v.pelem with
               | Variable (k,v) when k.pelem = name -> Some v
               | _ -> None)
             items
         with
         | [] -> None, items
         | _::_::_ -> bad_format ~pos "Duplicate '%s:' field" name
         | [v] -> Some (parse ~pos v), items)
      snd


  let extract_field name =
    partition_fields ((=) name) -|
    (map_fst @@ opt @@
     singleton -| item -|
     pp ~name:(Printf.sprintf "'%s:' field" name)
       (fun ~pos:_ (_,v) -> v)
       (fun v -> name,v))

  let check_opam_version
      ?(optional=false)
      ~format_version
      ?(f=fun v -> OpamVersion.(compare format_version (nopatch v) >= 0))
      ()
    =
    let name = "opam-version" in
    let opam_v = V.string -| of_module name (module OpamVersion) in
    let f v =
      OpamFormatConfig.(!r.skip_version_checks) || match v with
      | Some v -> f v
      | None -> optional
    in
    let errmsg =
      Printf.sprintf
        "unsupported or missing file format version; should be %s or older"
        (OpamVersion.to_string format_version)
    in
    field name (parse opam_v) -|
    map_fst (check ~name ~raise:OpamPp.bad_version ~errmsg f) -|
    pp ~name
      (fun ~pos:_ (_,x) -> x)
      (fun x ->
         (* re-extract the field using parse when printing, to check that it is
            not undefined *)
         match parse ~pos:pos_null (field name (parse opam_v)) x with
         | None, _ -> failwith "opam version must be printed"
         | v, l -> v, l)

  let opam_version ?(undefined=false) ~format_version () =
    let name = "opam-version" in
    pp
      (fun ~pos:_ items ->
         if not undefined then items else
           List.filter (function
               | { pelem = Variable ({ pelem = fname; _},
                                     { pelem = String _version; _}); _}
                 when fname = name ->
                 (* check opam version already called, we don't need to check
                    that it is the same version *)
                 false
               | _ -> true) items)
      (fun items ->
         let opam_v = V.string -| of_module name (module OpamVersion) in
         match parse ~pos:pos_null (field name (parse opam_v)) items with
         | None, items ->
           let opam_v =
             nullify_pos @@
             Variable (nullify_pos name,
                       nullify_pos @@
                       String (OpamVersion.to_string format_version))
           in
           opam_v :: items
         | Some _, items -> items)

  type signature = string * string * string

  let signature =
    V.list -| (V.string ^+ V.string ^+ last -| V.string) -|
    pp (fun ~pos:_ (a,(b,c)) -> a,b,c) (fun (a,b,c) -> a,(b,c))

  exception Invalid_signature of pos * (string*string*string) list option

  let signed ~check =
    let module OpamPrinter = OpamPrinter.FullPos in
    let pp_sig = V.map_list ~depth:2 signature in
    extract_field "signature" -|
    pp ~name:"signed-file"
      (fun ~pos -> function
         | Some sgs, items ->
           let sgs = parse ~pos pp_sig sgs in
           let str = OpamPrinter.Normalise.items items in
           if not (check sgs str) then
             raise (Invalid_signature (pos, Some sgs))
           else (sgs, items)
         | None, _ ->
           raise (Invalid_signature (pos, None)))
      (fun (sgs, items) ->
         assert (check sgs (OpamPrinter.Normalise.items items));
         Some (print pp_sig sgs),
         items)
end

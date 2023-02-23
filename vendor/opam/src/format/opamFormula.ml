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

module Re = Dune_re

type relop = [`Eq|`Neq|`Geq|`Gt|`Leq|`Lt]

let neg_relop = function
  | `Eq -> `Neq
  | `Neq -> `Eq
  | `Geq -> `Lt
  | `Gt -> `Leq
  | `Leq -> `Gt
  | `Lt -> `Geq

let string_of_relop = OpamPrinter.FullPos.relop_kind

type version_constraint = relop * OpamPackage.Version.t

type atom = OpamPackage.Name.t * version_constraint option

let string_of_atom = function
  | n, None       -> OpamPackage.Name.to_string n
  | n, Some (r,c) ->
    Printf.sprintf "%s (%s %s)"
      (OpamPackage.Name.to_string n)
      (string_of_relop r)
      (OpamPackage.Version.to_string c)

let short_string_of_atom = function
  | n, None       -> OpamPackage.Name.to_string n
  | n, Some (`Eq,c) ->
    Printf.sprintf "%s.%s"
      (OpamPackage.Name.to_string n)
      (OpamPackage.Version.to_string c)
  | n, Some (r,c) ->
    Printf.sprintf "%s%s%s"
      (OpamPackage.Name.to_string n)
      (string_of_relop r)
      (OpamPackage.Version.to_string c)

let string_of_atoms atoms =
  OpamStd.List.concat_map " & " short_string_of_atom atoms

let atom_of_string =
  let re = lazy Re.(compile @@ whole_string @@ seq [
      group @@ rep1 @@ diff any (set ">=<.!");
      group @@ alt [ seq [ set "<>"; opt @@ char '=' ];
                     set "=."; str "!="; ];
      group @@ rep1 any;
    ])
  in
  fun str ->
    try
      let lazy re = re in
      let sub = Re.exec re str in
      let sname = Re.Group.get sub 1 in
      let sop = Re.Group.get sub 2 in
      let sversion = Re.Group.get sub 3 in
      let name = OpamPackage.Name.of_string sname in
      let sop = if sop = "." then "=" else sop in
      let op = OpamLexer.FullPos.relop sop in
      let version = OpamPackage.Version.of_string sversion in
      name, Some (op, version)
    with Not_found | Failure _ | OpamLexer.Error _ ->
      OpamPackage.Name.of_string str, None

type 'a conjunction = 'a list

let string_of_conjunction string_of_atom c =
  Printf.sprintf "(%s)" (OpamStd.List.concat_map " & " string_of_atom c)

type 'a disjunction = 'a list

let string_of_disjunction string_of_atom c =
  Printf.sprintf "(%s)" (OpamStd.List.concat_map " | " string_of_atom c)

type 'a cnf = 'a list list

let string_of_cnf string_of_atom cnf =
  let string_of_clause c =
    let left, right = match c with [_] -> "", "" | _ -> "(", ")" in
    OpamStd.List.concat_map ~left ~right " | " string_of_atom c
  in
  OpamStd.List.concat_map " & " string_of_clause cnf

type 'a dnf = 'a list list

let string_of_dnf string_of_atom cnf =
  let string_of_clause c =
    let left, right = match c with [_] -> "", "" | _ -> "(", ")" in
    OpamStd.List.concat_map ~left ~right " & " string_of_atom c
  in
  OpamStd.List.concat_map " | " string_of_clause cnf

type 'a formula =
  | Empty
  | Atom of 'a
  | Block of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula

let make_and a b = match a, b with
  | Empty, r | r, Empty -> r
  | a, b -> And (a, b)

let make_or a b = match a, b with
  | Empty, r | r, Empty -> r (* we're not assuming Empty is true *)
  | a, b -> Or (a, b)

let string_of_formula string_of_a f =
  let rec aux ?(in_and=false) f =
    let paren_if ?(cond=false) s =
      if cond || OpamFormatConfig.(!r.all_parens)
      then Printf.sprintf "(%s)" s
      else s
    in
    match f with
    | Empty    -> "[]"
    | Atom a   -> paren_if (string_of_a a)
    | Block x  -> Printf.sprintf "(%s)" (aux x)
    | And(x,y) ->
      paren_if
        (Printf.sprintf "%s & %s"
           (aux ~in_and:true x) (aux ~in_and:true y))
    | Or(x,y)  ->
      paren_if ~cond:in_and
        (Printf.sprintf "%s | %s" (aux x) (aux y))
  in
  aux f

let rec map f = function
  | Empty    -> Empty
  | Atom x   -> f x
  | And(x,y) -> make_and (map f x) (map f y)
  | Or(x,y)  -> make_or (map f x) (map f y)
  | Block x  ->
    match map f x with
    | Empty -> Empty
    | x -> Block x

(* Maps top-down *)
let rec map_formula f t =
  let t = f t in
  match t with
  | Block x  -> Block (map_formula f x)
  | And(x,y) -> make_and (map_formula f x) (map_formula f y)
  | Or(x,y)  -> make_or (map_formula f x) (map_formula f y)
  | x -> x

let rec map_up_formula f t =
  let t = match t with
    | Block x -> f (Block (map_up_formula f x))
    | And(x,y) -> f (make_and (map_up_formula f x) (map_up_formula f y))
    | Or(x,y) -> f (make_or (map_up_formula f x) (map_up_formula f y))
    | Atom x -> f (Atom x)
    | Empty -> Empty
  in
  f t

let neg neg_atom =
  map_formula
    (function
      | And(x,y) -> Or(x,y)
      | Or(x,y) -> And(x,y)
      | Atom x -> Atom (neg_atom x)
      | x -> x)

let rec iter f = function
  | Empty    -> ()
  | Atom x   -> f x
  | Block x  -> iter f x
  | And(x,y) -> iter f x; iter f y
  | Or(x,y)  -> iter f x; iter f y

let rec fold_left f i = function
  | Empty    -> i
  | Atom x   -> f i x
  | Block x  -> fold_left f i x
  | And(x,y) -> fold_left f (fold_left f i x) y
  | Or(x,y)  -> fold_left f (fold_left f i x) y

let rec fold_right f i = function
  | Empty    -> i
  | Atom x   -> f i x
  | Block x  -> fold_right f i x
  | And(x,y) -> fold_right f (fold_right f i y) x
  | Or(x,y)  -> fold_right f (fold_right f i y) x

type version_formula = version_constraint formula

type t = (OpamPackage.Name.t * version_formula) formula

let rec compare_formula f x y =
  let rec compare_atom x = function
    | Empty -> 1
    | Atom y -> f x y
    | Block y -> compare_atom x y
    | And (y,z) | Or (y,z) ->
      let r = compare_atom x y in
      if r <> 0 then r else compare_atom x z
  in
  match x, y with
  | Empty, Empty -> 0
  | Empty, _ -> -1
  | _ , Empty -> 1
  | Atom x, Atom y -> f x y
  | Atom x, y -> compare_atom x y
  | x , Atom y -> -1 * (compare_atom y x)
  | Block x, y | x, Block y -> compare_formula f x y
  | (And (x,y) | Or (x,y)) as lhs, ((And (x',y') | Or (x',y')) as rhs) ->
    let l = compare_formula f x x' in
    if l <> 0 then l else
    let r = compare_formula f y y' in
    if r <> 0 then r else
      (match lhs, rhs with
       | And _, And _ | Or _, Or _ -> 0
       | And _, Or _ -> 1
       | Or _, And _ -> -1
       | _ -> assert false)

let compare_relop op1 op2 =
  match op1, op2 with
  | `Lt,`Lt | `Leq,`Leq | `Neq,`Neq | `Eq,`Eq | `Geq,`Geq | `Gt,`Gt -> 0
  | `Lt, _ -> -1
  | _, `Lt -> 1
  | `Leq, _ -> -1
  | _, `Leq -> 1
  | `Neq, _ -> -1
  | _, `Neq -> 1
  | `Eq, _ -> -1
  | _, `Eq -> 1
  | `Geq, _ -> -1
  | _, `Geq -> 1

let compare_version_formula =
  compare_formula (fun (op1,v1) (op2,v2) ->
      let c = compare v1 v2 in
      if c <> 0 then c else
        compare_relop op1 op2)

let compare_nc (n1, c1) (n2, c2) =
  let c = OpamPackage.Name.compare n1 n2 in
  if c <> 0 then c else compare_version_formula c1 c2

let compare = compare_formula compare_nc

let rec eval atom = function
  | Empty    -> true
  | Atom x   -> atom x
  | Block x  -> eval atom x
  | And(x,y) -> eval atom x && eval atom y
  | Or(x,y)  -> eval atom x || eval atom y

let rec partial_eval atom = function
  | Empty -> `Formula Empty
  | Atom x -> atom x
  | And(x,y) ->
    (match partial_eval atom x, partial_eval atom y with
     | `False, _ | _, `False -> `False
     | `True, f | f, `True -> f
     | `Formula x, `Formula y -> `Formula (And (x,y)))
  | Or(x,y) ->
    (match partial_eval atom x, partial_eval atom y with
     | `True, _ | _, `True -> `True
     | `False, f | f, `False -> f
     | `Formula x, `Formula y -> `Formula (Or (x,y)))
  | Block x -> partial_eval atom x

let check_relop relop c = match relop with
  | `Eq  -> c =  0
  | `Neq -> c <> 0
  | `Geq -> c >= 0
  | `Gt  -> c >  0
  | `Leq -> c <= 0
  | `Lt  -> c <  0

let eval_relop relop v1 v2 =
  check_relop relop (OpamPackage.Version.compare v1 v2)

let check_version_formula f v =
  eval (fun (relop, vref) -> eval_relop relop v vref) f

let check (name,cstr) package =
  name = OpamPackage.name package &&
  match cstr with
  | None -> true
  | Some (relop, v) -> eval_relop relop (OpamPackage.version package) v

let packages_of_atoms ?(disj=false) pkgset atoms =
  (* Conjunction for constraints over the same name (unless [disj] is
     specified), but disjunction on the package names *)
  let ffilter = if disj then List.exists else List.for_all in
  let by_name =
    List.fold_left (fun acc (n,_ as atom) ->
        OpamPackage.Name.Map.update n (fun a -> atom::a) [] acc)
      OpamPackage.Name.Map.empty atoms
  in
  OpamPackage.Name.Map.fold (fun name atoms acc ->
      OpamPackage.Set.union acc @@
      OpamPackage.Set.filter
        (fun nv -> ffilter (fun a -> check a nv) atoms)
        (OpamPackage.packages_of_name pkgset name))
    by_name OpamPackage.Set.empty

let satisfies_depends pkgset f =
  eval (fun (name, cstr) ->
      OpamPackage.Set.exists (fun nv -> check_version_formula cstr nv.version)
        (OpamPackage.packages_of_name pkgset name))
    f

let to_string t =
  let string_of_constraint (relop, version) =
    Printf.sprintf "%s %s" (string_of_relop relop)
      (OpamPackage.Version.to_string version) in
  let string_of_pkg = function
    | n, Empty -> OpamPackage.Name.to_string n
    | n, (Atom _ as c) ->
      Printf.sprintf "%s %s"
        (OpamPackage.Name.to_string n)
        (string_of_formula string_of_constraint c)
    | n, c ->
      Printf.sprintf "%s (%s)"
        (OpamPackage.Name.to_string n)
        (string_of_formula string_of_constraint c) in
  string_of_formula string_of_pkg t

(* convert a formula to a CNF *)
let cnf_of_formula t =
  let rec mk_left x y = match y with
    | Block y   -> mk_left x y
    | And (a,b) -> And (mk_left x a, mk_left x b)
    | Empty     -> x
    | _         -> Or (x,y) in
  let rec mk_right x y = match x with
    | Block x   -> mk_right x y
    | And (a,b) -> And (mk_right a y, mk_right b y)
    | Empty     -> y
    | _         -> mk_left x y in
  let rec mk = function
    | Empty     -> Empty
    | Block x   -> mk x
    | Atom x    -> Atom x
    | And (x,y) -> And (mk x, mk y)
    | Or (x,y)  -> mk_right (mk x) (mk y) in
  mk t

(* convert a formula to DNF *)
let dnf_of_formula t =
  let rec mk_left x y = match y with
    | Block y  -> mk_left x y
    | Or (a,b) -> Or (mk_left x a, mk_left x b)
    | _        -> And (x,y) in
  let rec mk_right x y = match x with
    | Block x  -> mk_right x y
    | Or (a,b) -> Or (mk_right a y, mk_right b y)
    | _        -> mk_left x y in
  let rec mk = function
    | Empty     -> Empty
    | Block x   -> mk x
    | Atom x    -> Atom x
    | Or (x,y)  -> Or (mk x, mk y)
    | And (x,y) -> mk_right (mk x) (mk y) in
  mk t

let verifies f nv =
  let name_formula =
    map (fun ((n, _) as a) -> if n = OpamPackage.name nv then Atom a else Empty)
      (dnf_of_formula f)
  in
  name_formula <> Empty &&
  eval (fun (_name, cstr) ->
      check_version_formula cstr (OpamPackage.version nv))
    name_formula

let packages pkgset f =
  let names =
    fold_left (fun acc (name, _) ->
        OpamPackage.Name.Set.add name acc)
      OpamPackage.Name.Set.empty f
  in
  (* dnf allows us to transform the formula into a union of intervals, where
     ignoring atoms for different package names works. *)
  let dnf = dnf_of_formula f in
  OpamPackage.Name.Set.fold (fun name acc ->
      (* Ignore conjunctions where [name] doesn't appear *)
      let name_formula =
        map (fun ((n, _) as a) -> if n = name then Atom a else Empty) dnf
      in
      OpamPackage.Set.union acc @@
      OpamPackage.Set.filter (fun nv ->
          let v = OpamPackage.version nv in
          eval (fun (_name, cstr) -> check_version_formula cstr v)
            name_formula)
        (OpamPackage.packages_of_name pkgset name))
    names OpamPackage.Set.empty

(* Convert a t an atom formula *)
let to_atom_formula (t:t): atom formula =
  map (fun (x, c) -> match c with
      | Empty -> Atom (x, None)
      | cs    -> map (fun c -> Atom (x, Some c)) cs)
    t

(* Convert an atom formula to a t-formula *)
let of_atom_formula (a:atom formula): t =
  let atom (n, v) =
    match v with
    | None       -> Atom (n, Empty)
    | Some (r,v) -> Atom (n, Atom (r,v)) in
  map atom a

let ands l = List.fold_left make_and Empty l

let rec ands_to_list = function
  | Empty -> []
  | And (e,f) ->
    List.rev_append (rev_ands_to_list e) (ands_to_list f)
  | Block f -> ands_to_list f
  | x -> [x]
and rev_ands_to_list = function
  | Empty -> []
  | Block f -> rev_ands_to_list f
  | And (e,f) ->
    List.rev_append (ands_to_list f) (rev_ands_to_list e)
  | x -> [x]

let of_conjunction c =
  of_atom_formula (ands (List.rev_map (fun x -> Atom x) c))

let ors l = List.fold_left make_or Empty l

let rec ors_to_list = function
  | Empty -> []
  | Or (e,f) ->
    List.rev_append (rev_ors_to_list e) (ors_to_list f)
  | Block f -> ors_to_list f
  | x -> [x]
and rev_ors_to_list = function
  | Empty -> []
  | Or (e,f) ->
    List.rev_append (ors_to_list f) (rev_ors_to_list e)
  | Block f -> rev_ors_to_list f
  | x -> [x]

let is_conjunction t =
  let rec aux = function
    | Or _ -> false
    | And (a,b) -> aux a && aux b
    | Block a -> aux a
    | _ -> true
  in
  aux t

let is_disjunction t =
  let rec aux = function
    | And _ -> false
    | Or (a,b) -> aux a && aux b
    | Block a -> aux a
    | _ -> true
  in
  aux t

let rec sort comp f=
  match f with
  | (Empty | Atom _) as f -> f
  | Block f -> Block (sort comp f)
  | And _ as f ->
    ands_to_list f
    |> List.rev_map (sort comp)
    |> List.sort (compare_formula comp)
    |> ands
  | Or _ as f ->
    ors_to_list f
    |> List.rev_map (sort comp)
    |> List.sort (compare_formula comp)
    |> ors

let atoms t =
  fold_right (fun accu x -> x::accu) [] (to_atom_formula t)

let formula_to_cnf t =
  let atoms = fold_right (fun acc a -> a::acc) [] in
  let conj = rev_ands_to_list t in
  if List.for_all is_disjunction conj then
    List.rev_map atoms conj (* this gives a nice speedup *)
  else
    List.rev_map atoms @@ rev_ands_to_list @@ cnf_of_formula t

let to_cnf t = formula_to_cnf @@ to_atom_formula t

let formula_to_dnf t =
  let disj = rev_ors_to_list t in
  let atoms = fold_right (fun acc a -> a::acc) [] in
  if List.for_all is_conjunction disj then
    List.rev_map atoms disj
  else
    List.rev_map atoms @@ rev_ors_to_list @@ dnf_of_formula t

let to_dnf t = formula_to_dnf @@ to_atom_formula t

let to_conjunction t =
  if is_conjunction t then atoms t
  else failwith (Printf.sprintf "%s is not a valid conjunction" (to_string t))

let to_disjunction t =
  if is_disjunction t then atoms t
  else failwith (Printf.sprintf "%s is not a valid disjunction" (to_string t))

let of_disjunction d =
  of_atom_formula (ors (List.rev_map (fun x -> Atom x) d))

let get_disjunction_formula version_set cstr =
  (* rev_ors_to_list cstr |>
   * List.fold_left *)

  List.rev_map (fun ff ->
      match ands_to_list ff with
      | [] -> assert false
      | [Atom _] as at -> at
      | _ ->
        OpamPackage.Version.Set.filter (check_version_formula ff) version_set |>
        OpamPackage.Version.Set.elements |>
        List.map (fun v -> Atom (`Eq, v)))
    (rev_ors_to_list cstr) |>
  List.flatten

let set_to_disjunction set t =
  List.map (function
      | And _ ->
        failwith (Printf.sprintf "%s is not a valid disjunction" (to_string t))
      | Or _ | Block _ | Empty -> assert false
      | Atom (name, Empty) -> [name, None]
      | Atom (name, Atom a) -> [name, Some a]
      | Atom (name, cstr) ->
        get_disjunction_formula
          (OpamPackage.versions_of_name set name)
          cstr |>
        List.map (function
            | Atom (relop, v) -> name, Some (relop, v)
            | _ -> assert false))
    (ors_to_list t) |>
  List.flatten

let simplify_ineq_formula vcomp f =
  let vals = fold_left (fun acc (_op, x) -> x::acc) [] f in
  let vals = List.sort_uniq vcomp vals in
  let vals_a = Array.of_list vals in
  let val_of_int i = vals_a.(i/2) in
  let int_of_val =
    let m = List.mapi (fun i v -> v, 2 * i + 1) vals in
    fun v -> OpamStd.List.assoc (fun v v' -> vcomp v v' = 0) v m
  in
  (* One integer for each value appearing in f, plus one for each interval *)
  let rec mk_ranges acc n = if n < 0 then acc else mk_ranges (n::acc) (n-1) in
  let ranges = mk_ranges [] (2 * Array.length vals_a + 2) in
  let int_formula = map (fun (op, x) -> Atom (op, int_of_val x)) f in
  let vals =
    List.map (fun i ->
        eval (fun (relop, iref) -> check_relop relop (i - iref)) int_formula,
        i)
      ranges
  in
  if List.for_all (fun (t, _) -> not t) vals then None else
  let rec aux = function
    | (true,  _) :: ((true,  _) :: _ as r) -> aux r
    | (false, _) :: ((false, _) :: _ as r) -> aux r
    | (true, _) :: (false, x) :: ((true, _) :: _ as r) when x mod 2 = 1 ->
      (`Neq, x) :: aux r
    | (false, _) :: (true, x) :: ((false, _) :: _ as r) when x mod 2 = 1 ->
      (`Eq, x) :: aux r
    | (true, _) :: ((false, x) :: _ as r) ->
      (if x mod 2 = 1 then `Lt, x else `Leq, x-1) :: aux r
    | (false, _) :: ((true, x) :: _ as r) ->
      (if x mod 2 = 1 then `Geq, x else `Gt, x-1) :: aux r
    | [_] | []-> []
  in
  let rec aux2 = function
    | (`Geq|`Gt|`Neq as op, i) :: r ->
      let rec find_upper acc = function
        | (`Leq|`Lt as op,  i) :: r ->
          ands (List.rev_append acc [Atom (op, val_of_int i)]) :: aux2 r
        | (`Neq, i) :: r ->
          find_upper (Atom (`Neq, val_of_int i) :: acc) r
        | r -> ands (List.rev acc) :: aux2 r
      in
      find_upper [Atom (op, val_of_int i)] r
    | (op, i) :: r -> Atom (op, val_of_int i) :: aux2 r
    | [] -> [Empty]
  in
  Some (ors (aux2 (aux vals)))

let simplify_version_formula f =
  simplify_ineq_formula OpamPackage.Version.compare f

(** Takes an ordered list of atoms and a predicate, returns a formula describing
    the subset of matching atoms *)
let gen_formula l f =
  let l = List.map (fun x -> f x, x) l in
  let rec aux (t, x as bound) l = match t, l with
    | true,  (false, y) :: (true,  _) :: r
    | false, (true, y)  :: (false, _) :: r ->
      let a = (if t then `Neq else `Eq), y in
      (match aux bound r with
       | b :: r -> b :: a :: r
       | r -> a :: r)
    | true,  (true,  _) :: r
    | false, (false, _) :: r ->
      aux bound r
    | true,  (false, _ as bound') :: r
    | false, (true,  _ as bound') :: r ->
      ((if t then `Geq else `Lt), x) :: aux bound' r
    | _, [] -> [(if t then `Geq else `Lt), x]
  in
  let rec aux2 = function
    | (`Geq|`Neq), _ as a :: r ->
      let rec find_upper acc = function
        | `Lt,  _ as a :: r ->
          ands (List.rev_append acc [Atom a]) :: aux2 r
        | `Neq, _ as a :: r ->
          find_upper (Atom a :: acc) r
        | r -> ands (List.rev acc) :: aux2 r
      in
      find_upper [Atom a] r
    | a :: r -> Atom a :: aux2 r
    | [] -> [Empty]
  in
  match l with
  | [] -> Some Empty
  | (t, x) :: r ->
    match aux (t, x) r with
    | [] -> assert false
    | [`Geq, _] -> Some Empty
    | [`Lt, _] -> None
    | _ :: r -> Some (ors (aux2 r))

let formula_of_version_set set subset =
  let module S = OpamPackage.Version.Set in
  match
    gen_formula (S.elements set) (fun x -> S.mem x subset)
  with
  | Some f -> f
  | None -> invalid_arg "Empty subset"

let simplify_version_set set f =
  let module S = OpamPackage.Version.Set in
  if S.is_empty set then Empty else
  let set = fold_left (fun set (_relop, v) -> S.add v set) set f in
  gen_formula (S.elements set) (check_version_formula f) |>
  OpamStd.Option.default f

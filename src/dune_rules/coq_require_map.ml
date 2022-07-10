open Import
module Path = Coq_module.Path
module Name = Coq_module.Name

type 'a node =
  | Leaf of 'a
  | Tree of 'a node Name.Map.t
  | Tree' of 'a * 'a node Name.Map.t

(* a.v a/b.v *)

type 'a t = 'a node Name.Map.t

let rec union x y =
  Name.Map.union x y ~f:(fun _ x y ->
      match (x, y) with
      | (Leaf _ | Tree' _), (Leaf _ | Tree' _) ->
        User_error.raise [ Pp.textf "TODO conflict" ]
      | Leaf l, Tree t | Tree t, Leaf l -> Some (Tree' (l, t))
      | Tree t, Tree' (l, t') | Tree' (l, t'), Tree t ->
        Some (Tree' (l, union t t'))
      | Tree t', Tree t -> Some (Tree (union t t')))

(* Invariant Leaf and Tree are mutually exclusive the combination of both is
   Tree' *)

let empty = Name.Map.empty

let singleton (type a) path (x : a) : a node =
  List.fold_right path ~init:(Leaf x) ~f:(fun a acc ->
      Tree (Name.Map.singleton a acc))

let rec add : 'a. 'a t -> Name.t list -> 'a -> 'a t =
 fun t path x ->
  match path with
  | [] -> assert false
  | [ p ] -> (
    match Name.Map.find t p with
    | None -> Name.Map.set t p (Leaf x)
    | Some (Tree y) -> Name.Map.set t p (Tree' (x, y))
    | Some (Leaf _) | Some (Tree' (_, _)) -> failwith "override")
  | p :: (_ :: _ as ps) -> (
    match Name.Map.find t p with
    | None -> Name.Map.add_exn t p (singleton ps x)
    | Some (Leaf x) ->
      let v = Tree' (x, t) in
      Name.Map.set t p v
    | Some (Tree m) ->
      let v = add m ps x in
      Name.Map.set t p (Tree v)
    | Some (Tree' (y, m)) ->
      let v = Tree' (y, add m ps x) in
      Name.Map.set t p v)

let add t path a = add t (Path.to_list path) a

let of_modules modules =
  List.fold_left modules ~init:empty ~f:(fun acc m ->
      add acc (Path.rev (Coq_module.path m)) m)

let rec fold t ~init ~f =
  Name.Map.fold t ~init ~f:(fun a init ->
      match a with
      | Leaf a -> f a init
      | Tree' (x, ts) ->
        let init = f x init in
        fold ts ~init ~f
      | Tree t -> fold t ~init ~f)

let find_all t ~prefix ~suffix =
  let prefix = Path.to_list prefix in
  let suffix = Path.to_list suffix in
  let rec check_prefix m m_prefix prefix =
    match (m_prefix, prefix) with
    | _, [] -> true
    | [], [ x ] -> Coq_module.Name.equal x (Coq_module.name m)
    | x :: m_prefix, y :: prefix ->
      Coq_module.Name.equal x y && check_prefix m m_prefix prefix
    | _, _ -> false
  in
  let add acc m =
    if check_prefix m (Path.to_list (Coq_module.path m)) prefix then m :: acc
    else acc
  in
  let rec loop acc (t : Coq_module.t t) path =
    match path with
    | [] -> fold t ~init:acc ~f:(fun x y -> add y x)
    | p :: ps -> (
      match Name.Map.find t p with
      | None -> acc
      | Some (Leaf s) -> (
        match ps with
        | [] -> add acc s
        | _ :: _ -> acc)
      | Some (Tree t) -> loop acc t ps
      | Some (Tree' (s, t)) ->
        let acc =
          match ps with
          | [] -> add acc s
          | _ :: _ -> acc
        in
        loop acc t ps)
  in
  loop [] t (List.rev suffix)

let rec dyn_of_node node : Dyn.t =
  match node with
  | Leaf x -> Dyn.variant "Leaf" [ Coq_module.to_dyn x ]
  | Tree m -> Dyn.variant "Tree" [ Name.Map.to_dyn dyn_of_node m ]
  | Tree' (x, m) ->
    Dyn.variant "Tree'"
      [ Tuple [ Coq_module.to_dyn x; Name.Map.to_dyn dyn_of_node m ] ]

let to_dyn t : Dyn.t = Name.Map.to_dyn dyn_of_node t

let rec t_equal ~equal t1 t2 = Name.Map.equal ~equal:(node_equal ~equal) t1 t2

and node_equal ~equal x y =
  match (x, y) with
  | Leaf x, Leaf y -> equal x y
  | Tree m, Tree n -> t_equal ~equal m n
  | Tree' (x, m), Tree' (y, n) -> equal x y && t_equal ~equal m n
  | _, _ -> false

let equal t1 t2 = t_equal ~equal:Coq_module.equal t1 t2

let merge_all = function
  | [] -> empty
  | init :: xs -> List.fold_left ~init ~f:union xs

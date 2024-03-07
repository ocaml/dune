open! Import
module Map = Module_name.Map

type key = Module_name.Path.t

type 'a t = 'a node Map.t

and 'a node =
  | Leaf of 'a
  | Map of 'a t

let empty = Map.empty

let mapi =
  let rec loop t f acc =
    Map.mapi t ~f:(fun name node ->
      let path = name :: acc in
      match node with
      | Leaf a -> Leaf (f (List.rev path) a)
      | Map m -> Map (loop m f path))
  in
  fun t ~f -> loop t f []
;;

let map t ~f = mapi t ~f:(fun _key m -> f m)
let of_map t : _ t = Map.map t ~f:(fun v -> Leaf v)

let rec find t = function
  | [] -> None
  | p :: ps ->
    (match Map.find t p with
     | None -> None
     | Some (Leaf a) -> Option.some_if (List.is_empty ps) a
     | Some (Map t) -> find t ps)
;;

let rec gen_set_k t ps v ~on_exists =
  match ps with
  | [] ->
    (match v with
     | Leaf _ -> Code_error.raise "gen_set: no top level leaf" []
     | Map m -> m)
  | p :: ps ->
    Map.update t p ~f:(fun x ->
      if List.is_empty ps
      then (
        match x with
        | None -> Some v
        | Some c -> on_exists c)
      else (
        match x with
        | None -> None
        | Some (Leaf _ as leaf) -> Some leaf
        | Some (Map m) -> Some (Map (gen_set_k m ps v ~on_exists))))
;;

let gen_set (type a) (t : a t) ps v =
  let exception Duplicate of a node in
  match gen_set_k t ps v ~on_exists:(fun v -> raise_notrace (Duplicate v)) with
  | s -> Ok s
  | exception Duplicate d -> Error d
;;

let set t path v = gen_set_k t path (Leaf v) ~on_exists:(fun _ -> Some (Leaf v))
let set_map t k v = gen_set t k (Map (of_map v))
let non_empty_map m = if Map.is_empty m then None else Some (Map m)

let rec filter_map t ~f =
  Map.filter_map t ~f:(function
    | Map m -> non_empty_map (filter_map m ~f)
    | Leaf a ->
      (match f a with
       | None -> None
       | Some a -> Some (Leaf a)))
;;

let rec remove t = function
  | [] -> t
  | p :: ps ->
    Map.update t p ~f:(fun x ->
      if List.is_empty ps
      then None
      else (
        match x with
        | None -> None
        | Some (Leaf _ as leaf) -> Some leaf
        | Some (Map m) -> non_empty_map (remove m ps)))
;;

let mem t p = Option.is_some (find t p)

let foldi t ~init ~f =
  let rec loop acc path t =
    Map.foldi ~init:acc t ~f:(fun k v acc ->
      match v with
      | Leaf s -> f (List.rev (k :: path)) s acc
      | Map t -> loop acc (k :: path) t)
  in
  loop init [] t
;;

let fold t ~init ~f = foldi t ~init ~f:(fun _key -> f)

let rec to_dyn f t =
  Map.to_dyn
    (function
      | Leaf a -> f a
      | Map a -> to_dyn f a)
    t
;;

let merge x y ~f =
  let rec base ~path ~f t =
    Map.foldi t ~init:empty ~f:(fun k v acc ->
      let path = k :: path in
      let rev_path = List.rev path in
      match v with
      | Leaf leaf ->
        (match f rev_path leaf with
         | None -> acc
         | Some leaf -> Map.add_exn acc k (Leaf leaf))
      | Map m -> Map.add_exn acc k (Map (base ~path ~f m)))
  in
  let rec loop path x y =
    match x, y with
    | None, None -> assert false
    | Some x, None -> base x ~path ~f:(fun path x -> f path (Some x) None)
    | None, Some y -> base y ~path ~f:(fun path x -> f path None (Some x))
    | Some x, Some y ->
      Map.merge x y ~f:(fun (name : Module_name.t) x y ->
        let path = name :: path in
        let rev_path = List.rev path in
        let leaf l r =
          match f rev_path l r with
          | None -> None
          | Some x -> Some (Leaf x)
        in
        match x, y with
        | None, None -> assert false
        (* leaves *)
        | None, Some (Leaf y) -> leaf None (Some y)
        | Some (Leaf x), None -> leaf (Some x) None
        | Some (Leaf x), Some (Leaf y) -> leaf (Some x) (Some y)
        (* maps *)
        | None, Some (Map v) ->
          non_empty_map (base v ~path ~f:(fun path x -> f path None (Some x)))
        | Some (Map v), None ->
          non_empty_map (base v ~path ~f:(fun path x -> f path (Some x) None))
        | Some (Map x), Some (Map y) -> non_empty_map (loop path (Some x) (Some y))
        (* mixed *)
        | Some (Leaf _), Some (Map y) -> non_empty_map (loop path None (Some y))
        | Some (Map x), Some (Leaf _) -> non_empty_map (loop path (Some x) None))
  in
  loop [] (Some x) (Some y)
;;

let singleton path v = set empty path v

let as_singleton t =
  match
    fold t ~init:None ~f:(fun v acc ->
      match acc with
      | None -> Some v
      | Some _ -> raise_notrace Exit)
  with
  | None | (exception Exit) -> None
  | Some v -> Some v
;;

let to_map t =
  Module_name.Map.map t ~f:(function
    | Leaf v -> v
    | Map _ -> assert false)
;;

let toplevel_only (t : _ t) =
  Module_name.Map.filter_map t ~f:(function
    | Leaf v -> Some v
    | Map _ -> None)
;;

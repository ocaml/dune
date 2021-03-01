type 'a binding =
  | Bound of { value : 'a ; mutable valid : bool }
  | Unbound

type 'a tree =
  | Leaf
  | Node of {
      mutable version : int;
      mutable left    : 'a tree;
      mutable binding : 'a binding;
      mutable right   : 'a tree;
      mutable parent  : 'a tree;
      mutable size    : int;
    }
  | Root of {
      mutable version : int;
      mutable child : 'a tree;
      mutable generation : unit ref;
      mutable on_invalidate : Obj.t Lwd.prim list;
    }

type 'a t = 'a tree
type 'a row = 'a tree

let not_origin = ref () (* not {!origin} *)
let origin = ref () (* not {!not_origin} *)

let make () =
  Root { child = Leaf; generation = origin; version = 0; on_invalidate = [] }

let set_parent ~parent = function
  | Root _ -> assert false
  | Node n -> n.parent <- parent
  | Leaf -> ()

let reparent ~parent ~oldchild ~newchild =
  match parent with
  | Root r ->
    assert (r.child == oldchild);
    r.child <- newchild
  | Node n when n.left == oldchild ->
    n.left <- newchild
  | Node n when n.right == oldchild ->
    n.right <- newchild
  | Leaf | Node _ -> assert false

let make_node ?set ~left ~right ~parent =
  let binding = match set with
    | None -> Unbound
    | Some value -> Bound { value ; valid = true }
  in
  let node = Node { left; right; parent; version = 0; size = 0; binding } in
  set_parent left  ~parent:node;
  set_parent right ~parent:node;
  node

let rec raw_invalidate = function
  | Node { size = 0; _ } -> ()
  | Node t ->
    t.size <- 0;
    raw_invalidate t.parent
  | Root r ->
    List.iter Lwd.invalidate r.on_invalidate
  | Leaf -> assert false

let prepend ?set = function
  | Root r as parent ->
    raw_invalidate parent;
    let node = make_node ?set ~left:Leaf ~right:r.child ~parent in
    r.child <- node;
    node
  | Leaf | Node _ -> assert false

let prepend' x set = ignore (prepend x ~set)

let append ?set = function
  | Root r as parent ->
    raw_invalidate parent;
    let node = make_node ?set ~left:r.child ~right:Leaf ~parent in
    r.child <- node;
    node
  | Leaf | Node _ -> assert false

let append' x set = ignore (append x ~set)

let before ?set = function
  | Node { parent = Leaf ; _ } | Leaf -> Leaf
  | Node n as parent ->
    raw_invalidate parent;
    let node = make_node ?set ~left:n.left ~right:Leaf ~parent in
    n.left <- node;
    node
  | Root _ -> assert false

let after ?set = function
  | Node { parent = Leaf ; _ } | Leaf -> Leaf
  | Node n as parent ->
    raw_invalidate parent;
    let node = make_node ?set ~left:Leaf ~right:n.right ~parent in
    n.right <- node;
    node
  | Root _ -> assert false

let get = function
  | Node { binding = Bound { value ; _ } ; _ } -> Some value
  | Leaf | Root _ | Node { binding = Unbound ; _ } -> None

let invalidate_binding = function
  | Unbound -> ()
  | Bound b -> b.valid <- false

let set_binding x = function
  | Root _ -> assert false
  | Leaf | Node { parent = Leaf; _ } -> ()
  | Node n as t ->
    raw_invalidate t;
    invalidate_binding n.binding;
    n.binding <- x

let set t value = set_binding (Bound { value; valid = true }) t
let unset t = set_binding Unbound t

let is_bound = function
  | Leaf | Node { parent = Leaf; _ } -> false
  | Root _ | Node _ -> true

let rec join left = function
  | Root _ | Leaf -> assert false
  | Node ({ left = Leaf; _ } as n) as self ->
    n.left <- left;
    set_parent left ~parent:self;
    raw_invalidate self
  | Node node ->
    join left node.left

let join left = function
  | Leaf -> left
  | right -> join left right; right

let remove = function
  | Root _ | Leaf | Node {parent = Leaf; _} -> ()
  | Node ({left; right; parent; _} as n) as t ->
    invalidate_binding n.binding;
    n.left <- Leaf;
    n.right <- Leaf;
    n.parent <- Leaf;
    n.binding <- Unbound;
    n.version <- max_int;
    raw_invalidate parent;
    let join = join left right in
    reparent ~parent ~oldchild:t ~newchild:join;
    set_parent join ~parent

let rec clear = function
  | Leaf -> ()
  | Node ({left; right; _} as n) ->
    invalidate_binding n.binding;
    n.left <- Leaf;
    n.right <- Leaf;
    n.parent <- Leaf;
    n.binding <- Unbound;
    n.version <- max_int;
    clear left;
    clear right
  | Root r as root ->
    let child = r.child in
    r.child <- Leaf;
    clear child;
    raw_invalidate root

(* Tree balancing *)

let size = function
  | Node node ->
    assert (node.size <> 0);
    node.size
  | Leaf -> 0
  | Root _ -> assert false

(** [smaller_ell smin smax] iff
    - [smin] is less than [smax]
    - [smin] and [smax] differs by less than two magnitude orders, i.e
      msbs(smin) >= msbs(smax) - 1
      where msbs is the index of the most significant bit set *)
let smaller_ell smin smax = (smin < smax) && ((smin land smax) lsl 1 < smax)

(** [disbalanced smin smax] check if two sub-trees of size [smin] and [smax],
    are disbalanczed. That is, msbs(smin) < msbs(smax) - 1 *)
let disbalanced smin smax = smaller_ell smin (smax lsr 1)

let reparent ~parent ~oldchild ~newchild =
  match parent with
  | Root r ->
    assert (r.child == oldchild);
    r.child <- newchild;
  | Node n when n.left == oldchild ->
    n.left <- newchild
  | Node n when n.right == oldchild ->
    n.right <- newchild
  | Leaf | Node _ -> assert false

let rec rot_left version = function
  | Node ({ right = (Node rn) as r; _} as sn) as s ->
    let parent = sn.parent in
    let rl = match rn.left with
      | Root _ -> assert false
      | Leaf -> Leaf
      | (Node rln) as rl ->
        rln.parent <- s;
        rl
    in
    rn.left <- s;
    sn.right <- rl;
    sn.parent <- r;
    rn.parent <- parent;
    reparent ~parent ~oldchild:s ~newchild:r;
    ignore (balance version s);
    balance version r
  | _ -> assert false

and rot_right version = function
  | Node ({ left = (Node ln) as l; _} as sn) as s ->
    let parent = sn.parent in
    let lr = match ln.right with
      | Root _ -> assert false
      | Leaf -> Leaf
      | (Node lrn) as lr ->
        lrn.parent <- s;
        lr
    in
    ln.right <- s;
    sn.left <- lr;
    sn.parent <- l;
    ln.parent <- parent;
    reparent ~parent ~oldchild:s ~newchild:l;
    ignore (balance version s);
    balance version l
  | _ -> assert false

and inc_left version = function
  | Root _ | Leaf -> assert false
  | Node {right; _} as self ->
    begin match right with
      | Node rn when smaller_ell (size rn.right) (size rn.left) ->
        ignore (rot_right version right)
      | _ -> ()
    end;
    rot_left version self

and inc_right version = function
  | Root _ | Leaf -> assert false
  | Node {left; _} as self ->
    begin match left with
      | Node ln when smaller_ell (size ln.left) (size ln.right) ->
        ignore (rot_left version left)
      | _ -> ()
    end;
    rot_right version self

and balance version = function
  | Root _ | Leaf -> assert false
  | Node node as self ->
    let sl = size node.left and sr = size node.right in
    if sl < sr then (
      if disbalanced sl sr
      then inc_left version self
      else (node.version <- version; node.size <- 1 + sl + sr; self)
    ) else (
      if disbalanced sr sl
      then inc_right version self
      else (node.version <- version; node.size <- 1 + sl + sr; self)
    )

let rec _compute_sub_size1 version = function
  | Root _ -> ()
  | Leaf -> ()
  | Node node as self ->
    if node.size = 0 then begin
      _compute_sub_size1 version node.left;
      _compute_sub_size1 version node.right;
      ignore (balance version self)
    end

let compute_sub_size = _compute_sub_size1

let rec reset_version version = function
  | Leaf -> ()
  | Node n ->
    n.version <- version;
    reset_version version n.left;
    reset_version version n.right
  | Root _ -> assert false

let rebalance = function
  | Root r ->
    begin match r.child with
      | Node { size = 0; _ } ->
        let version = succ r.version in
        let version =
          if version = max_int then (
            r.generation <- ref ();
            reset_version 0 r.child;
            0
          )
          else version
        in
        r.version <- version;
        compute_sub_size version r.child;
        version
      | Node _ | Leaf -> r.version
      | Root _ -> assert false
    end
  | _ -> assert false

let plus (zero, plus) x y =
  if x == zero then y
  else if y == zero then x
  else plus x y

type ('a, 'b) reduction_tree =
  | Red_leaf
  | Red_node of {
      cell: 'a row;
      binding: 'a binding;
      reduction: 'b;
      aggregate: 'b;
      left : ('a, 'b) reduction_tree;
      right : ('a, 'b) reduction_tree;
    }

type ('a, 'b) reduction = {
  mutable version: int;
  source: 'a tree;
  mutable result : ('a, 'b) reduction_tree;
  mutable generation: unit ref;
  mapper: 'a row -> 'a -> 'b;
  monoid: 'b Lwd_utils.monoid;
}


let extract_bindings tree =
  let rec aux acc = function
    | Red_leaf -> acc
    | Red_node rnode ->
      let acc = aux acc rnode.right in
      let acc = match rnode.binding with
        | Unbound -> acc
        | Bound { valid = false; _ } -> acc
        | _ -> (rnode.binding, rnode.reduction) :: acc
      in
      aux acc rnode.left
  in
  aux [] tree

let full_rebuild red tree =
  let bindings = ref (extract_bindings red.result) in
  let rec aux = function
    | Node node as cell ->
      let left = aux node.left in
      let reduction =
        match node.binding, !bindings with
        | Unbound, _ -> fst red.monoid
        | binding, ((binding', reduction) :: bindings')
          when binding == binding' ->
          bindings := bindings';
          reduction
        | Bound b, _ -> assert b.valid; red.mapper cell b.value
      in
      let right = aux node.right in
      let aggregate = match left with
        | Red_leaf -> reduction
        | Red_node r -> plus red.monoid r.aggregate reduction
      in
      let aggregate = match right with
        | Red_leaf -> aggregate
        | Red_node r -> plus red.monoid aggregate r.aggregate
      in
      Red_node {
        cell;
        binding = node.binding;
        reduction;
        aggregate;
        left;
        right;
      }
    | Leaf -> Red_leaf
    | Root _ -> assert false
  in
  let result = aux tree in
  assert (!bindings = []);
  result

let extract_fringe version tree =
  let rec aux acc = function
    | Red_leaf -> acc
    | Red_node rnode as tree ->
      match rnode.cell with
      | Node node when node.version <= version -> tree :: acc
      | _ ->
        let acc = aux acc rnode.right in
        let acc = match rnode.binding with
          | Unbound -> acc
          | Bound { valid = false; _ } -> acc
          | _ -> tree :: acc
        in
        aux acc rnode.left
  in
  aux [] tree

let incremental_rebuild red version tree =
  let fringe = ref (extract_fringe version red.result) in
  let rec aux = function
    | Node node as cell when node.version <= version ->
      begin match !fringe with
        | (Red_node rnode as reduction) :: fringe' ->
          assert (rnode.cell == cell);
          fringe := fringe';
          reduction
        | _ -> assert false
      end
    | Node node as cell ->
      let left = aux node.left in
      let reduction =
        match node.binding, !fringe with
        | Unbound, _ -> fst red.monoid
        | binding, (Red_node rnode :: fringe')
          when binding == rnode.binding ->
          fringe := fringe';
          rnode.reduction
        | Bound b, _ ->
          assert b.valid; red.mapper cell b.value
      in
      let right = aux node.right in
      let aggregate = match left with
        | Red_leaf -> reduction
        | Red_node r -> plus red.monoid r.aggregate reduction
      in
      let aggregate = match right with
        | Red_leaf -> aggregate
        | Red_node r -> plus red.monoid aggregate r.aggregate
      in
      Red_node {
        cell;
        binding = node.binding;
        reduction;
        aggregate;
        left;
        right;
      }
    | Root _ | Leaf -> Red_leaf
  in
  let result = aux tree in
  assert (!fringe = []);
  result

let eval red =
  match red.source with
  | Leaf | Node _ -> assert false
  | Root root ->
    let version = rebalance red.source in
    if true then (
      if red.generation != root.generation then (
        red.generation <- root.generation;
        red.result <- full_rebuild red root.child;
      ) else (
        red.result <- incremental_rebuild red red.version root.child
      );
    ) else (
      red.result <- full_rebuild red root.child;
    );
    red.version <- version;
    match red.result with
    | Red_leaf -> fst red.monoid
    | Red_node r -> r.aggregate

let opaque : 'a Lwd.prim -> Obj.t Lwd.prim = Obj.magic

let map_reduce mapper monoid source =
  let reduction = {
    source; mapper; monoid;
    result = Red_leaf;
    generation = not_origin;
    version = 0;
  } in
  let prim = Lwd.prim
      ~acquire:(fun self ->
          match reduction.source with
          | Leaf | Node _ -> assert false
          | Root root ->
            root.on_invalidate <- opaque self :: root.on_invalidate;
            reduction
        )
      ~release:(fun self reduction ->
          match reduction.source with
          | Leaf | Node _ -> assert false
          | Root root ->
            root.on_invalidate <-
              List.filter ((!=) (opaque self)) root.on_invalidate
        )
  in
  Lwd.map ~f:eval (Lwd.get_prim prim)

let reduce monoid source = map_reduce (fun _ x -> x) monoid source

let rec iter f = function
  | Leaf -> ()
  | Node t ->
    iter f t.left;
    begin match t.binding with
      | Bound x -> f x.value
      | Unbound -> ()
    end;
    iter f t.right
  | Root t ->
    iter f t.child

let rec left_most : 'a row -> 'a row option = function
  | Root _ -> assert false
  | Leaf -> None
  | Node n as self ->
    match left_most n.left with
    | Some _ as x -> x
    | None -> Some self

let rec right_most : 'a row -> 'a row option = function
  | Root _ -> assert false
  | Leaf -> None
  | Node n as self ->
    match right_most n.right with
    | Some _ as x -> x
    | None -> Some self

let first : 'a t -> 'a row option = function
  | Leaf | Node _ -> assert false
  | Root root -> left_most root.child

let last : 'a t -> 'a row option = function
  | Leaf | Node _ -> assert false
  | Root root -> right_most root.child

let next : 'a row -> 'a row option = function
  | Root _ -> assert false
  | Leaf -> None
  | Node n as self ->
    match left_most n.right with
    | Some _ as x -> x
    | None ->
      let rec walk_root self = function
        | Leaf -> assert false
        | Root _ -> None
        | Node n' as parent ->
          if n'.left == self then Some parent else (
            assert (n'.right == self);
            walk_root parent n'.parent
          )
      in
      walk_root self n.parent

let prev : 'a row -> 'a row option = function
  | Root _ -> assert false
  | Leaf -> None
  | Node n as self ->
    match right_most n.left with
    | Some _ as x -> x
    | None ->
      let rec walk_root self = function
        | Leaf -> assert false
        | Root _ -> None
        | Node n' as parent ->
          if n'.right == self then Some parent else (
            assert (n'.left == self);
            walk_root parent n'.parent
          )
      in
      walk_root self n.parent

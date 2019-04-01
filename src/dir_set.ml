open! Stdune

module T : sig

  type t =
    | Empty
    | Universal
    | Nontrivial of nontrivial
  and
    nontrivial = private {
    here : bool;
    children : children;
  }
  and
    children = private {
    default : bool;
    exceptions : t String.Map.t;
  }

  val create : here:bool -> children:children -> t
  val create_children : default:bool -> exceptions:t String.Map.t -> children

  val is_empty : t -> bool
  val is_universal : t -> bool

end = struct

  type t =
    | Empty
    | Universal
    | Nontrivial of nontrivial
  and
    nontrivial = {
    here : bool;
    children : children;
  }
  and
    children = {
    default : bool;
    exceptions : t String.Map.t;
  }

  let is_universal ~here ~children =
    String.Map.is_empty children.exceptions && here && children.default

  let is_empty ~here ~children =
    String.Map.is_empty children.exceptions
    && not here && not children.default

  let create ~here ~children =
    if is_empty ~here ~children then Empty
    else
    if is_universal ~here ~children then Universal
    else
      Nontrivial { here; children }

  let is_empty = function
    | Empty -> true
    | _ -> false

  let is_universal = function
    | Universal -> true
    | _ -> false

  let is_trivial ~value t = match value with
    | false -> is_empty t
    | true -> is_universal t

  let create_children ~default ~exceptions =
    { default;
      exceptions =
        String.Map.filter exceptions
          ~f:(fun v -> not (is_trivial ~value:default v));
    }

end

include T

let empty = Empty
let universal = Universal

let empty_children =
  create_children ~default:false ~exceptions:String.Map.empty
let universal_children =
  create_children ~default:true ~exceptions:String.Map.empty

let trivial v = match v with
  | false -> empty
  | true -> universal

module Children = struct

  type set = T.t

  type t = T.children = private {
    default : bool;
    exceptions : T.t String.Map.t;
  }

  let exceptions t = t.exceptions
  let default t = t.default

  let create = create_children

end

let rec union x y =
  match x, y with
  | Empty, _ -> y
  | _, Empty -> x
  | Universal, _ | _, Universal -> universal
  | Nontrivial x, Nontrivial y ->
    create
      ~here:(x.here || y.here)
      ~children:(union_children x.children y.children)
and
  union_children x y =
  create_children
    ~default:(x.default || y.default)
    ~exceptions:(
      String.Map.merge
        x.exceptions
        y.exceptions
        ~f:(fun _key vx vy ->
          let vx = Option.value vx ~default:(trivial x.default) in
          let vy = Option.value vy ~default:(trivial y.default) in
          Some (union vx vy)))

let rec intersect x y =
  match x, y with
  | Universal, _ -> y
  | _, Universal -> x
  | Empty, _ | _, Empty -> empty
  | Nontrivial x, Nontrivial y ->
    create
      ~here:(x.here && y.here)
      ~children:(intersect_children x.children y.children)
and
  intersect_children x y =
  create_children
    ~default:(x.default && y.default)
    ~exceptions:(
      String.Map.merge
        x.exceptions
        y.exceptions
        ~f:(fun _key vx vy ->
          let vx = Option.value vx ~default:(trivial x.default) in
          let vy = Option.value vy ~default:(trivial y.default) in
          Some (intersect vx vy)))

let rec negate x =
  match x with
  | Universal -> empty
  | Empty -> universal
  | Nontrivial { here; children } ->
    create ~here:(not here)
      ~children:(
        Children.create
          ~default:(not children.default)
          ~exceptions:(String.Map.map children.exceptions ~f:(negate)))

let here = function
  | Empty -> false
  | Universal -> true
  | Nontrivial t -> t.here

let children = function
  | Empty -> empty_children
  | Universal -> universal_children
  | Nontrivial t -> t.children

let rec mem t dir = match dir with
  | [] -> here t
  | child :: rest ->
    let children = children t in
    match String.Map.find children.exceptions child with
    | None -> children.default
    | Some t ->
      mem t rest

let mem t dir = mem t (Path.Build.explode dir)

let descend t child =
  let children = children t in
  match String.Map.find children.exceptions child with
  | None -> trivial children.default
  | Some t -> t

let union_all = List.fold_left ~init:empty ~f:union

let of_subtree_gen subtree =
  let rec loop = function
    | [] -> subtree
    | component :: rest ->
      create ~here:false
        ~children:(
          Children.create ~default:false
            ~exceptions:(String.Map.singleton component (loop rest)))
  in
  fun path -> loop (Path.Build.explode path)

let just_the_root =
  create
    ~here:true
    ~children:(Children.create ~default:false ~exceptions:String.Map.empty)

let of_subtrees paths =
  List.map paths ~f:(of_subtree_gen universal)
  |> union_all

let of_individual_dirs paths =
  List.map paths ~f:(of_subtree_gen just_the_root)
  |> union_all

type element =
  | One_dir of Path.Build.t
  | Subtree of Path.Build.t

let of_list list =
  List.map list ~f:(function
    | One_dir dir -> of_subtree_gen just_the_root dir
    | Subtree dir -> of_subtree_gen universal dir)
  |> union_all

let is_subset x ~of_ =
  is_empty (intersect x (negate of_))

let rec to_sexp t = match t with
  | Empty -> Sexp.Atom "Empty"
  | Universal -> Sexp.Atom "Universal"
  | Nontrivial t ->
    Sexp.List (
      (
        (match t.here with | true -> [ ".", Sexp.Atom "true" ] | false -> []) @
        (String.Map.to_list t.children.exceptions
         |> List.map ~f:(fun (s, t) ->
           s, to_sexp t)) @
        (match t.children.default with
         | false -> []
         | true -> [("*", Sexp.Atom "Universal")]))
      |> List.map ~f:(fun (k, v) -> Sexp.List [Sexp.Atom k; v]))

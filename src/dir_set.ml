open! Stdune

type t =
  | Empty
  | Universal
  | Nontrivial of nontrivial
and nontrivial = {
  default : bool;
  here : bool;
  exceptions : t String.Map.t;
}

let here = function
  | Empty -> false
  | Universal -> true
  | Nontrivial t -> t.here

let default = function
  | Empty -> false
  | Universal -> true
  | Nontrivial t -> t.default

let exceptions = function
  | Empty | Universal -> String.Map.empty
  | Nontrivial t -> t.exceptions

let empty = Empty
let universal = Universal

let trivial = function
  | false -> Empty
  | true -> Universal

let create ~default ~here ~exceptions =
  if String.Map.is_empty exceptions && here = default then
    trivial default
  else
    Nontrivial { here; default; exceptions }

let is_empty = function
  | Empty -> true
  | _ -> false

let is_universal = function
  | Universal -> true
  | _ -> false

let merge_exceptions a b ~default ~f =
  String.Map.merge a.exceptions b.exceptions ~f:(fun _ x y ->
    let x = Option.value x ~default:(trivial a.default) in
    let y = Option.value y ~default:(trivial b.default) in
    match default, f x y with
    | false, Empty | true, Universal -> None
    | _, res -> Some res)

let merge_nontrivial a b ~f_one ~f_set =
  let default = f_one a.default b.default in
  create
    ~here:(f_one a.here b.here)
    ~default
    ~exceptions:(merge_exceptions a b ~default ~f:f_set)
[@@inline always]

let rec union x y =
  match x, y with
  | Empty, v | v, Empty -> v
  | Universal, _ | _, Universal -> Universal
  | Nontrivial x, Nontrivial y ->
    merge_nontrivial x y ~f_one:(||) ~f_set:union

let rec inter x y =
  match x, y with
  | Universal, v | v, Universal -> v
  | Empty, _ | _, Empty -> Empty
  | Nontrivial x, Nontrivial y ->
    merge_nontrivial x y ~f_one:(&&) ~f_set:inter

let rec negate x =
  match x with
  | Universal -> Empty
  | Empty -> Universal
  | Nontrivial { here; default; exceptions } ->
    Nontrivial { here = not here
               ; default = not default
               ; exceptions = String.Map.map exceptions ~f:negate
               }

let rec diff x y =
  match x with
  | Empty -> Empty
  | Universal -> negate y
  | Nontrivial nx ->
    match y with
    | Empty -> x
    | Universal -> Empty
    | Nontrivial ny ->
      merge_nontrivial nx ny
        ~f_one:(fun a b -> a && not b)
        ~f_set:diff

let rec mem t dir =
  match t with
  | Empty -> false
  | Universal -> true
  | Nontrivial { here; default; exceptions } ->
    match dir with
    | [] -> here
    | child :: rest ->
      match String.Map.find exceptions child with
      | None -> default
      | Some t -> mem t rest

let mem t dir = mem t (Path.Build.explode dir)

let descend t child =
  match t with
  | Empty -> Empty
  | Universal -> Universal
  | Nontrivial { here = _; default; exceptions } ->
    match String.Map.find exceptions child with
    | None -> trivial default
    | Some t -> t

let union_all = List.fold_left ~init:empty ~f:union
let inter_all = List.fold_left ~init:empty ~f:inter

let of_subtree_gen =
  let rec loop subtree = function
    | [] -> subtree
    | component :: rest ->
      Nontrivial
        { here = false
        ; default = false
        ; exceptions = String.Map.singleton component (loop subtree rest)
        }
  in
  fun subtree path -> loop subtree (Path.Build.explode path)

let just_the_root =
  Nontrivial
    { here = true
    ; default = false
    ; exceptions = String.Map.empty
    }

let subtree p = of_subtree_gen universal p
let singleton p = of_subtree_gen just_the_root p

let is_subset =
  let not_subset () = raise_notrace Exit in
  let rec loop x y =
    match x, y with
    | _, Universal | Empty, _ -> Empty
    | Universal, _ | _, Empty -> not_subset ()
    | Nontrivial x, Nontrivial y ->
      if (x.here    && not y.here   ) ||
         (x.default && not y.default) then
        not_subset ();
      ignore
        (merge_exceptions x y ~default:false ~f:loop : t String.Map.t);
      Empty
  in
  fun x ~of_ ->
    match loop x of_ with
    | (_ : t) -> true
    | exception Exit -> false

let rec to_sexp t = match t with
  | Empty -> Sexp.Atom "Empty"
  | Universal -> Sexp.Atom "Universal"
  | Nontrivial { here; default; exceptions } ->
    Sexp.List (
      (
        (match here with | true -> [ ".", Sexp.Atom "true" ] | false -> []) @
        (String.Map.to_list exceptions
         |> List.map ~f:(fun (s, t) ->
           s, to_sexp t)) @
        (match default with
         | false -> []
         | true -> [("*", Sexp.Atom "Universal")]))
      |> List.map ~f:(fun (k, v) -> Sexp.List [Sexp.Atom k; v]))

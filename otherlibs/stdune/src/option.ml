type 'a t = 'a option =
  | None
  | Some of 'a

module O = struct
  let ( >>| ) t f =
    match t with
    | None -> None
    | Some a -> Some (f a)
  ;;

  let ( >>= ) t f =
    match t with
    | None -> None
    | Some a -> f a
  ;;

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >>| )
end

let map t ~f = O.( >>| ) t f
let bind t ~f = O.( >>= ) t f

let iter t ~f =
  match t with
  | None -> ()
  | Some x -> f x
;;

let forall t ~f =
  match t with
  | None -> true
  | Some t -> f t
;;

let value t ~default =
  match t with
  | Some x -> x
  | None -> default
;;

let value_exn = function
  | Some x -> x
  | None -> Code_error.raise "Option.value_exn" []
;;

let some x = Some x
let some_if cond x = if cond then Some x else None

let is_some = function
  | None -> false
  | Some _ -> true
;;

let is_none = function
  | None -> true
  | Some _ -> false
;;

let both x y =
  match x, y with
  | Some x, Some y -> Some (x, y)
  | _ -> None
;;

let split = function
  | Some (x, y) -> Some x, Some y
  | None -> None, None
;;

let to_list = function
  | None -> []
  | Some x -> [ x ]
;;

let equal eq x y =
  match x, y with
  | None, None -> true
  | Some _, None -> false
  | None, Some _ -> false
  | Some sx, Some sy -> eq sx sy
;;

let compare cmp x y =
  match x, y with
  | None, None -> Ordering.Eq
  | Some _, None -> Gt
  | None, Some _ -> Lt
  | Some x, Some y -> cmp x y
;;

let try_with f =
  match f () with
  | exception _ -> None
  | s -> Some s
;;

module List = struct
  let all =
    let rec loop acc = function
      | [] -> Some (List.rev acc)
      | None :: _ -> None
      | Some x :: xs -> loop (x :: acc) xs
    in
    fun xs -> loop [] xs
  ;;

  let traverse xs ~f =
    let rec loop acc = function
      | [] -> Some (List.rev acc)
      | x :: xs ->
        (match f x with
         | None -> None
         | Some x -> loop (x :: acc) xs)
    in
    loop [] xs
  ;;
end

let hash f = function
  | None -> Stdlib.Hashtbl.hash None
  | Some s -> Stdlib.Hashtbl.hash (f s)
;;

let merge x y ~f =
  match x, y with
  | None, res -> res
  | res, None -> res
  | Some x, Some y -> Some (f x y)
;;

let first_some x y =
  match x with
  | None -> y
  | Some _ -> x
;;

module Unboxed = struct
  type 'a t = Obj.t

  let some x = Obj.repr x

  (* [Sys.opaque_identity] is a paranoid thing to protect against potential compiler
     optimisations looking through the [none] and seeing the type.

     The "memory corruption" issue discussed in [option_array.ml] served as an
     inspiration, but we don't know if it's really necessary. (Empirically, it's not.)
  *)
  let none = Obj.repr (Sys.opaque_identity (-1))

  (* CR-someday amokhov: Let's expose [phys_equal] somewhere from Stdune? *)
  let phys_equal = Stdlib.( == )
  let is_none t = phys_equal t none
  let is_some t = not (phys_equal t none)

  let value_exn t =
    if is_none t then Code_error.raise "Option.Unboxed.value_exn called on None" [];
    Obj.obj t
  ;;

  let to_option t = if is_none t then None else Some (value_exn t)
  let iter t ~f = if is_none t then () else f (value_exn t)
  let match_ t ~none ~some = if is_none t then none () else some (value_exn t)

  let to_dyn f x =
    if is_none x then Dyn.variant "None" [] else Dyn.variant "Some" [ f (value_exn x) ]
  ;;
end

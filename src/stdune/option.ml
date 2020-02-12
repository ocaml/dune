type 'a t = 'a option =
  | None
  | Some of 'a

module O = struct
  let ( >>| ) t f =
    match t with
    | None -> None
    | Some a -> Some (f a)

  let ( >>= ) t f =
    match t with
    | None -> None
    | Some a -> f a

  let ( let* ) = ( >>= )

  let ( let+ ) = ( >>| )
end

let map t ~f = O.( >>| ) t f

let bind t ~f = O.( >>= ) t f

let iter t ~f =
  match t with
  | None -> ()
  | Some x -> f x

let forall t ~f =
  match t with
  | None -> true
  | Some t -> f t

let value t ~default =
  match t with
  | Some x -> x
  | None -> default

let value_exn = function
  | Some x -> x
  | None -> Code_error.raise "Option.value_exn" []

let some x = Some x

let some_if cond x =
  if cond then
    Some x
  else
    None

let is_some = function
  | None -> false
  | Some _ -> true

let is_none = function
  | None -> true
  | Some _ -> false

let both x y =
  match (x, y) with
  | Some x, Some y -> Some (x, y)
  | _ -> None

let split = function
  | Some (x, y) -> (Some x, Some y)
  | None -> (None, None)

let to_list = function
  | None -> []
  | Some x -> [ x ]

let equal eq x y =
  match (x, y) with
  | None, None -> true
  | Some _, None -> false
  | None, Some _ -> false
  | Some sx, Some sy -> eq sx sy

let compare cmp x y =
  match (x, y) with
  | None, None -> Ordering.Eq
  | Some _, None -> Gt
  | None, Some _ -> Lt
  | Some x, Some y -> cmp x y

let try_with f =
  match f () with
  | exception _ -> None
  | s -> Some s

module List = struct
  let all =
    let rec loop acc = function
      | [] -> Some (List.rev acc)
      | None :: _ -> None
      | Some x :: xs -> loop (x :: acc) xs
    in
    fun xs -> loop [] xs
end

let hash f = function
  | None -> Stdlib.Hashtbl.hash None
  | Some s -> Stdlib.Hashtbl.hash (f s)

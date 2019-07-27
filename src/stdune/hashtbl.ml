module type S = Hashtbl_intf.S

include struct
  [@@@warning "-32"]

  let find_opt t key =
    match MoreLabels.Hashtbl.find t key with
    | x -> Some x
    | exception Not_found -> None

  let find_exn t key = Option.value_exn (find_opt t key)
end

module Make(H : sig
    include Hashable.S
    val to_dyn : t -> Dyn.t
  end) = struct
  include MoreLabels.Hashtbl.Make(H)

  include struct
    [@@@warning "-32"]

    let find_opt t key =
      match find t key with
      | x -> Some x
      | exception Not_found -> None
  end

  include struct
    let find = find_opt
    let find_exn t key =
      match find_opt t key with
      | Some v -> v
      | None ->
        Code_error.raise "Hashtbl.find_exn"
          [ "key", H.to_dyn key
          ]

    let set t key data = add t ~key ~data

    let find_or_add t key ~f =
      match find t key with
      | Some x -> x
      | None ->
        let x = f key in
        set t key x;
        x

    let foldi t ~init ~f =
      fold t ~init ~f:(fun ~key ~data acc -> f key data acc)
    let fold  t ~init ~f = foldi t ~init ~f:(fun _ x -> f x)
  end

  let of_list l =
    let h = create (List.length l) in
    let rec loop = function
      | [] -> Result.Ok h
      | (k, v) :: xs ->
        begin match find h k with
        | None -> set h k v; loop xs
        | Some v' -> Error (k, v', v)
        end
    in
    loop l

  let of_list_exn l =
    match of_list l with
    | Result.Ok h -> h
    | Error (key, _, _) ->
      Code_error.raise "Hashtbl.of_list_exn duplicate keys"
        ["key", H.to_dyn key]

  let add_exn t key data =
    match find t key with
    | None -> set t key data
    | Some _ ->
      Code_error.raise "Hastbl.add_exn: key already exists"
        ["key", H.to_dyn key]

  let add t key data =
    match find t key with
    | None -> set t key data; Result.Ok ()
    | Some p -> Result.Error p

  let keys t = foldi t ~init:[] ~f:(fun key _ acc -> key :: acc)

  let to_dyn f t =
    Dyn.Map (
      foldi t ~init:[] ~f:(fun key data acc ->
        (H.to_dyn key, f data) :: acc)
    )
end

open MoreLabels.Hashtbl

type nonrec ('a, 'b) t = ('a, 'b) t

let hash = hash
let create = create
let replace = replace
let length = length
let remove = remove
let mem = mem
let reset = reset

let find = find_opt

let set t key data = add t ~key ~data

let find_or_add t key ~f =
  match find t key with
  | Some x -> x
  | None ->
    let x = f key in
    set t key x;
    x

let add_exn t key data =
  match find t key with
  | None -> set t key data
  | Some _ -> Code_error.raise "Hastbl.add_exn: key already exists" []

let add t key data =
  match find t key with
  | None -> set t key data; Result.Ok ()
  | Some p -> Error p

let foldi t ~init ~f = fold  t ~init ~f:(fun ~key ~data acc -> f key data acc)
let fold  t ~init ~f = foldi t ~init ~f:(fun _ x -> f x)

let iter t ~f = iter ~f t

let keys t = foldi t ~init:[] ~f:(fun key _ acc -> key :: acc)

let to_dyn (type key) f g t =
  let module M =
    Map.Make(struct
      type t = key
      let compare a b = Ordering.of_int (compare a b)
      let to_dyn = f
    end)
  in
  let m =
    foldi t ~init:M.empty ~f:(fun key data acc -> M.set acc key data)
  in
  M.to_dyn g m

module type S = Hashtbl_intf.S

include struct
  [@@@warning "-32"]

  let find_opt t key =
    match MoreLabels.Hashtbl.find t key with
    | x -> Some x
    | exception Not_found -> None
end

module Make(H : Hashable.S) = struct
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
    let add t key data = add t ~key ~data

    let find_or_add t key ~f =
      match find t key with
      | Some x -> x
      | None ->
        let x = f key in
        add t key x;
        x

    let foldi t ~init ~f =
      fold t ~init ~f:(fun ~key ~data acc -> f key data acc)
    let fold  t ~init ~f = foldi t ~init ~f:(fun _ x -> f x)
  end
end

open MoreLabels.Hashtbl

type nonrec ('a, 'b) t = ('a, 'b) t

let hash = hash
let create = create
let add = add
let replace = replace
let length = length
let remove = remove
let mem = mem

let find = find_opt

let add t key data = add t ~key ~data

let find_or_add t key ~f =
  match find t key with
  | Some x -> x
  | None ->
    let x = f key in
    add t key x;
    x

let foldi t ~init ~f = fold  t ~init ~f:(fun ~key ~data acc -> f key data acc)
let fold  t ~init ~f = foldi t ~init ~f:(fun _ x -> f x)

let iter t ~f = iter ~f t

let keys t = foldi t ~init:[] ~f:(fun key _ acc -> key :: acc)

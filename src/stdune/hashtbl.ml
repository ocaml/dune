include MoreLabels.Hashtbl

let find t key =
  match find t key with
  | exception Not_found -> None
  | x -> Some x

let find_or_add t key ~f =
  match find t key with
  | Some x -> x
  | None ->
    let x = f key in
    add t ~key ~data:x;
    x

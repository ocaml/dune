include struct
  [@@@warning "-32"]

  let find_opt t key =
    match MoreLabels.Hashtbl.find t key with
    | x -> Some x
    | exception Not_found -> None
end

include MoreLabels.Hashtbl

let find = find_opt

let find_or_add t key ~f =
  match find t key with
  | Some x -> x
  | None ->
    let x = f key in
    add t ~key ~data:x;
    x

let add t key data = add t ~key ~data

let foldi t ~init ~f = fold  t ~init ~f:(fun ~key ~data acc -> f key data acc)
let fold  t ~init ~f = foldi t ~init ~f:(fun _ x -> f x)

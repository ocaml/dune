include Stdlib.Queue

let push t x = add x t
let peek_exn t = peek t
let pop_exn t = pop t
let pop t = if is_empty t then None else Some (pop_exn t)
let peek t = if is_empty t then None else Some (peek t)
let iter t ~f = iter f t
let fold t ~f ~init = fold f init t
let to_list t = List.rev (fold t ~f:(fun acc a -> a :: acc) ~init:[])

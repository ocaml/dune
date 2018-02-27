type ('a, 'error) t = ('a, 'error) Caml.result =
  | Ok    of 'a
  | Error of 'error

let is_ok = function
  | Ok    _ -> true
  | Error _ -> false

let is_error = function
  | Ok    _ -> false
  | Error _ -> true

let bind t ~f =
  match t with
  | Ok x -> f x
  | Error _ as t -> t

let map x ~f =
  match x with
  | Ok x -> Ok (f x)
  | Error _ as x -> x

let map_error x ~f =
  match x with
  | Ok _ as res -> res
  | Error x -> Error (f x)

module O = struct
  let ( >>= ) t f = bind t ~f
  let ( >>| ) t f = map  t ~f
end

open O

let all =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | t :: l ->
      t >>= fun x ->
      loop (x :: acc) l
  in
  fun l -> loop [] l

let concat_map =
  let rec loop f acc = function
    | [] -> Ok (List.rev acc)
    | x :: l ->
      f x >>= fun y ->
      loop f (List.rev_append y acc) l
  in
  fun l ~f -> loop f [] l

type ('a, 'error) result = ('a, 'error) t

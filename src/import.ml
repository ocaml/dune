include (StdLabels
         : module type of struct include StdLabels end
         with module List := StdLabels.List)
include MoreLabels

module String_set = Set.Make(String)
module String_map = Map.Make(String)

module List = struct
  include ListLabels

  let rec filter_map l ~f =
    match l with
    | [] -> []
    | x :: l ->
      match f x with
      | None -> filter_map l ~f
      | Some x -> x :: filter_map l ~f

  let concat_map l ~f = concat (map l ~f)
end

type ('a, 'b) eq =
  | Eq : ('a, 'a) eq
  | Ne : ('a, 'b) eq

let (^/) a b = a ^ "/" ^ b

let sprintf = Printf.sprintf

let lines_of_file fn =
  let ic = open_in fn in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> close_in ic; List.rev acc
    | line -> loop (line :: acc)
  in
  loop []



include (StdLabels
         : module type of struct include StdLabels end
         with module List := StdLabels.List)
include MoreLabels

module String_set = Set.Make(String)
module String_map = Map.Make(String)

type ('a, 'b) either =
  | Inl of 'a
  | Inr of 'b

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

  let partition_map =
    let rec loop l accl accr ~f =
      match l with
      | [] -> (List.rev accl, List.rev accr)
      | x :: l ->
        match f x with
        | Inl y -> loop l (y :: accl) accr ~f
        | Inr y -> loop l accl (y :: accr) ~f
    in
    fun l ~f -> loop l [] [] ~f
end

type ('a, 'b) eq =
  | Eq : ('a, 'a) eq
  | Ne : ('a, 'b) eq

let (^/) a b = a ^ "/" ^ b

let sprintf = Printf.sprintf

let protectx x ~finally ~f =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let with_file_in fn ~f =
  protectx (open_in fn) ~finally:close_in ~f

let with_lexbuf_from_file fn ~f =
  with_file_in fn ~f:(fun ic ->
      let lb = Lexing.from_channel ic in
      lb.lex_curr_p <-
        { pos_fname = fn
        ; pos_lnum  = 1
        ; pos_bol   = 0
        ; pos_cnum  = 0
        };
      f lb)

let lines_of_file fn =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop ic (line :: acc)
  in
  with_file_in fn ~f:(fun ic -> loop ic [])

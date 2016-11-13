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

let protectx x ~finally ~f =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let with_file_in fn ~f =
  protectx (open_in fn) ~finally:close_in ~f

let with_lexbuf_from_file fn ~f =
  with_file_in fn ~f:(fun ic -> f (Lexing.from_channel ic))

let lines_of_file fn =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop ic (line :: acc)
  in
  with_file_in fn ~f:(fun ic -> loop ic [])

type location =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

let lexeme_loc lb =
  { start = Lexing.lexeme_start lb
  ; stop  = Lexing.lexeme_stop  lb
  }

exception File_error of location * string

let file_error ~loc fmt =
  Printf.ksprintf (fun msg -> raise (File_error (loc, msg))) fmt

let lex_error lb fmt =
  file_error ~loc:(lexeme_loc lb) fmt

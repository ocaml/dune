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
let ksprintf = Printf.ksprintf

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

let input_lines =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop ic (line :: acc)
  in
  fun ic -> loop ic []

let lines_of_file fn = with_file_in fn ~f:input_lines

exception Error of string
let die fmt = ksprintf (fun msg -> raise (Error msg)) fmt

let handle_process_status cmd (status : Unix.process_status) =
  match status with
  | WEXITED   0 -> ()
  | WEXITED   n -> die "Command exited with code %d: %s"     n (Lazy.force cmd)
  | WSIGNALED n -> die "Command got killed by signal %d: %s" n (Lazy.force cmd)
  | WSTOPPED  _ -> assert false

let with_process_in cmd ~f =
  let ic = Unix.open_process_in cmd in
  match f ic with
  | exception e ->
    ignore (Unix.close_process_in ic : Unix.process_status);
    raise e
  | y ->
    handle_process_status (lazy cmd) (Unix.close_process_in ic);
    y

let run_and_read_lines cmd = with_process_in cmd ~f:input_lines

let run_and_read_line cmd =
  match run_and_read_lines cmd with
  | []  -> die "Command returned no output: %s" cmd
  | [x] -> x
  | _   -> die "Command returned too many lines: %s" cmd

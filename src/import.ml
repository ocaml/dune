include Jbuilder_re

module Array   = StdLabels.Array
module Bytes   = StdLabels.Bytes
module Set     = MoreLabels.Set

external reraise : exn -> _ = "%reraise"

let open_in = open_in_bin
let open_out = open_out_bin

let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf

(* An error in the code of jbuild, that should be reported upstream *)
exception Code_error of string
let code_errorf fmt = ksprintf (fun msg -> raise (Code_error msg)) fmt

type ('a, 'b) result =
  | Ok    of 'a
  | Error of 'b

type ('a, 'b) either =
  | Inl of 'a
  | Inr of 'b

module List = struct
  type 'a t = 'a list

  include ListLabels

  let is_empty = function
    | [] -> true
    | _  -> false

  let rec filter_map l ~f =
    match l with
    | [] -> []
    | x :: l ->
      match f x with
      | None -> filter_map l ~f
      | Some x -> x :: filter_map l ~f

  let concat_map l ~f = concat (map l ~f)

  let rev_partition_map =
    let rec loop l accl accr ~f =
      match l with
      | [] -> (accl, accr)
      | x :: l ->
        match f x with
        | Inl y -> loop l (y :: accl) accr ~f
        | Inr y -> loop l accl (y :: accr) ~f
    in
    fun l ~f -> loop l [] [] ~f

  let partition_map l ~f =
    let l, r = rev_partition_map l ~f in
    (List.rev l, List.rev r)

  let rec find_map l ~f =
    match l with
    | [] -> None
    | x :: l ->
      match f x with
      | None -> find_map l ~f
      | Some _ as res -> res

  let rec find l ~f =
    match l with
    | [] -> None
    | x :: l -> if f x then Some x else find l ~f

  let longest_map l ~f =
    fold_left l ~init:0 ~f:(fun acc x ->
      max acc (String.length (f x)))

  let longest l = longest_map l ~f:(fun x -> x)
end

module Hashtbl = struct
  include MoreLabels.Hashtbl

  let find_exn t key ~string_of_key ~table_desc =
    try
      find t key
    with Not_found ->
      code_errorf "%s not found in table %s"
        (string_of_key key) (table_desc t)

  let find t key =
    match find t key with
    | exception Not_found -> None
    | x -> Some x

  let find_or_add t key ~f =
    match find t key with
    | Some x -> x
    | None ->
      let x = f () in
      add t ~key ~data:x;
      x
end

module Map = struct
  module type S = sig
    include MoreLabels.Map.S

    val add_multi : 'a list t -> key:key -> data:'a -> 'a list t
    val find : key -> 'a t -> 'a option
    val find_default : key -> 'a t -> default:'a -> 'a
    val find_exn
      :  key
      -> 'a t
      -> string_of_key:(key -> string)
      -> desc:('a t -> string)
      -> 'a
    val of_alist : (key * 'a) list -> ('a t, key * 'a * 'a) result
    val of_alist_exn : (key * 'a) list -> 'a t
    val of_alist_multi : (key * 'a) list -> 'a list t
    val keys : 'a t -> key list
    val values : 'a t -> 'a list
  end

  module Make(Key : MoreLabels.Map.OrderedType) : S with type key = Key.t = struct
    include MoreLabels.Map.Make(Key)

    let add_multi t ~key ~data =
      let rest =
        match find key t with
        | exception Not_found -> []
        | l -> l
      in
      add t ~key ~data:(data :: rest)

    let find_exn = find

    let find key t =
      match find key t with
      | exception Not_found -> None
      | x -> Some x

    let find_default key t ~default =
      try
        find_exn key t
      with Not_found ->
        default

    let of_alist l =
      List.fold_left l ~init:(Ok empty) ~f:(fun acc (key, data) ->
        match acc with
        | Error _ -> acc
        | Ok t ->
          if mem key t then
            Error (key, data, find_exn key t)
          else
            Ok (add t ~key ~data))

    let of_alist_exn l =
      match of_alist l with
      | Ok x -> x
      | Error _ -> invalid_arg "Map.of_alist_exn"

    let of_alist_multi l =
      List.fold_left l ~init:empty ~f:(fun acc (key, data) ->
        add_multi acc ~key ~data)

    let keys   t = bindings t |> List.map ~f:fst
    let values t = bindings t |> List.map ~f:snd

    let find_exn key t ~string_of_key ~desc =
      try
        find_exn key t
      with Not_found ->
        code_errorf "%s not found in map %s"
          (string_of_key key) (desc t)
  end
end

module String_set = Set.Make(String)
module String_map = Map.Make(String)

module String = struct
  include StringLabels

  let is_prefix s ~prefix =
    let len = length s in
    let prefix_len = length prefix in
    len >= prefix_len &&
    sub s ~pos:0 ~len:prefix_len = prefix

  let is_suffix s ~suffix =
    let len = length s in
    let suffix_len = length suffix in
    len >= suffix_len &&
    sub s ~pos:(len - suffix_len) ~len:suffix_len = suffix

  include struct
    [@@@warning "-3"]
    let capitalize_ascii   = String.capitalize
    let uncapitalize_ascii = String.uncapitalize
  end

  let split_words s =
    let rec skip_blanks i =
      if i = length s then
        []
      else
        match s.[i] with
        | ',' | ' ' | '\t' | '\n' -> skip_blanks (i + 1)
        | _ -> parse_word i (i + 1)
    and parse_word i j =
      if j = length s then
        [sub s ~pos:i ~len:(j - i)]
      else
        match s.[j] with
        | ',' | ' ' | '\t' | '\n' -> sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
        | _ -> parse_word i (j + 1)
    in
    skip_blanks 0

  let lsplit2 s ~on =
    match index s on with
    | exception Not_found -> None
    | i ->
      Some
        (sub s ~pos:0 ~len:i,
         sub s ~pos:(i + 1) ~len:(String.length s - i - 1))

  let index s ch =
    match index s ch with
    | i -> Some i
    | exception Not_found -> None

  let split s ~on =
    let rec loop i j =
      if j = length s then
        [sub s ~pos:i ~len:(j - i)]
      else if s.[j] = on then
        sub s ~pos:i ~len:(j - i) :: loop (j + 1) (j + 1)
      else
        loop i (j + 1)
    in
    loop 0 0
end

module Filename = struct
  include Filename

  let split_ext fn =
    match String.rindex fn '.' with
    | exception Not_found -> None
    | i ->
      Some
        (String.sub fn ~pos:0 ~len:i,
         String.sub fn ~pos:i ~len:(String.length fn - i))

  let ext fn =
    match String.rindex fn '.' with
    | exception Not_found -> None
    | i ->
      Some
        (String.sub fn ~pos:i ~len:(String.length fn - i))
end

module Option = struct
  type 'a t = 'a option

  let map t ~f =
    match t with
    | None -> None
    | Some x -> Some (f x)

  let iter t ~f =
    match t with
    | None -> ()
    | Some x -> f x

  let value t ~default =
    match t with
    | Some x -> x
    | None -> default

  let value_exn = function
    | Some x -> x
    | None -> assert false
end

type ('a, 'b) eq = Eq : ('a, 'a) eq

type nothing = (int, string) eq

let protectx x ~finally ~f =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let with_file_in fn ~f =
  protectx (open_in fn) ~finally:close_in ~f

let with_file_out fn ~f =
  protectx (open_out fn) ~finally:close_out ~f

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

let read_file fn =
  protectx (open_in fn) ~finally:close_in ~f:(fun ic ->
    let len = in_channel_length ic in
    really_input_string ic len)

let lines_of_file fn = with_file_in fn ~f:input_lines

let write_file fn data = with_file_out fn ~f:(fun oc -> output_string oc data)

exception Fatal_error of string
let die fmt = ksprintf (fun msg -> raise (Fatal_error msg)) fmt

let warn fmt =
  ksprintf (fun msg ->
    prerr_endline ("Warning: jbuild: " ^ msg))
    fmt

let copy_channels =
  let buf_len = 65536 in
  let buf = Bytes.create buf_len in
  let rec loop ic oc =
    match input ic buf 0 buf_len with
    | 0 -> ()
    | n -> output oc buf 0 n; loop ic oc
  in
  loop

let copy_file ~src ~dst =
  with_file_in src ~f:(fun ic ->
    let perm = (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm in
    protectx (open_out_gen
                [Open_wronly; Open_creat; Open_trunc; Open_binary]
                perm
                dst)
      ~finally:close_out
      ~f:(fun oc ->
        copy_channels ic oc))

module Staged : sig
  type +'a t
  val unstage : 'a t -> 'a
  val stage : 'a -> 'a t
end = struct
  type 'a t = 'a
  let unstage t = t
  let stage t = t
end

type fail = { fail : 'a. unit -> 'a }

let need_quoting s =
  let len = String.length s in
  len = 0 ||
  let rec loop i =
    if i = len then
      false
    else
      match s.[i] with
      | ' ' | '\"' -> true
      | _ -> loop (i + 1)
  in
  loop 0

let quote_for_shell s =
  if need_quoting s then
    Filename.quote s
  else
    s

let suggest_function : (string -> string list -> string list) ref = ref (fun _ _ -> [])

let hint name candidates =
  match !suggest_function name candidates with
  | [] -> ""
  | l ->
    let rec mk_hint = function
      | [a; b] -> sprintf "%s or %s" a b
      | [a] -> a
      | a :: l -> sprintf "%s, %s" a (mk_hint l)
      | [] -> ""
    in
    sprintf "\nHint: did you mean %s?" (mk_hint l)

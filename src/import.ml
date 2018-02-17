include Jbuilder_re
include Errors

module Array   = StdLabels.Array
module Bytes   = StdLabels.Bytes

module Set = struct
  module type OrderedType = MoreLabels.Set.OrderedType
  module type S = sig
    include MoreLabels.Set.S
    val map : f:(elt -> elt) -> t -> t
  end

  module Make(Elt : OrderedType) : S with type elt = Elt.t = struct
    module M = MoreLabels.Set.Make(Elt)

    include struct
      [@@@warning "-32"]
      (* [map] is only available since 4.04 *)
      let map ~f t =
        M.elements t
        |> List.map f
        |> M.of_list
    end

    include M
  end
end

external reraise : exn -> _ = "%reraise"

(* To make bug reports usable *)
let () = Printexc.record_backtrace true

let sprintf = Printf.sprintf
let ksprintf = Printf.ksprintf

let initial_cwd = Sys.getcwd ()

type ('a, 'b) either =
  | Inl of 'a
  | Inr of 'b

type ('a, 'b) result = ('a, 'b) Result.t =
  | Ok of 'a
  | Error of 'b

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

  let filteri l ~f =
    let rec filteri l i =
      match l with
      | [] -> []
      | x :: l ->
        let i' = succ i in
        if f i x
        then x :: filteri l i'
        else filteri l i'
    in
    filteri l 0

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

  let rec last = function
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs
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
      let x = f key in
      add t ~key ~data:x;
      x
end

module Map = struct
  module type S = sig
    include MoreLabels.Map.S

    val add_multi : 'a list t -> key:key -> data:'a -> 'a list t
    val find : key -> 'a t -> 'a option
    val find_default : key -> 'a t -> default:'a -> 'a
    val of_alist : (key * 'a) list -> ('a t, key * 'a * 'a) result
    val of_alist_exn : (key * 'a) list -> 'a t
    val of_alist_multi : (key * 'a) list -> 'a list t
    val of_alist_reduce : (key * 'a) list -> f:('a -> 'a -> 'a) -> 'a t
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

    let of_alist_reduce l ~f =
      List.fold_left l ~init:empty ~f:(fun acc (key, data) ->
        match find key acc with
        | None -> add acc ~key ~data
        | Some x -> add acc ~key ~data:(f x data))

    let of_alist_exn l =
      match of_alist l with
      | Ok x -> x
      | Error _ -> code_errorf "Map.of_alist_exn got duplicated key"

    let of_alist_multi l =
      let l = List.rev l in
      List.fold_left l ~init:empty ~f:(fun acc (key, data) ->
        add_multi acc ~key ~data)

    let keys   t = bindings t |> List.map ~f:fst
    let values t = bindings t |> List.map ~f:snd
  end
end

module String_set = Set.Make(String)
module String_map = Map.Make(String)

module String = struct
  include StringLabels

  let break s ~pos =
    (sub s ~pos:0 ~len:pos,
     sub s ~pos ~len:(String.length s - pos))

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

  let drop_prefix s ~prefix =
    if is_prefix s ~prefix then
      if length s = length prefix then
        Some ""
      else
        Some (sub s ~pos:(length prefix) ~len:(length s - length prefix))
    else
      None

  include struct
    [@@@warning "-3"]
    let capitalize_ascii   = String.capitalize
    let uncapitalize_ascii = String.uncapitalize
    let uppercase_ascii    = String.uppercase
    let lowercase_ascii    = String.lowercase
  end

  let extract_words s ~is_word_char =
    let rec skip_blanks i =
      if i = length s then
        []
      else if is_word_char s.[i] then
        parse_word i (i + 1)
      else
        skip_blanks (i + 1)
    and parse_word i j =
      if j = length s then
        [sub s ~pos:i ~len:(j - i)]
      else if is_word_char s.[j] then
        parse_word i (j + 1)
      else
        sub s ~pos:i ~len:(j - i) :: skip_blanks (j + 1)
    in
    skip_blanks 0

  let extract_comma_space_separated_words s =
    extract_words s ~is_word_char:(function
      | ',' | ' ' | '\t' | '\n' -> false
      | _ -> true)

  let extract_blank_separated_words s =
    extract_words s ~is_word_char:(function
      | ' ' | '\t' -> false
      | _ -> true)

  let lsplit2 s ~on =
    match index s on with
    | exception Not_found -> None
    | i ->
      Some
        (sub s ~pos:0 ~len:i,
         sub s ~pos:(i + 1) ~len:(String.length s - i - 1))

  let rsplit2 s ~on =
    match rindex s on with
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

  let split_lines s =
    let rec loop ~last_is_cr ~acc i j =
      if j = length s then (
        let acc =
          if j = i || (j = i + 1 && last_is_cr) then
          acc
        else
          sub s ~pos:i ~len:(j - i) :: acc
        in
        List.rev acc
      ) else
        match s.[j] with
        | '\r' -> loop ~last_is_cr:true ~acc i (j + 1)
        | '\n' ->
          let line =
            let len = if last_is_cr then j - i - 1 else j - i in
            sub s ~pos:i ~len
          in
          loop ~acc:(line :: acc) (j + 1) (j + 1) ~last_is_cr:false
        | _ ->
          loop ~acc i (j + 1) ~last_is_cr:false
    in
    loop ~acc:[] 0 0 ~last_is_cr:false

  (* Escape ONLY double quotes.  String.escape also escapes
     '\n',... and transforms all chars above '~' into '\xxx' which is
     not suitable for UTF-8 strings. *)
  let escape_double_quote s =
    let n = ref 0 in
    let len = String.length s in
    for i = 0 to len - 1 do
      if String.unsafe_get s i = '"' then incr n;
    done;
    if !n = 0 then s
    else (
      let b = Bytes.create (len + !n) in
      n := 0;
      for i = 0 to len - 1 do
        if String.unsafe_get s i = '"' then (
          Bytes.unsafe_set b !n '\\';
          incr n;
        );
        Bytes.unsafe_set b !n (String.unsafe_get s i);
        incr n
      done;
      Bytes.unsafe_to_string b
    )
end

module Sys = struct
  include Sys

  let force_remove =
    if win32 then
      fun fn ->
        try
          remove fn
        with Sys_error _ ->
          (* Try to remove the "read-only" attribute, then retry. *)
          (try Unix.chmod fn 0o666 with Unix.Unix_error _ -> ());
          remove fn
    else
      remove
end

module Filename = struct
  include Filename

  (* Return the index of the start of the extension, using the same semantic as
     [Filename.extension] in 4.04 *)
  let extension_start =
    (* This is from the win32 implementation, but it is acceptable for the usage we make
       of it in this function and covers all platforms. *)
    let is_dir_sep = function
      | '/' | '\\' | ':' -> true
      | _ -> false
    in
    let rec check_at_least_one_non_dot s len candidate i =
      if i < 0 then
        len
      else
        match s.[i] with
        | '.' ->
          check_at_least_one_non_dot s len candidate (i - 1)
        | c ->
          if is_dir_sep c then
            len
          else
            candidate
    in
    let rec search_dot s len i =
      if i <= 0 then
        len
      else
        match s.[i] with
        | '.' -> check_at_least_one_non_dot s len i (i - 1)
        | c   -> if is_dir_sep c then len else search_dot s len (i - 1)
    in
    fun s ->
      let len = String.length s in
      search_dot s len (len - 1)

  let split_extension fn =
    let i = extension_start fn in
    (String.sub fn ~pos:0 ~len:i,
     String.sub fn ~pos:i ~len:(String.length fn - i))

  let split_extension_after_dot fn =
    let i = extension_start fn + 1 in
    let len = String.length fn in
    if i > len then
      (fn, "")
    else
      (String.sub fn ~pos:0 ~len:i,
       String.sub fn ~pos:i ~len:(String.length fn - i))

  let extension fn =
    let i = extension_start fn in
    String.sub fn ~pos:i ~len:(String.length fn - i)

  type program_name_kind =
    | In_path
    | Relative_to_current_dir
    | Absolute

  let analyze_program_name fn =
    if not (Filename.is_relative fn) then
      Absolute
    else if String.contains fn '/' || (Sys.win32 && String.contains fn '\\') then
      Relative_to_current_dir
    else
      In_path
end

module Option = struct
  type 'a t = 'a option

  module Infix = struct
    let (>>=) t f =
      match t with
      | None -> None
      | Some a -> f a
  end

  let bind t ~f = Infix.(>>=) t f

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

  let some_if cond x =
    if cond then Some x else None

  let is_some = function
    | None   -> false
    | Some _ -> true

  let is_none = function
    | None   -> true
    | Some _ -> false

  let both x y =
    match x, y with
    | Some x, Some y -> Some (x, y)
    | _ -> None

  let to_list = function
    | None -> []
    | Some x -> [x]
end

type ('a, 'b) eq = Eq : ('a, 'a) eq

type nothing = (int, string) eq

let protectx x ~finally ~f =
  match f x with
  | y           -> finally x; y
  | exception e -> finally x; raise e

let warn fmt =
  ksprintf (fun msg ->
    prerr_endline ("Warning: jbuild: " ^ msg))
    fmt

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


(* [maybe_quoted s] is [s] if [s] doesn't need escaping according to OCaml lexing
   conventions and [sprintf "%S" s] otherwise. *)
let maybe_quoted s =
  let escaped = String.escaped s in
  if s == escaped || s = escaped then
    s
  else
    sprintf {|"%s"|} escaped

(* Disable file operations to force to use the IO module *)
let open_in      = `Use_Io
let open_in_bin  = `Use_Io
let open_in_gen  = `Use_Io
let open_out     = `Use_Io
let open_out_bin = `Use_Io
let open_out_gen = `Use_Io

(* We open this module at the top of module generating rules, to make sure they don't do
   Io manually *)
module No_io = struct
  module Io = struct end
end

module Fmt = struct
  type 'a t = Format.formatter -> 'a -> unit

  let kstrf f fmt =
    let buf = Buffer.create 17 in
    let f fmt = Format.pp_print_flush fmt () ; f (Buffer.contents buf) in
    Format.kfprintf f (Format.formatter_of_buffer buf) fmt

  let failwith fmt = kstrf failwith fmt
end

(* This is ugly *)
let printer = ref (Printf.eprintf "%s%!")
let print_to_console s = !printer s

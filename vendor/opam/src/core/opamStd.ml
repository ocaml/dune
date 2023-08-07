(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module Re = Dune_re

module type SET = sig
  include Set.S
  val map: (elt -> elt) -> t -> t
  val is_singleton: t -> bool
  val choose_one : t -> elt
  val choose_opt : t -> elt option
  val of_list: elt list -> t
  val to_list_map: (elt -> 'b) -> t -> 'b list
  val to_string: t -> string
  val to_json: t OpamJson.encoder
  val of_json: t OpamJson.decoder
  val find: (elt -> bool) -> t -> elt
  val find_opt: (elt -> bool) -> t -> elt option
  val safe_add: elt -> t -> t
  val fixpoint: (elt -> t) -> t -> t
  val map_reduce: ?default:'a -> (elt -> 'a) -> ('a -> 'a -> 'a) -> t -> 'a

  module Op : sig
    val (++): t -> t -> t
    val (--): t -> t -> t
    val (%%): t -> t -> t
  end
end
module type MAP = sig
  include Map.S
  val to_string: ('a -> string) -> 'a t -> string
  val to_json: 'a OpamJson.encoder -> 'a t OpamJson.encoder
  val of_json: 'a OpamJson.decoder -> 'a t OpamJson.decoder
  val keys: 'a t -> key list
  val values: 'a t -> 'a list
  val find_opt: key -> 'a t -> 'a option
  val choose_opt: 'a t -> (key * 'a) option
  val union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val is_singleton: 'a t -> bool
  val of_list: (key * 'a) list -> 'a t
  val safe_add: key -> 'a -> 'a t -> 'a t
  val update: key -> ('a -> 'a) -> 'a -> 'a t -> 'a t
  val map_reduce:
    ?default:'b -> (key -> 'a -> 'b) -> ('b -> 'b -> 'b) -> 'a t -> 'b
  val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
end
module type ABSTRACT = sig
  type t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val of_string: string -> t
  val to_string: t -> string
  val to_json: t OpamJson.encoder
  val of_json: t OpamJson.decoder
  module Set: SET with type elt = t
  module Map: MAP with type key = t
end

module type OrderedType = sig
  include Set.OrderedType
  val to_string: t -> string
  val to_json: t OpamJson.encoder
  val of_json: t OpamJson.decoder
end

module OpamCompare = struct
  external compare : 't -> 't -> int = "%compare"
  external equal : 't -> 't -> bool = "%equal"
  external (=) : 't -> 't -> bool = "%equal"
  external (<>) : 't -> 't -> bool = "%notequal"
  external (<) : 't -> 't -> bool = "%lessthan"
  external (>) : 't -> 't -> bool = "%greaterthan"
  external (<=) : 't -> 't -> bool = "%lessequal"
  external (>=) : 't -> 't -> bool = "%greaterequal"
end

let max_print = 100

module OpamList = struct

  let cons x xs = x :: xs

  let concat_map ?(left="") ?(right="") ?nil ?last_sep sep f =
    let last_sep = match last_sep with None -> sep | Some sep -> sep in
    function
    | [] -> (match nil with Some s -> s | None -> left^right)
    | l ->
      let seplen = String.length sep in
      let strs,len =
        List.fold_left (fun (strs,len) x ->
            let s = f x in s::strs, String.length s + seplen + len)
          ([], String.length left + String.length right - seplen)
          l
      in
      let len = match l with
        | _::_::_ -> len + String.length last_sep - seplen
        | _ -> len
      in
      let buf = Bytes.create len in
      let prepend i s =
        let slen = String.length s in
        Bytes.blit_string s 0 buf (i - slen) slen;
        i - slen
      in
      let pos = prepend len right in
      let pos = prepend pos (List.hd strs) in
      let pos =
        List.fold_left (fun (pos, cur_sep) s -> (prepend (prepend pos cur_sep) s, sep))
          (pos, last_sep) (List.tl strs) |> fst
      in
      let pos = prepend pos left in
      assert (pos = 0);
      Bytes.to_string buf

  let rec find_opt f = function
    | [] -> None
    | x::r -> if f x then Some x else find_opt f r

  let to_string f =
    concat_map ~left:"{ " ~right:" }" ~nil:"{}" ", " f

  let rec remove_duplicates eq = function
    | a::(b::_ as r) when eq a b -> remove_duplicates eq r
    | a::r -> a::remove_duplicates eq r
    | [] -> []

  let sort_nodup cmp l =
    remove_duplicates (fun a b -> cmp a b = 0) (List.sort cmp l)

  let filter_map f l =
    let rec loop accu = function
      | []     -> List.rev accu
      | h :: t ->
        match f h with
        | None   -> loop accu t
        | Some x -> loop (x::accu) t in
    loop [] l

  let filter_some l = filter_map (fun x -> x) l

  let rec find_map f = function
    | [] -> raise Not_found
    | x::r -> match f x with
      | Some r -> r
      | None -> find_map f r

  let rec find_map_opt f = function
    | [] -> None
    | x::r -> match f x with
      | Some r -> Some r
      | None -> find_map_opt f r

  let insert comp x l =
    let rec aux = function
      | [] -> [x]
      | h::t when comp h x < 0 -> h::aux t
      | l -> x :: l in
    aux l

  let rec insert_at index value = function
    | [] -> [value]
    | l when index <= 0 -> value :: l
    | x::l -> x :: insert_at (index - 1) value l

  let rec assoc eq x = function
    | [] -> raise Not_found
    | (a,b)::r -> if eq a x then b else assoc eq x r

  let rec assoc_opt eq x = function
    |  [] -> None
    | (a,b)::l -> if eq a x then Some b else assoc_opt eq x l

  let pick f l =
    let rec aux acc = function
      | [] -> None, l
      | x::l ->
        if f x then Some x, List.rev_append acc l
        else aux (x::acc) l
    in
    aux [] l

  let pick_assoc eq x l =
    let rec aux acc = function
      | [] -> None, l
      | (k,v) as b::r ->
        if eq k x then Some v, List.rev_append acc r
        else aux (b::acc) r
    in
    aux [] l

  let rec mem_assoc eq x = function
    | [] -> false
    | (a,_)::r -> eq a x || mem_assoc eq x r

  let update_assoc eq k v l =
    let rec aux acc = function
      | [] -> List.rev ((k,v)::acc)
      | (k1,_) as b::r ->
        if eq k1 k then List.rev_append acc ((k,v)::r)
        else aux (b::acc) r
    in
    aux [] l

  let remove_assoc eq k l =
    let rec aux acc = function
      | [] -> List.rev acc
      | (k1,_) as b::r ->
        if eq k1 k then List.rev_append acc r
        else aux (b::acc) r
    in
    aux [] l

  let fold_left_map f s l =
    let s, l_rev =
      List.fold_left (fun (s, l_rev) x ->
          let s, y = f s x in
          s, y :: l_rev)
        (s, []) l
    in
    s, List.rev l_rev

end


module Set = struct

  module Make (O : OrderedType) = struct

    module S = Set.Make(O)

    include S

    let fold f set i =
      let r = ref i in
      S.iter (fun elt ->
          r := f elt !r
        ) set;
      !r

    let is_singleton s =
      not (is_empty s) &&
      min_elt s == max_elt s

    let choose_one s =
      if is_empty s then raise Not_found
      else if is_singleton s then choose s
      else failwith "choose_one"

    let choose_opt s =
      try Some (choose s) with Not_found -> None

    let of_list l =
      List.fold_left (fun set e -> add e set) empty l

    let to_list_map f set =
      fold (fun x acc -> f x :: acc) set []

    let to_string s =
      if S.cardinal s > max_print then
        Printf.sprintf "%d elements" (S.cardinal s)
      else
        let l = S.fold (fun nv l -> O.to_string nv :: l) s [] in
        OpamList.to_string (fun x -> x) (List.rev l)

    let map f t =
      S.fold (fun e set -> S.add (f e) set) t S.empty

    exception Found of elt

    let find_opt fn t =
      try iter (fun x -> if fn x then raise (Found x)) t; None
      with Found x -> Some x

    let find fn t =
      match find_opt fn t with
      | Some x -> x
      | None -> raise Not_found

    let to_json t =
      let elements = S.elements t in
      let jsons = List.map O.to_json elements in
      `A jsons

    let of_json = function
      | `A jsons ->
        begin try
            let get = function
              | None -> raise Not_found
              | Some v -> v in
            let elems = List.map get (List.map O.of_json jsons) in
            Some (S.of_list elems)
          with Not_found -> None
        end
      | _ -> None

    module Op = struct
      let (++) = union
      let (--) = diff
      let (%%) = inter
    end

    let safe_add elt t =
      if mem elt t
      then failwith (Printf.sprintf "duplicate entry %s" (O.to_string elt))
      else add elt t

    let fixpoint f =
      let open Op in
      let rec aux fullset curset =
        if is_empty curset then fullset else
        let newset = fold (fun nv set -> set ++ f nv) curset empty in
        let fullset = fullset ++ curset in
        aux fullset (newset -- fullset)
      in
      aux empty

    let map_reduce ?default f op t =
      match choose_opt t with
      | Some x ->
        fold (fun x acc -> op acc (f x)) (remove x t) (f x)
      | None ->
        match default with
        | Some d -> d
        | None -> invalid_arg "Set.map_reduce"

  end

end

module Map = struct

  module Make (O : OrderedType) = struct

    module M = Map.Make(O)

    include M

    let fold f map i =
      let r = ref i in
      M.iter (fun key value->
          r:= f key value !r
        ) map;
      !r

    let map f map =
      fold (fun key value map ->
          add key (f value) map
        ) map empty

    let mapi f map =
      fold (fun key value map ->
          add key (f key value) map
        ) map empty

    let filter_map f map =
      fold (fun key value map ->
          match f key value with
          | Some value -> add key value map
          | None -> map
        ) map empty

    let values map =
      List.rev (M.fold (fun _ v acc -> v :: acc) map [])

    let keys map =
      List.rev (M.fold (fun k _ acc -> k :: acc) map [])

    let union f m1 m2 =
      M.merge (fun _ a b -> match a, b with
          | Some _ as s, None | None, (Some _ as s) -> s
          | Some v1, Some v2 -> Some (f v1 v2)
          | None, None -> assert false)
        m1 m2

    let is_singleton s =
      not (is_empty s) &&
      fst (min_binding s) == fst (max_binding s)

    let to_string string_of_value m =
      if M.cardinal m > max_print then
        Printf.sprintf "%d elements" (M.cardinal m)
      else
        let s (k,v) = Printf.sprintf "%s:%s" (O.to_string k) (string_of_value v) in
        let l = fold (fun k v l -> s (k,v)::l) m [] in
        OpamList.to_string (fun x -> x) l

    let of_list l =
      List.fold_left (fun map (k,v) -> add k v map) empty l

    let to_json json_of_value t =
      let bindings = M.bindings t in
      let jsons = List.map (fun (k,v) ->
          `O [ ("key"  , O.to_json k);
               ("value", json_of_value v) ]
        ) bindings in
      `A jsons

    let of_json value_of_json = function
      | `A jsons ->
        begin try
            let get_pair = function
              | `O binding ->
                begin match
                    O.of_json (OpamList.assoc String.equal "key" binding),
                    value_of_json (OpamList.assoc String.equal "value" binding)
                  with
                  | Some key, Some value -> (key, value)
                  | _ -> raise Not_found
                end
              | _ -> raise Not_found in
            let pairs = List.map get_pair jsons in
            Some (of_list pairs)
          with Not_found -> None
        end
      | _ -> None

    let find_opt k map = try Some (find k map) with Not_found -> None

    let choose_opt m =
      try Some (choose m) with Not_found -> None

    let safe_add k v map =
      if mem k map
      then failwith (Printf.sprintf "duplicate entry %s" (O.to_string k))
      else add k v map

    let update k f zero map =
      let v = try find k map with Not_found -> zero in
      add k (f v) map

    let map_reduce ?default f op t =
      match choose_opt t with
      | Some (k, v) ->
        fold (fun k v acc -> op acc (f k v)) (remove k t) (f k v)
      | None ->
        match default with
        | Some d -> d
        | None -> invalid_arg "Map.map_reduce"
  end

end

module AbstractString = struct
  type t = string
  let compare = String.compare
  let equal = String.equal
  let of_string x = x
  let to_string x = x
  let to_json x = `String x
  let of_json = function
    | `String x -> Some x
    | _ -> None
  module O = struct
    type t = string
    let to_string = to_string
    let compare = compare
    let to_json = to_json
    let of_json = of_json
  end
  module Set = Set.Make(O)
  module Map = Map.Make(O)
end


module OInt = struct
  type t = int
  let compare = Int.compare
  let to_string = string_of_int
  let to_json i = `String (string_of_int i)
  let of_json = function
    | `String s -> (try Some (int_of_string s) with _ -> None)
    | _ -> None
end

module IntMap = Map.Make(OInt)
module IntSet = Set.Make(OInt)



module Option = struct
  let map f = function
    | None -> None
    | Some x -> Some (f x)

  let iter f = function
    | None -> ()
    | Some x -> f x

  let default dft = function
    | None -> dft
    | Some x -> x

  let default_map dft = function
    | None -> dft
    | some -> some

  let replace f = function
    | None -> None
    | Some x -> f x

  let map_default f dft = function
    | None -> dft
    | Some x -> f x

  let compare cmp o1 o2 =
    match o1,o2 with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some x1, Some x2 -> cmp x1 x2

  let equal f o1 o2 =
    match o1, o2 with
    | Some o1, Some o2 -> f o1 o2
    | None, None -> true
    | _ , _  -> false

  let to_string ?(none="") f = function
    | Some x -> f x
    | None -> none

  let to_list = function
    | None -> []
    | Some x -> [x]

  let some x = Some x
  let none _ = None

  let of_Not_found f x =
    try Some (f x) with Not_found -> None

  module Op = struct
    let (>>=) = function
      | None -> fun _ -> None
      | Some x -> fun f -> f x
    let (>>|) opt f = map f opt
    let (>>+) opt f = match opt with
      | None -> f ()
      | some -> some
    let (+!) opt dft = default dft opt
    let (++) = function
      | None -> fun opt -> opt
      | some -> fun _ -> some
  end
end



module OpamString = struct

  module OString = struct
    type t = string
    let compare = String.compare
    let to_string x = x
    let to_json x = `String x
    let of_json = function
      | `String s -> Some s
      | _ -> None
  end

  module StringSet = Set.Make(OString)
  module StringMap = Map.Make(OString)

  module SetSet = Set.Make(StringSet)
  module SetMap = Map.Make(StringSet)

  module Set = StringSet
  module Map = StringMap

  let starts_with ~prefix s =
    let x = String.length prefix in
    let n = String.length s in
    n >= x &&
    let rec chk i = i >= x || prefix.[i] = s.[i] && chk (i+1) in
    chk 0

  let ends_with ~suffix s =
    let x = String.length suffix in
    let n = String.length s in
    n >= x &&
    let rec chk i = i >= x || suffix.[i] = s.[i+n-x] && chk (i+1) in
    chk 0

  let for_all f s =
    let len = String.length s in
    let rec aux i = i >= len || f s.[i] && aux (i+1) in
    aux 0

  let contains_char s c =
    try let _ = String.index s c in true
    with Not_found -> false

  let contains ~sub =
    Re.(execp (compile (str sub)))

  let exact_match re s =
    try
      let subs = Re.exec re s in
      let subs = Array.to_list (Re.Group.all_offset subs) in
      let n = String.length s in
      let subs = List.filter (fun (s,e) -> s=0 && e=n) subs in
      List.length subs > 0
    with Not_found ->
      false

  let find_from f s i =
    let l = String.length s in
    if i < 0 || i > l then
      invalid_arg "find_from"
    else
      let rec g i =
        if i < l then
          if f s.[i] then
            i
          else
            g (succ i)
        else
          raise Not_found in
      g i

  let map f s =
    let len = String.length s in
    let b = Bytes.create len in
    for i = 0 to len - 1 do Bytes.set b i (f s.[i]) done;
    Bytes.to_string b

  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false

  let strip str =
    let p = ref 0 in
    let l = String.length str in
    while !p < l && is_whitespace (String.unsafe_get str !p) do
      incr p;
    done;
    let p = !p in
    let l = ref (l - 1) in
    while !l >= p && is_whitespace (String.unsafe_get str !l) do
      decr l;
    done;
    String.sub str p (!l - p + 1)

  let strip_right str =
    let rec aux i =
      if i < 0 || not (is_whitespace str.[i]) then i else aux (i-1)
    in
    let l = String.length str in
    let i = aux (l-1) in
    if i = l - 1 then str
    else String.sub str 0 (i+1)

  let sub_at n s =
    if String.length s <= n then
      s
    else
      String.sub s 0 n

  let remove_prefix ~prefix s =
    if starts_with ~prefix s then
      let x = String.length prefix in
      let n = String.length s in
      String.sub s x (n - x)
    else
      s

  let remove_suffix ~suffix s =
    if ends_with ~suffix s then
      let x = String.length suffix in
      let n = String.length s in
      String.sub s 0 (n - x)
    else
      s

  let cut_at_aux fn s sep =
    try
      let i = fn s sep in
      let name = String.sub s 0 i in
      let version = String.sub s (i+1) (String.length s - i - 1) in
      Some (name, version)
    with Invalid_argument _ | Not_found ->
      None

  let cut_at = cut_at_aux String.index

  let rcut_at = cut_at_aux String.rindex

  let split s c =
    (* old compat version (Re 1.2.0)
       {[Re_str.split (Re_str.regexp (Printf.sprintf "[%c]+" c)) s]} *)
    Re.(split (compile (rep1 (char c)))) s

  let split_delim s c =
    let tokens = Re.(split_full (compile (char c)) s) in
    let rec aux acc = function
      | [] -> acc
      | (`Delim _)::[] -> ""::acc
      | (`Text s)::tl -> aux (s::acc) tl
      | (`Delim _)::tl -> aux acc tl
    in
    let acc0 =
      match tokens with
      | (`Delim _)::_ -> [""]
      |_ -> []
    in List.rev (aux acc0 tokens)

  let fold_left f acc s =
    let acc = ref acc in
    for i = 0 to String.length s - 1 do acc := f !acc s.[i] done;
    !acc

  let compare_case s1 s2 =
    let l1 = String.length s1 and l2 = String.length s2 in
    let len = min l1 l2 in
    let rec aux i =
      if i < len then
        let c1 = s1.[i] and c2 = s2.[i] in
        match Char.compare (Char.lowercase_ascii c1) (Char.lowercase_ascii c2)
        with
        | 0 ->
          (match Char.compare c1 c2 with
           | 0 -> aux (i+1)
           | c -> c)
        | c -> c
      else
        if l1 < l2 then -1
        else if l1 > l2 then 1
        else 0
    in
    aux 0

  let is_prefix_of ~from ~full s =
    let length_s = String.length s in
    let length_full = String.length full in
    if from < 0 || from > length_full then
      invalid_arg "is_prefix_of"
    else
      length_s <= length_full
      && length_s > from
      && String.sub full 0 length_s = s

  let is_hex s =
    try
      String.iter (function
          | '0'..'9' | 'A'..'F' | 'a'..'f' -> ()
          | _ -> raise Exit)
        s;
      true
    with Exit -> false

end

type warning_printer =
  {mutable warning : 'a . ('a, unit, string, unit) format4 -> 'a}
let console = ref {warning = fun fmt -> Printf.ksprintf prerr_string fmt}

module Env = struct

  (* Remove from a c-separated list of string the one with the given prefix *)
  let reset_value ~prefix c v =
    let v = OpamString.split v c in
    List.filter (fun v -> not (OpamString.starts_with ~prefix v)) v

  (* Split the list in two according to the first occurrence of the string
     starting with the given prefix.
  *)
  let cut_value ~prefix c v =
    let v = OpamString.split v c in
    let rec aux before =
      function
      | [] -> [], List.rev before
      | curr::after when OpamString.starts_with ~prefix curr ->
        before, after
      | curr::after -> aux (curr::before) after
    in aux [] v

  let escape_single_quotes ?(using_backslashes=false) =
    if using_backslashes then
      Re.(replace (compile (set "\\\'")) ~f:(fun g -> "\\"^Group.get g 0))
    else
      Re.(replace_string (compile (char '\'')) ~by:"'\"'\"'")

  let escape_powershell =
    (* escape single quotes with two single quotes.
       https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.1 *)
    Re.(replace_string (compile (char '\'')) ~by:"''")

  module Name = struct
    module M = struct
      include AbstractString

      let compare =
        if Sys.win32 then
          fun l r ->
            String.(compare (lowercase_ascii l) (lowercase_ascii r))
        else
          String.compare
    end

    type t = string

    let of_string = M.of_string
    let to_string = M.to_string
    let of_json = M.of_json
    let to_json = M.to_json
    let compare = M.compare

    let equal =
      if Sys.win32 then
        fun l r ->
          String.(equal (lowercase_ascii l) (lowercase_ascii r))
      else
        String.equal

    let equal_string = equal

    module Set = Set.Make(M)
    module Map = Map.Make(M)
  end

  let to_list env =
    List.rev_map (fun s ->
        match OpamString.cut_at s '=' with
        | None   -> s, ""
        | Some p -> p)
      (Array.to_list env)

  let raw_env = Unix.environment

  let list =
    let lazy_env = lazy (to_list (raw_env ())) in
    fun () -> Lazy.force lazy_env

  let cyg_env cygbin =
    let env = raw_env () in
    let f v =
      match OpamString.cut_at v '=' with
      | Some (path, c) when Name.equal_string path "path" ->
        Printf.sprintf "%s=%s;%s"
          path cygbin c
      | _ -> v
    in
    Array.map f env

  let get_full n = List.find (fun (k,_) -> Name.equal k n) (list ())

  let get n = snd (get_full n)

  let getopt = Option.of_Not_found get

  let getopt_full n =
    try let (n, v) = get_full n in (n, Some v)
    with Not_found -> (n, None)
end


(** To use when catching default exceptions: ensures we don't catch fatal errors
    like C-c *)
let fatal e = match e with
  | Sys.Break -> prerr_newline (); raise e
  | Assert_failure _ | Match_failure _ -> raise e
  | _ -> ()

module OpamSys = struct

  let path_sep = if Sys.win32 then ';' else ':'

  let split_path_variable ?(clean=true) =
    if Sys.win32 then fun path ->
      let length = String.length path in
      let rec f acc index current last normal =
        if index = length then
          let current = current ^ String.sub path last (index - last) in
          List.rev (if current <> "" then current::acc else acc)
        else let c = path.[index]
          and next = succ index in
          if c = ';' && normal || c = '"' then
            let current = current ^ String.sub path last (index - last) in
            if c = '"' then
              f acc next current next (not normal)
            else
            let acc = if current = "" then acc else current::acc in
            f acc next "" next true
          else
            f acc next current last normal in
      f [] 0 "" 0 true
    else fun path ->
      let split = if clean then OpamString.split else OpamString.split_delim in
      split path path_sep

  let with_process_in cmd args f =
    if Sys.win32 then
      assert false;
    let path = split_path_variable (Env.get "PATH") in
    let cmd =
      List.find Sys.file_exists (List.map (fun d -> Filename.concat d cmd) path)
    in
    let ic = Unix.open_process_in (cmd^" "^args) in
    try
      let r = f ic in
      ignore (Unix.close_process_in ic) ; r
    with exn ->
      ignore (Unix.close_process_in ic) ; raise exn

  let tty_out = Unix.isatty Unix.stdout

  let tty_in = Unix.isatty Unix.stdin

  let default_columns = lazy (
    let default = 16_000_000 in
    let cols =
      try int_of_string (Env.get "COLUMNS") with
      | Not_found
      | Failure _ -> default
    in
    if cols > 0 then cols else default
  )

  let get_terminal_columns () =
    let fallback = 80 in
    let cols =
      try (* terminfo *)
        with_process_in "tput" "cols"
          (fun ic -> int_of_string (input_line ic))
      with
      | Unix.Unix_error _ | Sys_error _ | Failure _ | End_of_file | Not_found ->
        try (* GNU stty *)
          with_process_in "stty" "size"
            (fun ic ->
               match OpamString.split (input_line ic) ' ' with
               | [_ ; v] -> int_of_string v
               | _ -> failwith "stty")
        with
        | Unix.Unix_error _ | Sys_error _ | Failure _
        | End_of_file | Not_found -> fallback
    in
    if cols > 0 then cols else fallback

  let win32_get_console_width default_columns =
    try
      let hConsoleOutput = OpamStubs.(getStdHandle STD_OUTPUT_HANDLE) in
      let {OpamStubs.size = (width, _); _} =
        OpamStubs.getConsoleScreenBufferInfo hConsoleOutput
      in
      width
    with Not_found ->
      Lazy.force default_columns

  let terminal_columns =
    let v = ref (lazy (get_terminal_columns ())) in
    let () =
      try Sys.set_signal 28 (* SIGWINCH *)
            (Sys.Signal_handle
               (fun _ -> v := lazy (get_terminal_columns ())))
      with Invalid_argument _ -> ()
    in
    if Sys.win32 then
      fun () ->
        win32_get_console_width default_columns
    else
      fun () ->
        if tty_out
        then Lazy.force !v
        else Lazy.force default_columns

  let home =
    (* Note: we ask Unix.getenv instead of Env.get to avoid
       forcing the environment in this function that is used
       before the .init() functions are called -- see
       OpamStateConfig.default. *)
    let home = lazy (
      try Unix.getenv "HOME"
      with Not_found ->
        if Sys.win32 then
          (* CSIDL_PROFILE = 0x28 *)
          OpamStubs.(shGetFolderPath 0x28 SHGFP_TYPE_CURRENT)
        else
          Sys.getcwd ()
    ) in
    fun () -> Lazy.force home

  let etc () = "/etc"

  let uname =
    let memo = Hashtbl.create 7 in
    fun arg ->
      try Hashtbl.find memo arg with Not_found ->
        let r =
          try
            with_process_in "uname" arg
              (fun ic -> Some (OpamString.strip (input_line ic)))
          with Unix.Unix_error _ | Sys_error _ | Not_found -> None
        in
        Hashtbl.add memo arg r;
        r

  let system () =
    (* CSIDL_SYSTEM = 0x25 *)
    OpamStubs.(shGetFolderPath 0x25 SHGFP_TYPE_CURRENT)

  type os =
    | Darwin
    | Linux
    | FreeBSD
    | OpenBSD
    | NetBSD
    | DragonFly
    | Cygwin
    | Win32
    | Unix
    | Other of string

  let os =
    let os = lazy (
      match Sys.os_type with
      | "Unix" -> begin
          match uname "-s" with
          | Some "Darwin"    -> Darwin
          | Some "Linux"     -> Linux
          | Some "FreeBSD"   -> FreeBSD
          | Some "OpenBSD"   -> OpenBSD
          | Some "NetBSD"    -> NetBSD
          | Some "DragonFly" -> DragonFly
          | _                -> Unix
        end
      | "Win32"  -> Win32
      | "Cygwin" -> Cygwin
      | s        -> Other s
    ) in
    fun () -> Lazy.force os

  type powershell_host = Powershell_pwsh | Powershell
  type shell = SH_sh | SH_bash | SH_zsh | SH_csh | SH_fish
    | SH_pwsh of powershell_host | SH_cmd

  let all_shells =
    [SH_sh; SH_bash;
     SH_zsh;
     SH_csh;
     SH_fish;
     SH_pwsh Powershell_pwsh;
     SH_pwsh Powershell;
     SH_cmd]

  let windows_default_shell = SH_cmd
  let unix_default_shell = SH_sh

  let shell_of_string = function
    | "tcsh"
    | "bsd-csh"
    | "csh"  -> Some SH_csh
    | "zsh"  -> Some SH_zsh
    | "bash" -> Some SH_bash
    | "fish" -> Some SH_fish
    | "pwsh" -> Some (SH_pwsh Powershell_pwsh)
    | "dash"
    | "sh"   -> Some SH_sh
    | _      -> None

  let executable_name =
    if Sys.win32 then
      fun name ->
        if Filename.check_suffix name ".exe" then
          name
        else
          name ^ ".exe"
    else
      fun x -> x

  let windows_process_ancestry = Lazy.from_fun OpamStubs.getProcessAncestry

  type shell_choice = Accept of shell

  let windows_get_shell =
    let categorize_process (_, image) =
      match String.lowercase_ascii (Filename.basename image) with
      | "powershell.exe" | "powershell_ise.exe" ->
        Some (Accept (SH_pwsh Powershell))
      | "pwsh.exe" -> Some (Accept (SH_pwsh Powershell_pwsh))
      | "cmd.exe" -> Some (Accept SH_cmd)
      | "env.exe" -> Some (Accept SH_sh)
      | name ->
        Option.map
          (fun shell -> Accept shell)
          (shell_of_string (Filename.chop_suffix name ".exe"))
    in
    lazy (
      let lazy ancestors = windows_process_ancestry in
      match OpamList.filter_map categorize_process ancestors with
      | [] -> None
      | Accept most_relevant_shell :: _ -> Some most_relevant_shell
    )

  let guess_shell_compat () =
    let parent_guess () =
      let ppid = Unix.getppid () in
      let dir = Filename.concat "/proc" (string_of_int ppid) in
      try
        Some (Unix.readlink (Filename.concat dir "exe"))
      with e ->
        fatal e;
        try
          with_process_in "ps"
            (Printf.sprintf "-p %d -o comm= 2>/dev/null" ppid)
            (fun ic -> Some (input_line ic))
        with
        | Unix.Unix_error _ | Sys_error _ | Failure _ | End_of_file | Not_found ->
            try
              let c = open_in_bin ("/proc/" ^ string_of_int ppid ^ "/cmdline") in
              begin try
                let s = input_line c in
                close_in c;
                Some (String.sub s 0 (String.index s '\000'))
              with
              | Not_found ->
                  None
              | e ->
                  close_in c;
                  fatal e; None
              end
            with e ->
              fatal e; None
    in
    let test shell = shell_of_string (Filename.basename shell) in
    if Sys.win32 then
      let shell =
        match Lazy.force windows_get_shell with
        | None ->
          Option.of_Not_found Env.get "SHELL" |> Option.replace test
        | some ->
          some
        in
      Option.default windows_default_shell shell
    else
      let shell =
        match Option.replace test (parent_guess ()) with
        | None ->
            Option.of_Not_found Env.get "SHELL" |> Option.replace test
        | some ->
            some
      in
      Option.default unix_default_shell shell

  let guess_dot_profile shell =
    let home f =
      try Filename.concat (home ()) f
      with Not_found -> f in
    match shell with
    | SH_fish ->
      Some (List.fold_left Filename.concat (home ".config") ["fish"; "config.fish"])
    | SH_zsh  -> Some (home ".zshrc")
    | SH_bash ->
      let shell =
        (try
           List.find Sys.file_exists [
             (* Bash looks up these 3 files in order and only loads the first,
                for LOGIN shells *)
             home ".bash_profile";
             home ".bash_login";
             home ".profile";
             (* Bash loads .bashrc INSTEAD, for interactive NON login shells only;
                but it's often included from the above.
                We may include our variables in both to be sure ; for now we rely
                on non-login shells inheriting their env from a login shell
                somewhere... *)
           ]
         with Not_found ->
           (* iff none of the above exist, creating this should be safe *)
           home ".bash_profile")
      in
      Some shell
    | SH_csh ->
      let cshrc = home ".cshrc" in
      let tcshrc = home ".tcshrc" in
      Some (if Sys.file_exists cshrc then cshrc else tcshrc)
    | SH_pwsh _ ->
      None
    | SH_sh -> Some (home ".profile")
    | SH_cmd ->
      None


  let registered_at_exit = ref []
  let at_exit f =
    Stdlib.at_exit f;
    registered_at_exit := f :: !registered_at_exit
  let exec_at_exit () =
    List.iter
      (fun f -> try f () with _ -> ())
      !registered_at_exit

  let get_windows_executable_variant =
    if Sys.win32 then
      let results = Hashtbl.create 17 in
      let requires_cygwin cygcheck name =
        let env = Env.cyg_env (Filename.dirname cygcheck) in
        let cmd = OpamCompat.Filename.quote_command cygcheck [name] in
        let ((c, _, _) as process) = Unix.open_process_full cmd env in
        let rec check_dll platform =
          match input_line c with
          | dll ->
            let tdll = (*String.trim*) dll in
            if OpamString.ends_with ~suffix:"cygwin1.dll" tdll then
              if OpamString.starts_with ~prefix:"  " dll then
                check_dll `Cygwin
              else if platform = `Native then
                check_dll (`Tainted `Cygwin)
              else
                check_dll platform
            else if OpamString.ends_with ~suffix:"msys-2.0.dll" tdll then
              if OpamString.starts_with ~prefix:"  " dll then
                check_dll `Msys2
              else if platform = `Native then
                check_dll (`Tainted `Msys2)
              else
                check_dll platform
            else
              check_dll platform
          | exception e ->
            Unix.close_process_full process |> ignore;
            fatal e;
            platform
        in
        check_dll `Native
      in
      fun ~cygbin name ->
        match cygbin with
        | Some cygbin ->
          (let cygcheck = Filename.concat cygbin "cygcheck.exe" in
           if Filename.is_relative name then
             requires_cygwin cygcheck name
           else
           try Hashtbl.find results (cygcheck, name)
           with Not_found ->
             let result = requires_cygwin cygcheck name in
             Hashtbl.add results (cygcheck, name) result;
             result)
        | None -> `Native
    else
    fun ~cygbin:_ _ -> `Native

  let is_cygwin_cygcheck ~cygbin =
    match cygbin with
    | Some cygbin ->
      let cygpath = Filename.concat cygbin "cygpath.exe" in
      Sys.file_exists cygpath
      && (get_windows_executable_variant ~cygbin:(Some cygbin) cygpath = `Cygwin)
    | None -> false

  let get_cygwin_variant ~cygbin cmd =
    (* Treat MSYS2's variant of `cygwin1.dll` called `msys-2.0.dll` equivalently.
       Confer https://www.msys2.org/wiki/How-does-MSYS2-differ-from-Cygwin/ *)
    match get_windows_executable_variant ~cygbin cmd with
    | `Native -> `Native
    | `Cygwin
    | `Msys2 -> `Cygwin
    | `Tainted _ -> `CygLinked

  let is_cygwin_variant ~cygbin cmd =
    get_cygwin_variant ~cygbin cmd = `Cygwin

  exception Exit of int
  exception Exec of string * string array * string array

  let exit i = raise (Exit i)

  type exit_reason =
    [ `Success | `False | `Bad_arguments | `Not_found | `Aborted | `Locked
    | `No_solution | `File_error | `Package_operation_error | `Sync_error
    | `Configuration_error | `Solver_failure | `Internal_error
    | `User_interrupt ]

  let exit_codes : (exit_reason * int) list = [
    (* Normal return values *)
    `Success, 0;
    `False, 1;
    (* Errors happening in normal use (user related, or impossible requests) *)
    `Bad_arguments, 2;
    `Not_found, 5;
    `Aborted, 10;
    `Locked, 15;
    `No_solution, 20;
    (* Errors related to the database (repository and package definitions) *)
    `File_error, 30;
    `Package_operation_error, 31;
    (* Network related error *)
    `Sync_error, 40;
    (* Opam setup error *)
    `Configuration_error, 50;
    (* Errors that shouldn't happen and are likely bugs *)
    `Solver_failure, 60;
    `Internal_error, 99;
    (* Received signals *)
    `User_interrupt, 130;
  ]

  let get_exit_code reason = OpamList.assoc OpamCompare.equal reason exit_codes

  let exit_because reason = exit (get_exit_code reason)

  type nonrec warning_printer = warning_printer =
    {mutable warning : 'a . ('a, unit, string, unit) format4 -> 'a}

  let set_warning_printer =
    let called = ref false in
    fun printer ->
      if !called then invalid_arg "Just what do you think you're doing, Dave?";
      called := true;
      console := printer
end


module Win32 = struct
  module RegistryHive = struct
    let to_string = function
    | OpamStubs.HKEY_CLASSES_ROOT   -> "HKEY_CLASSES_ROOT"
    | OpamStubs.HKEY_CURRENT_CONFIG -> "HKEY_CURRENT_CONFIG"
    | OpamStubs.HKEY_CURRENT_USER   -> "HKEY_CURRENT_USER"
    | OpamStubs.HKEY_LOCAL_MACHINE  -> "HKEY_LOCAL_MACHINE"
    | OpamStubs.HKEY_USERS          -> "HKEY_USERS"

    let of_string = function
    | "HKCR"
    | "HKEY_CLASSES_ROOT"   -> OpamStubs.HKEY_CLASSES_ROOT
    | "HKCC"
    | "HKEY_CURRENT_CONFIG" -> OpamStubs.HKEY_CURRENT_CONFIG
    | "HKCU"
    | "HKEY_CURRENT_USER"   -> OpamStubs.HKEY_CURRENT_USER
    | "HKLM"
    | "HKEY_LOCAL_MACHINE"  -> OpamStubs.HKEY_LOCAL_MACHINE
    | "HKU"
    | "HKEY_USERS"          -> OpamStubs.HKEY_USERS
    | _                     -> failwith "RegistryHive.of_string"
  end

  let (set_parent_pid, parent_putenv) =
    let ppid = ref (OpamCompat.Lazy.map (function (_::(pid, _)::_) -> pid | _ -> 0l) OpamSys.windows_process_ancestry) in
    let parent_putenv = lazy (
      let {contents = lazy ppid} = ppid in
      let our_architecture = OpamStubs.getProcessArchitecture None in
      let their_architecture = OpamStubs.getProcessArchitecture (Some ppid) in
      let no_opam_putenv =
        let warning = lazy (
          !console.warning "opam-putenv was not found - \
                            OPAM is unable to alter environment variables";
          false)
        in
        fun _ _ -> Lazy.force warning
      in
      if our_architecture <> their_architecture then
        match their_architecture with
        | OpamStubs.ARM | ARM64 | IA64 | Unknown ->
          (* ARM support not yet implemented - just ensure we don't inject Intel
             code into an ARM process! *)
          no_opam_putenv
        | AMD64 | Intel ->
          (*
           * Expect to see opam-putenv.exe in the same directory as opam.exe,
           * rather than PATH (allow for crazy users like developers who may have
           * both builds of opam)
           *)
          let putenv_exe =
            Filename.(concat (dirname Sys.executable_name) "opam-putenv.exe")
          in
          let ctrl = ref stdout in
          let quit_putenv () =
            if !ctrl <> stdout then
              let () = Printf.fprintf !ctrl "::QUIT\n%!" in
              ctrl := stdout
          in
          at_exit quit_putenv;
          if Sys.file_exists putenv_exe then
            fun key value ->
              if !ctrl = stdout then begin
                let (inCh, outCh) = Unix.pipe () in
                let _ =
                  Unix.create_process putenv_exe
                                      [| putenv_exe; Int32.to_string ppid |]
                                      inCh Unix.stdout Unix.stderr
                in
                ctrl := (Unix.out_channel_of_descr outCh);
                set_binary_mode_out !ctrl true;
              end;
              Printf.fprintf !ctrl "%s\n%s\n%!" key value;
              if key = "::QUIT" then ctrl := stdout;
              true
          else
            no_opam_putenv
      else
        function "::QUIT" -> fun _ -> true
        | key -> OpamStubs.process_putenv ppid key)
    in
      ((fun pid ->
          if Lazy.is_val parent_putenv then
            failwith "Target parent already known";
          ppid := Lazy.from_val pid),
       (fun key -> (Lazy.force parent_putenv) key))

  let persistHomeDirectory dir =
    (* Update our environment *)
    Unix.putenv "HOME" dir;
    (* Update our parent's environment *)
    ignore (parent_putenv "HOME" dir);
    (* Persist the value to the user's environment *)
    OpamStubs.(writeRegistry HKEY_CURRENT_USER "Environment" "HOME" REG_SZ dir);
    (* Broadcast the change (or a reboot would be required) *)
    (* These constants are defined in WinUser.h *)
    let hWND_BROADCAST = 0xffffn in
    let sMTO_ABORTIFHUNG = 0x2 in
    OpamStubs.(sendMessageTimeout hWND_BROADCAST 5000 sMTO_ABORTIFHUNG
                                  WM_SETTINGCHANGE 0 "Environment") |> ignore
end


module OpamFormat = struct

  let visual_length_substring s ofs len =
    let rec aux acc i =
      if i >= len then acc
      else match s.[ofs + i] with
        | '\xc2'..'\xdf' -> aux (acc - min 1 (len - i)) (i + 2)
        | '\xe0'..'\xef' -> aux (acc - min 2 (len - i)) (i + 3)
        | '\xf0'..'\xf4' -> aux (acc - min 3 (len - i)) (i + 4)
        | '\027' ->
          (try
             let j = String.index_from s (ofs+i+1) 'm' - ofs in
             if j > len then acc - (len - i) else
               aux (acc - (j - i + 1)) (j + 1)
           with Not_found | Invalid_argument _ ->
             acc - (len - i))
        | _ -> aux acc (i + 1)
    in
    aux len 0

  let visual_length s = visual_length_substring s 0 (String.length s)

  let visual_width s =
    List.fold_left max 0 (List.map visual_length (OpamString.split s '\n'))

  let cut_at_visual s width =
    let rec aux extra i =
      try
        let j = String.index_from s i '\027' in
        let k = String.index_from s (j+1) 'm' in
        if j - extra > width then width + extra
        else aux (extra + k - j + 1) (k + 1)
      with Not_found -> min (String.length s) (width + extra)
         | Invalid_argument _ -> String.length s
    in
    let cut_at = aux 0 0 in
    if cut_at = String.length s then s else
    let sub = String.sub s 0 cut_at in
    let rec rem_escapes i =
      try
        let j = String.index_from s i '\027' in
        let k = String.index_from s (j+1) 'm' in
        String.sub s j (k - j + 1) :: rem_escapes (k+1)
      with Not_found | Invalid_argument _ -> []
    in
    String.concat "" (sub :: rem_escapes cut_at)

  let indent_left s ?(visual=s) nb =
    let nb = nb - String.length visual in
    if nb <= 0 then
      s
    else
      s ^ String.make nb ' '

  let indent_right s ?(visual=s) nb =
    let nb = nb - String.length visual in
    if nb <= 0 then
      s
    else
      String.make nb ' ' ^ s

  let align_table ll =
    let rec transpose ll =
      if List.for_all ((=) []) ll then [] else
      let col, rest =
        List.fold_left (fun (col,rest) -> function
            | hd::tl -> hd::col, tl::rest
            | [] -> ""::col, []::rest)
          ([],[]) ll
      in
      List.rev col::transpose (List.rev rest)
    in
    let columns = transpose ll in
    let pad n s =
      let sn = visual_length s in
      if sn >= n then s
      else s ^ (String.make (n - sn) ' ')
    in
    let pad_multi n s =
      match OpamString.split s '\n' with
      | [] | [_] -> pad n s ^"\n"
      | ls -> String.concat "\n" (List.map (pad n) ls)
    in
    let align sl =
      let (len, multiline) =
        List.fold_left (fun (len,ml) s ->
            if String.contains s '\n' then max len (visual_width s), true
            else max len (visual_length s), ml)
          (0, false) sl
      in
      List.map (if multiline then pad_multi len else pad len) sl
    in
    let rec map_but_last f = function
      | ([] | [_]) as l -> l
      | x::r -> f x :: map_but_last f r
    in
    transpose (map_but_last align columns)

  let reformat
      ?(start_column=0) ?(indent=0) ?(width=OpamSys.terminal_columns ()) s =
    let slen = String.length s in
    let buf = Buffer.create 1024 in
    let rec find_nonsp i =
      if i >= slen then i else
      match s.[i] with ' ' -> find_nonsp (i+1) | _ -> i
    in
    let rec find_split i =
      if i >= slen then i else
      match s.[i] with ' ' | '\n' -> i | _ -> find_split (i+1)
    in
    let newline i =
      Buffer.add_char buf '\n';
      if i+1 < slen && s.[i+1] <> '\n' then
        for _i = 1 to indent do Buffer.add_char buf ' ' done
    in
    let rec print i col =
      if i >= slen then () else
      if s.[i] = '\n' then (newline i; print (i+1) indent) else
      let j = find_nonsp i in
      let k = find_split j in
      let len_visual = visual_length_substring s i (k - i) in
      if col + len_visual >= width && col > indent then
        (newline i;
         Buffer.add_substring buf s j (k - j);
         print k (indent + len_visual - j + i))
      else
        (Buffer.add_substring buf s i (k - i);
         print k (col + len_visual))
    in
    print 0 start_column;
    Buffer.contents buf

  let itemize ?(bullet="  - ") f =
    let indent = visual_length bullet in
    OpamList.concat_map ~left:bullet ~right:"\n" ~nil:"" ("\n"^bullet)
      (fun s -> reformat ~start_column:indent ~indent (f s))

  let rec pretty_list ?(last="and") = function
    | []    -> ""
    | [a]   -> a
    | [a;b] -> Printf.sprintf "%s %s %s" a last b
    | h::t  -> Printf.sprintf "%s, %s" h (pretty_list ~last t)

  let as_aligned_table ?(width=OpamSys.terminal_columns ()) l =
    let itlen =
      List.fold_left (fun acc s -> max acc (visual_length s))
        0 l
    in
    let by_line = (width + 1) / (itlen + 1) in
    if by_line <= 1 then
      List.map (fun x -> [x]) l
    else
    let rec aux rline n = function
      | [] -> [List.rev rline]
      | x::r as line ->
        if n = 0 then List.rev rline :: aux [] by_line line
        else aux (x :: rline) (n-1) r
    in
    align_table (aux [] by_line l)

end


module Exn = struct

  let fatal = fatal

  let register_backtrace, get_backtrace =
    let registered_backtrace = ref None in
    (fun e ->
       registered_backtrace :=
         match !registered_backtrace with
         | Some (e1, _) as reg when e1 == e -> reg
         | _ -> Some (e, Printexc.get_backtrace ())),
    (fun e ->
       match !registered_backtrace with
       | Some(e1,bt) when e1 == e -> bt
       | _ -> Printexc.get_backtrace ())

  let pretty_backtrace e =
    match get_backtrace e with
    | "" -> ""
    | b  ->
      let b =
        OpamFormat.itemize ~bullet:"  " (fun x -> x) (OpamString.split b '\n')
      in
      Printf.sprintf "Backtrace:\n%s" b

  let finalise e f =
    let bt = Printexc.get_raw_backtrace () in
    f ();
    Printexc.raise_with_backtrace e bt

  let finally f k =
    match k () with
    | r -> f (); r
    | exception e -> finalise e f
end


module Op = struct

  let (@@) f x = f x

  let (|>) x f = f x

  let (@*) g f x = g (f x)

  let (@>) f g x = g (f x)

end


module Config = struct

  module type Sig = sig

    type t
    type 'a options_fun

    val default: t
    val set: t -> (unit -> t) options_fun
    val setk: (t -> 'a) -> t -> 'a options_fun
    val r: t ref
    val update: ?noop:_ -> (unit -> unit) options_fun
    val init: ?noop:_ -> (unit -> unit)  options_fun
    val initk: 'a -> 'a options_fun

  end

  type env_var = string

  type when_ = [ `Always | `Never | `Auto ]
  type when_ext = [ `Extended | when_ ]
  type answer = [ `unsafe_yes | `all_yes | `all_no | `ask ]
  type yes_answer = [ `unsafe_yes | `all_yes ]

  let env conv var =
    try Option.map conv (Env.getopt ("OPAM"^var))
    with Failure _ ->
      flush stdout;
      !console.warning
        "Invalid value for environment variable OPAM%s, ignored." var;
      None

  let bool_of_string s =
    match String.lowercase_ascii s with
    | "" | "0" | "no" | "false" -> Some false
    | "1" | "yes" | "true" -> Some true
    | _ -> None

  let bool s =
    match bool_of_string s with
    | Some s -> s
    | None -> failwith "env_bool"

  let env_bool var = env bool var

  let env_int var = env int_of_string var

  type level = int
  let env_level var =
    env (function s ->
        if s = "" then 0 else
        match bool_of_string s with
        | Some true -> 0
        | Some false -> 1
        | None -> int_of_string s)
      var

  type sections = int option OpamString.Map.t
  let env_sections var =
    env (fun s ->
      let f map elt =
        let parse_value (section, value) =
          try
            (section, Some (int_of_string value))
          with Failure _ ->
            (section, None)
        in
        let (section, level) =
          Option.map_default parse_value (elt, None) (OpamString.cut_at elt ':')
        in
        OpamString.Map.add section level map
      in
      List.fold_left f OpamString.Map.empty (OpamString.split s ' ')) var

  let env_string var =
    env (fun s -> s) var

  let env_float var =
    env float_of_string var

  let when_ext s =
    match String.lowercase_ascii s with
    | "extended" -> `Extended
    | "always" -> `Always
    | "never" -> `Never
    | "auto" -> `Auto
    | _ -> failwith "env_when"

  let env_when_ext var = env when_ext var

  let env_when var =
    env (fun v -> match when_ext v with
        | (`Always | `Never | `Auto) as w -> w
        | `Extended -> failwith "env_when")
      var

  let resolve_when ~auto = function
    | `Always -> true
    | `Never -> false
    | `Auto -> Lazy.force auto

  let answer s =
    match String.lowercase_ascii s with
    | "ask" -> `ask
    | "yes" -> `all_yes
    | "no" -> `all_no
    | "unsafe-yes" -> `unsafe_yes
    | _ -> failwith "env_answer"

  let env_answer =
    env (fun s ->
        try if bool s then `all_yes else `all_no
        with Failure _ -> answer s)


  module E = struct
    type t = ..
    type t += REMOVED
    let (r : t list ref) = ref []

    let update v = r := v :: !r
    let updates l = r := l @ !r

    let find var = OpamList.find_map var !r
    let value_t var = try Some (find var) with Not_found -> None
    let value var =
      let l = lazy (value_t var) in
      fun () -> Lazy.force l
  end

end

module List = OpamList
module String = OpamString
module Sys = OpamSys
module Format = OpamFormat
module Compare = OpamCompare

module Re = Core

exception Parse_error = Perl.Parse_error
exception Not_supported = Perl.Not_supported

type regexp = Re.re

type flag =
  [ `CASELESS
  | `MULTILINE
  | `ANCHORED
  | `DOTALL
  ]

type split_result =
  | Text of string
  | Delim of string
  | Group of int * string
  | NoGroup

type groups = Core.Group.t

let re ?(flags = []) pat =
  let opts =
    List.map
      (function
        | `CASELESS -> `Caseless
        | `MULTILINE -> `Multiline
        | `ANCHORED -> `Anchored
        | `DOTALL -> `Dotall)
      flags
  in
  Perl.re ~opts pat
;;

let regexp ?flags pat = Re.compile (re ?flags pat)
let extract ~rex s = Re.Group.all (Re.exec rex s)
let exec ~rex ?pos s = Re.exec rex ?pos s
let get_substring s i = Re.Group.get s i
let names rex = Re.group_names rex |> List.map fst |> Array.of_list

let get_named_substring rex name s =
  let rec loop = function
    | [] -> raise Not_found
    | (n, i) :: rem when n = name ->
      (try get_substring s i with
       | Not_found -> loop rem)
    | _ :: rem -> loop rem
  in
  loop (Re.group_names rex)
;;

let get_substring_ofs s i = Re.Group.offset s i
let pmatch ~rex s = Re.execp rex s

let substitute ~rex ~subst str =
  let b = Buffer.create 1024 in
  let rec loop pos on_match =
    if Re.execp ~pos rex str
    then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.Group.offset ss 0 in
      if on_match && start = pos && start = fin
      then (
        if (* Empty match following a match *)
           pos < String.length str
        then (
          Buffer.add_char b str.[pos];
          loop (pos + 1) false))
      else (
        let pat = Re.Group.get ss 0 in
        Buffer.add_substring b str pos (start - pos);
        Buffer.add_string b (subst pat);
        if start = fin
        then (
          if (* Manually advance by one after an empty match *)
             fin < String.length str
          then (
            Buffer.add_char b str.[fin];
            loop (fin + 1) false))
        else loop fin true))
    else Buffer.add_substring b str pos (String.length str - pos)
  in
  loop 0 false;
  Buffer.contents b
;;

let split ~rex str =
  let finish str last accu =
    let accu = String.sub str last (String.length str - last) :: accu in
    List.rev accu
  in
  let rec loop accu last pos on_match =
    if Re.execp ~pos rex str
    then (
      let ss = Re.exec ~pos rex str in
      let start, fin = Re.Group.offset ss 0 in
      if on_match && start = pos && start = fin
      then
        if (* Empty match following a match *)
           pos = String.length str
        then finish str last accu
        else loop accu last (pos + 1) false
      else (
        let accu = String.sub str last (start - last) :: accu in
        if start = fin
        then
          if (* Manually advance by one after an empty match *)
             fin = String.length str
          then finish str fin accu
          else loop accu fin (fin + 1) false
        else loop accu fin fin true))
    else finish str last accu
  in
  loop [] 0 0 false
;;

(* From PCRE *)
let string_unsafe_sub s ofs len =
  let r = Bytes.create len in
  Bytes.unsafe_blit s ofs r 0 len;
  Bytes.unsafe_to_string r
;;

let quote s =
  let len = String.length s in
  let buf = Bytes.create (len lsl 1) in
  let pos = ref 0 in
  for i = 0 to len - 1 do
    match String.unsafe_get s i with
    | ('\\' | '^' | '$' | '.' | '[' | '|' | '(' | ')' | '?' | '*' | '+' | '{') as c ->
      Bytes.unsafe_set buf !pos '\\';
      incr pos;
      Bytes.unsafe_set buf !pos c;
      incr pos
    | c ->
      Bytes.unsafe_set buf !pos c;
      incr pos
  done;
  string_unsafe_sub buf 0 !pos
;;

let full_split ?(max = 0) ~rex s =
  if String.length s = 0
  then []
  else if max = 1
  then [ Text s ]
  else (
    let results = Re.split_full rex s in
    let matches =
      List.map
        (function
          | `Text s -> [ Text s ]
          | `Delim d ->
            let matches = Re.Group.all_offset d in
            let delim = Re.Group.get d 0 in
            Delim delim
            ::
            (let l = ref [] in
             for i = 1 to Array.length matches - 1 do
               l
               := (if matches.(i) = (-1, -1) then NoGroup else Group (i, Re.Group.get d i))
                  :: !l
             done;
             List.rev !l))
        results
    in
    List.concat matches)
;;

type substrings = Group.t

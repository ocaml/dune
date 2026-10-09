(* NOTE: this file contains local modifications with respect to upstream
   (see vendor/update-build_path_prefix_map.sh): [decode_prefix] accepts
   unescaped ':' characters and [decode_map] rejoins segments that contain
   no '=' separator (merging a drive-letter prefix with the entry that
   follows it), so that maps containing Windows drive letters such as
   "C:\foo" can be decoded. *)
type path = string
type path_prefix = string
type error_message = string

let errorf fmt = Printf.ksprintf (fun err -> Error err) fmt

let encode_prefix str =
  let buf = Buffer.create (String.length str) in
  let push_char = function
    | '%' -> Buffer.add_string buf "%#"
    | '=' -> Buffer.add_string buf "%+"
    | ':' -> Buffer.add_string buf "%."
    | c -> Buffer.add_char buf c
  in
  String.iter push_char str;
  Buffer.contents buf

let decode_prefix str =
  let buf = Buffer.create (String.length str) in
  let rec loop i =
    if i >= String.length str
    then Ok (Buffer.contents buf)
    else match str.[i] with
      | '=' ->
        errorf "invalid character '=' in key or value"
      | '%' ->
        let push c = Buffer.add_char buf c; loop (i + 2) in
        if i + 1 = String.length str then
          errorf "invalid encoded string %S (trailing '%%')" str
        else begin match str.[i + 1] with
            | '#' -> push '%'
            | '+' -> push '='
            | '.' -> push ':'
            | c -> errorf "invalid %%-escaped character '%c'" c
        end
      | c ->
        Buffer.add_char buf c;
        loop (i + 1)
  in loop 0

type pair = { target: path_prefix; source : path_prefix }

let encode_pair { target; source } =
  String.concat "=" [encode_prefix target; encode_prefix source]

let decode_pair str =
  match String.index str '=' with
  | exception Not_found ->
    errorf "invalid key/value pair %S, no '=' separator" str
  | equal_pos ->
    let encoded_target = String.sub str 0 equal_pos in
    let encoded_source =
      String.sub str (equal_pos + 1) (String.length str - equal_pos - 1) in
    match decode_prefix encoded_target, decode_prefix encoded_source with
    | Ok target, Ok source -> Ok { target; source }
    | ((Error _ as err), _) | (_, (Error _ as err)) -> err

type map = pair option list

let encode_map map =
  let encode_elem = function
    | None -> ""
    | Some pair -> encode_pair pair
  in
  List.map encode_elem map
  |> String.concat ":"

let decode_map str =
  let exception Shortcut of error_message in
  let decode_or_empty = function
    | "" -> None
    | pair ->
      begin match decode_pair pair with
        | Ok str -> Some str
        | Error err -> raise (Shortcut err)
      end
  in
  (* An unescaped ':' inside a key/value pair (e.g. the drive letter of a
     Windows path such as "C:\foo") is split by [String.split_on_char].
     Rejoin the segments that contain no '=' separator with their neighbour
     so that such pairs can be decoded. This is a leniency for maps that do
     not follow the recommended encoding; properly encoded maps are
     unaffected. A ':' at the very end of a value is ambiguous with an empty
     entry and must be encoded (e.g. "C%."). A drive letter at the start of
     a non-initial entry is merged with the entry that follows it (see
     [merge_drive_letters]); other ':' in the target of a non-initial entry
     must be encoded. *)
  (* A drive-letter prefix (a single letter followed by a native path, e.g.
     "C" in "C:\work=src") is split from its entry by
     [String.split_on_char]. If the entry is not the first one, the letter
     would be absorbed into the previous pair's value, so merge it with the
     segment that follows it (a native path containing an '='). *)
  let merge_drive_letters lst =
    let is_drive_letter s =
      String.length s = 1
      &&
      let c = s.[0] in
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    in
    let is_native_entry s =
      String.length s > 0
      && (s.[0] = '\\' || s.[0] = '/')
      && String.contains s '='
    in
    let rec loop = function
      | [] | [ _ ] as l -> l
      | x :: y :: rest ->
        if is_drive_letter x && is_native_entry y
        then (x ^ ":" ^ y) :: loop rest
        else x :: loop (y :: rest)
    in
    loop lst
  in
  let split_pairs str =
    (* Walk the ':'-split segments, accumulating entries in reverse. A
       segment containing '=' opens a pair; a segment without '=' continues
       the nearest preceding entry (before any pair it is a target prefix
       and joins the pair that follows it). Empty segments are preserved as
       empty entries. *)
    let rec loop acc = function
      | [] -> List.rev acc
      | "" :: rest -> loop ("" :: acc) rest
      | seg :: rest ->
        if String.contains seg '='
        then
          (* join a pending target prefix, if any *)
          let rec join_prefix = function
            | e :: tl when e <> "" && not (String.contains e '=') ->
              Some ((e ^ ":" ^ seg) :: tl)
            | x :: tl ->
              (match join_prefix tl with
               | Some tl -> Some (x :: tl)
               | None -> None)
            | [] -> None
          in
          (match join_prefix acc with
           | Some acc -> loop acc rest
           | None -> loop (seg :: acc) rest)
        else
          (* continue the nearest preceding entry *)
          let rec join_prev = function
            | e :: tl when e <> "" -> Some ((e ^ ":" ^ seg) :: tl)
            | x :: tl ->
              (match join_prev tl with
               | Some tl -> Some (x :: tl)
               | None -> None)
            | [] -> None
          in
          (match join_prev acc with
           | Some acc -> loop acc rest
           | None -> loop (seg :: acc) rest)
    in
    loop [] (merge_drive_letters (String.split_on_char ':' str))
  in
  match List.map decode_or_empty (split_pairs str) with
  | exception (Shortcut err) -> Error err
  | map -> Ok map
;;

let rewrite_opt prefix_map path =
  let is_prefix = function
    | None -> false
    | Some { target = _; source } ->
      String.length source <= String.length path
      && String.equal source (String.sub path 0 (String.length source))
  in
  match
    List.find is_prefix
      (* read key/value pairs from right to left, as the spec demands *)
      (List.rev prefix_map)
  with
  | exception Not_found -> None
  | None -> None
  | Some { source; target } ->
      Some (target ^ (String.sub path (String.length source)
                       (String.length path - String.length source)))

let rewrite prefix_map path =
  match rewrite_opt prefix_map path with
  | None -> path
  | Some path -> path

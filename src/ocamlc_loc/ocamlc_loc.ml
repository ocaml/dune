module Re = Dune_re

type severity =
  | Error
  | Warning of
      { code : int
      ; name : string
      }

type message =
  | Raw of string
  | Structured of
      { preview : string option
      ; message : string
      ; severity : severity
      }

type loc =
  { path : string
  ; line : [ `Single of int | `Range of int * int ]
  ; chars : (int * int) option
  }

type report =
  { loc : loc
  ; message : message
  ; related : (loc * message) list
  }

let re =
  lazy
    (let open Re in
    let path = rep (compl [ char '"' ]) in
    let number = group (rep1 digit) in
    let range = seq [ number; char '-'; number ] in
    let chars = seq [ str "characters"; rep1 space; range ] in
    let file = seq [ str "File "; char '"'; group path; char '"'; char ',' ] in
    let single_marker, line = mark (seq [ str "line"; rep1 space; number ]) in
    let range_marker, lines = mark (seq [ str "lines"; rep1 space; range ]) in
    let re =
      seq
        [ file
        ; rep1 space
        ; alt [ line; lines ]
        ; opt (seq [ char ','; rep1 space; chars ])
        ; char ':'
        ; rep space
        ]
    in
    (Re.compile re, single_marker, range_marker))

let message_re =
  lazy
    (let open Re in
    let error_marker, error = mark (str "Error") in
    let warning =
      seq
        [ str "Warning "
        ; group (rep1 digit)
        ; rep1 space
        ; char '['
        ; group (rep1 (compl [ char ']' ]))
        ; char ']'
        ]
    in
    let severity = seq [ alt [ bol; bos ]; alt [ error; warning ]; char ':' ] in
    let re =
      seq
        [ opt (group (rep1 any))
        ; severity
        ; rep space
        ; group (rep any)
        ; rep space
        ]
    in
    (compile re, error_marker))

let parse_message msg =
  let re, error_marker = Lazy.force message_re in
  match Re.exec re msg with
  | exception Not_found -> Raw msg
  | group ->
    let preview =
      if Re.Group.test group 1 then
        Some (Re.Group.get group 1)
      else
        None
    in
    let severity =
      if Re.Mark.test group error_marker then
        Error
      else
        let code = int_of_string (Re.Group.get group 2) in
        let name = Re.Group.get group 3 in
        Warning { code; name }
    in
    Structured { preview; severity; message = Re.Group.get group 4 }

let parse s : report list =
  let re, single_marker, _ = Lazy.force re in
  match Re.split_full re s with
  | [] -> []
  | [ `Text _ ] -> []
  | (`Delim _ :: _ as rest)
  | `Text _ :: rest ->
    let loc_of_group group message =
      let str_group = Re.Group.get group in
      let int_group i = int_of_string (str_group i) in
      let line =
        if Re.Mark.test group single_marker then
          `Single (int_group 2)
        else
          `Range (int_group 3, int_group 4)
      in
      let chars =
        if Re.Group.test group 5 then
          Some (int_group 5, int_group 6)
        else
          None
      in
      let message = parse_message message in
      { loc = { path = str_group 1; line; chars }; message; related = [] }
    in
    let rec loop acc = function
      | `Text _ :: _ -> assert false
      | `Delim _ :: `Delim _ :: _ -> assert false
      | `Delim g :: `Text m :: rest ->
        let loc = loc_of_group g m in
        loop (loc :: acc) rest
      | [ `Delim g ] ->
        let loc = loc_of_group g "" in
        loc :: acc
      | [] -> acc
    in
    List.rev (loop [] rest)

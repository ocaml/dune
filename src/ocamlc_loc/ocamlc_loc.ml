module Re = Dune_re
module Dyn = Stdune.Dyn

type severity =
  | Error
  | Warning of
      { code : int option (* codes are available starting 4.12 *)
      ; name : string
      }

type message =
  | Raw of string
  | Structured of
      { file_excerpt : string option
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

let dyn_of_severity =
  let open Dyn.Encoder in
  function
  | Error -> constr "Error" []
  | Warning { code; name } ->
    constr "Warning"
      [ record [ ("code", (option int) code); ("name", string name) ] ]

let dyn_of_message =
  let open Dyn.Encoder in
  function
  | Raw s -> constr "Raw" [ string s ]
  | Structured { file_excerpt; message; severity } ->
    constr "Structured"
      [ record
          [ ("file_excerpt", (option string) file_excerpt)
          ; ("message", string message)
          ; ("severity", dyn_of_severity severity)
          ]
      ]

let dyn_of_loc { path; line; chars } =
  let open Dyn.Encoder in
  record
    [ ("path", string path)
    ; ( "line"
      , match line with
        | `Single i -> constr "Single" [ int i ]
        | `Range (i, j) -> constr "Range" [ int i; int j ] )
    ; ("chars", option (pair int int) chars)
    ]

let dyn_of_report { loc; message; related } =
  let open Dyn.Encoder in
  record
    [ ("loc", dyn_of_loc loc)
    ; ("message", dyn_of_message message)
    ; ("related", list (pair dyn_of_loc dyn_of_message) related)
    ]

let re =
  lazy
    (let open Re in
    let path = rep (compl [ char '"' ]) in
    let number = group (rep1 digit) in
    let range = seq [ number; char '-'; number ] in
    let chars = seq [ str "characters"; rep1 space; range ] in
    let file = seq [ str "File "; char '"'; group path; char '"'; char ',' ] in
    let single_marker, line = mark (seq [ str "line"; rep1 space; number ]) in
    let lines = seq [ str "lines"; rep1 space; range ] in
    let related_marker, related_space = mark (seq [ char '\n'; rep1 blank ]) in
    let re =
      seq
        [ opt related_space
        ; file
        ; rep1 space
        ; alt [ line; lines ]
        ; opt (seq [ char ','; rep1 space; chars ])
        ; char ':'
        ; rep space
        ]
    in
    (Re.compile re, single_marker, related_marker))

let message_re =
  lazy
    (let open Re in
    let error_marker, error = mark (str "Error") in
    let warning =
      seq
        [ str "Warning "
        ; group (rep1 digit)
        ; opt
            (seq
               [ rep1 space
               ; char '['
               ; group (rep1 (compl [ char ']' ]))
               ; char ']'
               ])
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
    let file_excerpt =
      if Re.Group.test group 1 then
        Some (Re.Group.get group 1)
      else
        None
    in
    let severity =
      if Re.Mark.test group error_marker then
        Error
      else
        let code =
          if Re.Group.test group 2 then
            Some (int_of_string (Re.Group.get group 2))
          else
            None
        in
        let name = Re.Group.get group 3 in
        Warning { code; name }
    in
    Structured { file_excerpt; severity; message = Re.Group.get group 4 }

let parse s =
  let re, single_marker, related_marker = Lazy.force re in
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
      let res = ({ path = str_group 1; line; chars }, message) in
      if Re.Mark.test group related_marker then
        `Related res
      else
        `Parent res
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

let parse s =
  let rec loop acc current = function
    | [] -> current_to_acc acc current
    | `Parent (loc, message) :: xs ->
      let acc = current_to_acc acc current in
      let current = `Accumulating { related = []; loc; message } in
      loop acc current xs
    | `Related (loc, message) :: xs ->
      let current =
        match current with
        | `None -> assert false
        | `Accumulating p ->
          `Accumulating { p with related = (loc, message) :: p.related }
      in
      loop acc current xs
  and current_to_acc acc current =
    match current with
    | `None -> acc
    | `Accumulating p -> { p with related = List.rev p.related } :: acc
  in
  let components = parse s in
  List.rev (loop [] `None components)

open Import

type bindings = string * string

type section =
  { name : string
  ; arg : string option
  ; bindings : bindings list
  }

type t = section list

let strip =
  let re = Re.(compile (alt [ seq [ bos; rep blank ]; seq [ rep blank; eos ] ])) in
  Re.replace_string ~all:true re ~by:""
;;

let parse_section_bindings =
  let binding_name = Re.(seq [ alpha; rep alnum ]) in
  let binding_assignment_re =
    Re.(
      compile
      @@ seq
           [ bol
           ; rep space
           ; group binding_name
           ; rep space
           ; char '='
           ; rep space
           ; group (rep1 any)
           ; rep space
           ; eol
           ])
  in
  let binding_boolean_re =
    Re.(compile @@ seq [ bol; rep space; group binding_name; rep space; eol ])
  in
  fun lines ->
    List.filter_map lines ~f:(fun line ->
      let binding_assignment =
        Re.exec_opt binding_assignment_re line
        |> Option.map ~f:(fun m ->
          let name = Re.Group.get m 1 in
          let value = Re.Group.get m 2 in
          name, value)
      in
      match binding_assignment with
      | Some _ as v -> v
      | None ->
        Re.exec_opt binding_boolean_re line
        |> Option.map ~f:(fun m ->
          let name = Re.Group.get m 1 in
          name, "true"))
;;

let parse_section_header =
  let identifier = Re.(rep1 (alt [ alnum; char '.'; char '-' ])) in
  let section_re =
    Re.(
      compile
      @@ seq
           [ char '['
           ; group identifier
           ; alt [ char ']'; seq [ rep1 blank; char '"'; group (rep1 any); str "\"]" ] ]
           ])
  in
  fun line ->
    Re.exec_opt section_re line
    |> Option.map ~f:(fun m ->
      (* names are case-insensitive, thus normalizing *)
      let section_name = Re.Group.get m 1 |> String.lowercase in
      section_name, Re.Group.get_opt m 2)
;;

let rec parse_sections acc lines =
  match lines with
  | [] -> List.rev acc
  | header :: rest ->
    (match parse_section_header header with
     | Some (name, arg) ->
       let this, rest =
         List.split_while rest ~f:(fun line ->
           not @@ String.is_prefix ~prefix:"[" @@ strip line)
       in
       let bindings = parse_section_bindings this in
       let section = { name; arg; bindings } in
       parse_sections (section :: acc) rest
     | None ->
       (* not a header, skip line *)
       parse_sections acc rest)
;;

let parse s =
  let lines = String.split ~on:'\n' s in
  parse_sections [] lines |> Result.ok
;;

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
  let binding_re =
    Re.(
      compile
      @@ seq [ rep space; group (rep1 (diff any (char '='))); char '='; group (rep1 any) ])
  in
  fun lines ->
    List.filter_map lines ~f:(fun line ->
      Re.exec_opt binding_re line
      |> Option.map ~f:(fun m ->
        let name = Re.Group.get m 1 |> strip in
        let value = Re.Group.get m 2 |> strip in
        name, value))
;;

let parse_section_header =
  let identifier = Re.(rep1 (diff any blank)) in
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
    |> Option.map ~f:(fun m -> Re.Group.get m 1, Re.Group.get_opt m 2)
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

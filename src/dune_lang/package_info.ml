open Import

type t =
  { source : Source_kind.t option
  ; license : string list option
  ; authors : string list option
  ; homepage : string option
  ; bug_reports : string option
  ; documentation : string option
  ; maintainers : string list option
  ; maintenance_intent : string list option
  }

let source t = t.source
let license t = t.license
let authors t = t.authors

let homepage t =
  match t.homepage, t.source with
  | None, Some (Host h) -> Some (Source_kind.Host.homepage h)
  | s, _ -> s
;;

let bug_reports t =
  match t.bug_reports, t.source with
  | None, Some (Host h) -> Some (Source_kind.Host.bug_reports h)
  | s, _ -> s
;;

let documentation t = t.documentation
let maintainers t = t.maintainers
let maintenance_intent t = t.maintenance_intent

let empty =
  { source = None
  ; license = None
  ; authors = None
  ; homepage = None
  ; bug_reports = None
  ; documentation = None
  ; maintainers = None
  ; maintenance_intent = None
  }
;;

let example ~authors ~maintainers ~license =
  { source =
      Some (Host (Source_kind.Host.Github { user = "username"; repo = "reponame" }))
  ; license = Some (Option.value license ~default:[ "LICENSE" ])
  ; authors = Some (Option.value authors ~default:[ "Author Name <author@example.com>" ])
  ; maintainers =
      Some
        (Option.value maintainers ~default:[ "Maintainer Name <maintainer@example.com>" ])
  ; documentation =
      Some "https://url/to/documentation"
      (* homepage and bug_reports are inferred from the source *)
  ; homepage = None
  ; bug_reports = None
  ; maintenance_intent = None
  }
;;

let to_dyn
      { source
      ; license
      ; authors
      ; homepage
      ; bug_reports
      ; documentation
      ; maintainers
      ; maintenance_intent
      }
  =
  let open Dyn in
  record
    [ "source", (option Source_kind.to_dyn) source
    ; "license", (option (list string)) license
    ; "homepage", (option string) homepage
    ; "documentation", (option string) documentation
    ; "bug_reports", (option string) bug_reports
    ; "maintainers", option (list string) maintainers
    ; "maintenance_intent", option (list string) maintenance_intent
    ; "authors", option (list string) authors
    ]
;;

let encode_fields
      { source
      ; authors
      ; license
      ; homepage
      ; documentation
      ; bug_reports
      ; maintainers
      ; maintenance_intent
      }
  =
  let open Encoder in
  record_fields
    [ field_o "source" Source_kind.encode source
    ; field_l "authors" string (Option.value ~default:[] authors)
    ; field_l "maintainers" string (Option.value ~default:[] maintainers)
    ; field_l "maintenance_intent" string (Option.value ~default:[] maintenance_intent)
    ; field_l "license" string (Option.value ~default:[] license)
    ; field_o "homepage" string homepage
    ; field_o "documentation" string documentation
    ; field_o "bug_reports" string bug_reports
    ]
;;

let maintenance_intent_list = [ "any"; "latest"; "none" ]

let rec pp_or_list () = function
  | [] -> ""
  | [ x ] -> x
  | [ x; y ] -> sprintf "%S or %S" x y
  | x :: xs -> sprintf "%S, %a" x pp_or_list xs
;;

let valid_maintenance_intent =
  let open Decoder in
  map_validate (located string) ~f:(fun (loc, str) ->
    let rec parse_part i =
      if i >= String.length str
      then if i > 0 then Error "version ends with a dot" else Error "empty version"
      else (
        match str.[i] with
        | '(' -> parse_token (i + 1) (i + 1)
        | '.' -> Error "unexpected dot"
        | _ -> inside_part (i + 1))
    and inside_part i =
      if i >= String.length str
      then Ok ()
      else (
        match str.[i] with
        | '.' -> parse_part (i + 1)
        | '(' | ')' -> Error "unexpected parenthesis"
        | _ -> inside_part (i + 1))
    and parse_token start i =
      if i >= String.length str
      then Error "unclosed parenthesis"
      else (
        match str.[i] with
        | ')' ->
          let token = String.sub str ~pos:start ~len:(i - start) in
          if List.mem ~equal:String.equal maintenance_intent_list token
          then after_token (i + 1)
          else
            Error
              (sprintf
                 "unknown intent %S, expected %a"
                 token
                 pp_or_list
                 maintenance_intent_list)
        | '-' ->
          let token = String.sub str ~pos:start ~len:(i - start) in
          if String.equal token "latest"
          then parse_num (i + 1) (i + 1)
          else Error (sprintf "substraction only allowed for \"latest\", not %S" token)
        | _ -> parse_token start (i + 1))
    and parse_num start i =
      if i >= String.length str
      then Error "unclosed parenthesis"
      else (
        match str.[i] with
        | ')' when i > start -> after_token (i + 1)
        | '0' when i > start -> parse_num start (i + 1)
        | '0' -> parse_num (i + 1) (i + 1)
        | '1' .. '9' -> parse_num start (i + 1)
        | _ -> Error "invalid substraction")
    and after_token i =
      if i >= String.length str
      then Ok ()
      else (
        match str.[i] with
        | '.' -> parse_part (i + 1)
        | _ -> Error "missing dot after intent")
    in
    match parse_part 0 with
    | Ok () -> Ok str
    | Error msg -> Error (User_error.make ~loc [ Pp.text msg ]))
;;

let decode_maintenance_intent =
  let open Decoder in
  Syntax.since Stanza.syntax (3, 18) >>> repeat valid_maintenance_intent
;;

let decode ?since () =
  let open Decoder in
  let v default = Option.value since ~default in
  let+ source =
    field_o "source" (Syntax.since Stanza.syntax (v (1, 7)) >>> Source_kind.decode)
  and+ authors =
    field_o "authors" (Syntax.since Stanza.syntax (v (1, 9)) >>> repeat string)
  and+ license =
    field_o
      "license"
      (Syntax.since Stanza.syntax (v (1, 9))
       >>> let* l = repeat1 string in
           (if List.length l > 1
            then Syntax.since ~what:"Parsing several licenses" Stanza.syntax (v (3, 2))
            else return ())
           >>> return l)
  and+ homepage = field_o "homepage" (Syntax.since Stanza.syntax (v (1, 10)) >>> string)
  and+ documentation =
    field_o "documentation" (Syntax.since Stanza.syntax (v (1, 10)) >>> string)
  and+ bug_reports =
    field_o "bug_reports" (Syntax.since Stanza.syntax (v (1, 10)) >>> string)
  and+ maintainers =
    field_o "maintainers" (Syntax.since Stanza.syntax (v (1, 10)) >>> repeat string)
  and+ maintenance_intent = field_o "maintenance_intent" decode_maintenance_intent in
  { source
  ; authors
  ; license
  ; homepage
  ; documentation
  ; bug_reports
  ; maintainers
  ; maintenance_intent
  }
;;

let superpose t1 t2 =
  let f o1 o2 =
    match o2 with
    | Some _ as x -> x
    | None -> o1
  in
  { source = f t1.source t2.source
  ; authors = f t1.authors t2.authors
  ; license = f t1.license t2.license
  ; homepage = f t1.homepage t2.homepage
  ; documentation = f t1.documentation t2.documentation
  ; bug_reports = f t1.bug_reports t2.bug_reports
  ; maintainers = f t1.maintainers t2.maintainers
  ; maintenance_intent = f t1.maintenance_intent t2.maintenance_intent
  }
;;

let create
      ~maintainers
      ~maintenance_intent
      ~authors
      ~homepage
      ~bug_reports
      ~documentation
      ~license
      ~source
  =
  { maintainers
  ; authors
  ; homepage
  ; bug_reports
  ; documentation
  ; license
  ; source
  ; maintenance_intent
  }
;;

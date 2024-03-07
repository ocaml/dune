open Stdune
open Dune_sexp

type t =
  { source : Source_kind.t option
  ; license : string list option
  ; authors : string list option
  ; homepage : string option
  ; bug_reports : string option
  ; documentation : string option
  ; maintainers : string list option
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

let empty =
  { source = None
  ; license = None
  ; authors = None
  ; homepage = None
  ; bug_reports = None
  ; documentation = None
  ; maintainers = None
  }
;;

let example =
  { source =
      Some (Host { kind = Source_kind.Host.Github; user = "username"; repo = "reponame" })
  ; license = Some [ "LICENSE" ]
  ; authors = Some [ "Author Name" ]
  ; maintainers = Some [ "Maintainer Name" ]
  ; documentation =
      Some "https://url/to/documentation"
      (* homepage and bug_reports are inferred from the source *)
  ; homepage = None
  ; bug_reports = None
  }
;;

let to_dyn { source; license; authors; homepage; bug_reports; documentation; maintainers }
  =
  let open Dyn in
  record
    [ "source", (option Source_kind.to_dyn) source
    ; "license", (option (list string)) license
    ; "homepage", (option string) homepage
    ; "documentation", (option string) documentation
    ; "bug_reports", (option string) bug_reports
    ; "maintainers", option (list string) maintainers
    ; "authors", option (list string) authors
    ]
;;

let encode_fields
  { source; authors; license; homepage; documentation; bug_reports; maintainers }
  =
  let open Encoder in
  record_fields
    [ field_o "source" Source_kind.encode source
    ; field_l "authors" string (Option.value ~default:[] authors)
    ; field_l "maintainers" string (Option.value ~default:[] maintainers)
    ; field_l "license" string (Option.value ~default:[] license)
    ; field_o "homepage" string homepage
    ; field_o "documentation" string documentation
    ; field_o "bug_reports" string bug_reports
    ]
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
  in
  { source; authors; license; homepage; documentation; bug_reports; maintainers }
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
  }
;;

let create ~maintainers ~authors ~homepage ~bug_reports ~documentation ~license ~source =
  { maintainers; authors; homepage; bug_reports; documentation; license; source }
;;

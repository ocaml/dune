open Import
module Sexp = Stdune.Dune_sexp

let atom =
  Conv.iso_result
    Conv.string
    (fun string ->
       match Sexp.Atom.parse string with
       | Some atom -> Ok atom
       | None -> Error (Failure "invalid Dune atom"))
    Sexp.Atom.to_string
;;

let pform =
  let open Conv in
  let payload =
    iso string Sexp.Template.Pform.Payload.of_string Sexp.Template.Pform.Payload.to_string
  in
  record
    (Record.make (fun name payload ->
       { Sexp.Template.Pform.loc = Loc.none; name; payload })
     |> Record.field "name" (required string) ~get:Sexp.Template.Pform.name
     |> Record.field "payload" (optional payload) ~get:Sexp.Template.Pform.payload
     |> Record.finish)
;;

let template_part =
  let open Conv in
  let text = constr "text" string (fun text -> Sexp.Template.Part.Text text) in
  let pform = constr "pform" pform (fun pform -> Sexp.Template.Part.Pform pform) in
  sum
    [ econstr text; econstr pform ]
    (function
      | Sexp.Template.Part.Text value -> case value text
      | Pform value -> case value pform)
;;

let template =
  let open Conv in
  record
    (Record.make (fun quoted parts -> { Sexp.Template.quoted; parts; loc = Loc.none })
     |> Record.field "quoted" (required bool) ~get:(fun { Sexp.Template.quoted; _ } ->
       quoted)
     |> Record.field
          "parts"
          (required (list template_part))
          ~get:(fun { Sexp.Template.parts; _ } -> parts)
     |> Record.finish)
;;

let sexp =
  let open Conv in
  fixpoint (fun sexp ->
    let atom = constr "atom" atom (fun atom -> Sexp.Atom atom) in
    let quoted = constr "quoted" string (fun string -> Sexp.Quoted_string string) in
    let template = constr "template" template (fun template -> Sexp.Template template) in
    let list = constr "list" (list sexp) (fun sexps -> Sexp.List sexps) in
    sum
      [ econstr atom; econstr quoted; econstr template; econstr list ]
      (function
        | Sexp.Atom value -> case value atom
        | Quoted_string value -> case value quoted
        | Template value -> case value template
        | List value -> case value list))
;;

let instance_id_env_var = "DUNE_RULES_PLUGIN_ID"

let load_rules =
  let conv = Conv.list sexp in
  let generation = Types.Decl.Request.make_current_gen ~req:conv ~resp:conv ~version:1 in
  Types.Decl.Request.make
    ~method_:(Types.Method.Name.of_string "dune/rules-plugin/load-rules")
    ~generations:[ generation ]
;;

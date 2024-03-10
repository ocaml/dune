open Import
open Dune_lang.Decoder

type allowed_vars =
  | Any
  | Only of (string * Dune_lang.Syntax.Version.t) list

(* The following variables are the ones allowed in the enabled_if fields of
   libraries, executables and install stanzas. While allowed variables for
   these stanzas are the same, the version at which they were allowed
   differs. *)
let common_vars_list =
  [ "architecture"
  ; "system"
  ; "model"
  ; "os_type"
  ; "ccomp_type"
  ; "profile"
  ; "ocaml_version"
  ; "context_name"
  ; "arch_sixtyfour"
  ]
;;

let common_vars ~since =
  Only
    (List.map
       ~f:(fun var ->
         match var with
         | "context_name" -> var, (2, 7)
         | "arch_sixtyfour" -> var, (3, 11)
         | _ -> var, since)
       common_vars_list)
;;

let emit_warning allowed_vars is_error var =
  let loc = Dune_lang.Template.Pform.loc var in
  let var_names = List.map ~f:fst allowed_vars in
  User_warning.emit
    ~loc
    ~is_error
    [ Pp.textf
        "Only %s variables are allowed in this 'enabled_if' field. Please upgrade your \
         dune language to at least 3.15."
        (String.enumerate_and var_names)
    ]
;;

let decode_value ~allowed_vars ?(is_error = true) () =
  match allowed_vars with
  | Any -> Blang.decode
  | Only allowed_vars ->
    Blang.Ast.decode
      ~override_decode_bare_literal:None
      (String_with_vars.decode_manually (fun env var ->
         match Dune_lang.Template.Pform.payload var with
         | Some _ ->
           emit_warning allowed_vars is_error var;
           Pform.Env.parse env var
         | None ->
           let name = Dune_lang.Template.Pform.name var in
           (match List.assoc allowed_vars name with
            | None ->
              emit_warning allowed_vars is_error var;
              Pform.Env.parse env var
            | Some min_ver ->
              let current_ver = Pform.Env.syntax_version env in
              if min_ver > current_ver
              then (
                let loc = Dune_lang.Template.Pform.loc var in
                let what = Dune_lang.Template.Pform.describe var in
                Dune_lang.Syntax.Error.since loc Stanza.syntax min_ver ~what)
              else Pform.Env.unsafe_parse_without_checking_version env var)))
;;

let decode ~allowed_vars ?is_error ~since () =
  let decode = decode_value ?is_error ~allowed_vars () in
  let decode =
    match since with
    | None -> decode
    | Some since -> Dune_lang.Syntax.since Stanza.syntax since >>> decode
  in
  field "enabled_if" ~default:Blang.true_ decode
;;

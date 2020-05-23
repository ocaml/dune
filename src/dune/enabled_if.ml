open Import
open Dune_lang.Decoder

type allowed_vars =
  | Any
  | Only of (string * Dune_lang.Syntax.Version.t) list

(* The following variables are the ones allowed in the enabled_if fields of
   libraries, executables and install stanzas. While allowed variables for
   theses stanzas are the same, the version at which they were allowed differs. *)
let common_vars_list =
  [ "architecture"
  ; "system"
  ; "model"
  ; "os_type"
  ; "ccomp_type"
  ; "profile"
  ; "ocaml_version"
  ]

let common_vars ~since =
  Only (List.map ~f:(fun var -> (var, since)) common_vars_list)

let emit_warning allowed_vars is_error var =
  let loc = String_with_vars.Var.loc var in
  let var_names = List.map ~f:fst allowed_vars in
  User_warning.emit ~loc ~is_error
    [ Pp.textf
        "Only %s variables are allowed in this 'enabled_if' field. If you \
         think that %s should also be allowed, please file an issue about it."
        (String.enumerate_and var_names)
        (String_with_vars.Var.name var)
    ];
  return ()

let decode ~allowed_vars ?(is_error = true) ~since () =
  let check_var ~allowed_vars var decoder_acc =
    ( match String_with_vars.Var.payload var with
    | Some _ -> emit_warning allowed_vars is_error var
    | None -> (
      let name = String_with_vars.Var.name var in
      match List.assoc allowed_vars name with
      | None -> emit_warning allowed_vars is_error var
      | Some min_ver ->
        let* current_ver = Dune_lang.Syntax.get_exn Stanza.syntax in
        if min_ver > current_ver then
          let loc = String_with_vars.Var.loc var in
          let what = "This variable" in
          Dune_lang.Syntax.Error.since loc Stanza.syntax min_ver ~what
        else
          return () ) )
    >>> decoder_acc
  in
  let check_vars blang =
    match allowed_vars with
    | Any -> return blang
    | Only allowed_vars ->
      Blang.fold_vars blang ~init:(return blang) ~f:(check_var ~allowed_vars)
  in
  let decode =
    ( match since with
    | None -> Blang.decode
    | Some since -> Dune_lang.Syntax.since Stanza.syntax since >>> Blang.decode
    )
    >>= check_vars
  in
  field "enabled_if" ~default:Blang.true_ decode

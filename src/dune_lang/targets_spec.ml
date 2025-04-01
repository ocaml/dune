open Stdune

module Multiplicity = struct
  type t =
    | One
    | Multiple

  let check_variable_matches_field ~loc ~field ~variable =
    let error field variable =
      User_error.raise
        ~loc
        [ Pp.textf
            "You can only use the variable %%{%s} if you defined the list of targets \
             using the field [%s] (not [%s])"
            variable
            field
            variable
        ]
    in
    match field, variable with
    | One, One | Multiple, Multiple -> ()
    | One, Multiple -> error "target" "targets"
    | Multiple, One -> error "targets" "target"
  ;;
end

module Kind = struct
  type t =
    | File
    | Directory
end

module Static = struct
  type 'path t =
    { targets : ('path * Kind.t * string option) list
    ; multiplicity : Multiplicity.t
    }
end

type 'a t =
  | Static of 'a Static.t
  | Infer
  | Bindings of 'a Bindings.t

let decode_target ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let base_target =
    let file =
      let+ file = String_with_vars.decode in
      file, Kind.File, None
    in
    let dir =
      let+ dir = sum ~force_parens:true [ "dir", String_with_vars.decode ] in
      if not allow_directory_targets
      then
        User_error.raise
          ~loc:(String_with_vars.loc dir)
          [ Pp.text "Directory targets require extension" ];
      dir, Kind.Directory, None
    in
    file <|> dir
  in
  let named_target =
    let+ name, (target, kind, _) =
      sum ~force_parens:true [ "named", pair string base_target ]
    in
    target, kind, Some name
  in
  named_target <|> base_target
;;

let decode_static ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let+ syntax_version = Dune_sexp.Syntax.get_exn Stanza.syntax
  and+ targets = repeat (decode_target ~allow_directory_targets) in
  if syntax_version < (1, 3)
  then
    List.iter targets ~f:(fun (target, (_ : Kind.t), _) ->
      if String_with_vars.has_pforms target
      then
        Dune_sexp.Syntax.Error.since
          (String_with_vars.loc target)
          Stanza.syntax
          (1, 3)
          ~what:"Using variables in the targets field");
  Static { targets; multiplicity = Multiple }
;;

let decode_one_static ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let+ () = Dune_sexp.Syntax.since Stanza.syntax (1, 11)
  and+ target = decode_target ~allow_directory_targets in
  Static { targets = [ target ]; multiplicity = One }
;;

let decode_bindings =
  let open Dune_sexp.Decoder in
  Bindings.decode String_with_vars.decode >>| fun bindings -> Bindings bindings
;;

let field ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  fields_mutually_exclusive
    ~default:Infer
    [ "target", decode_one_static ~allow_directory_targets
    ; "targets", decode_static ~allow_directory_targets
    ; "bindings", decode_bindings (* New binding-style targets *)
    ]
;;

let expand_bindings bindings =
  List.fold_left bindings ~init:[] ~f:(fun acc -> function
    | Bindings.Unnamed target -> (target, Kind.File, None) :: acc
    | Bindings.Named (name, targets) ->
      List.fold_left targets ~init:acc ~f:(fun acc target ->
        (target, Kind.File, Some name) :: acc))
;;

[@@@warning "-32"]

let is_empty = function
  | Infer -> true
  | Static { targets = []; _ } -> true
  | Bindings b -> Bindings.is_empty b
  | _ -> false
;;

[@@@warning "+32"]

let get_target_by_name name = function
  | Static { targets; _ } ->
    List.find_map targets ~f:(fun (target, _, _) ->
      match String_with_vars.text_only target with
      | Some s when s = name -> Some target
      | _ -> None)
  | Infer -> None
  | Bindings b -> Bindings.get_target_by_name name b
;;

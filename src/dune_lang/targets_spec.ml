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
            variable
            field
        ]
    in
    match field, variable with
    | One, One | Multiple, Multiple -> ()
    | One, Multiple -> error "target" "targets"
    | Multiple, One -> error "targets" "target"
  ;;
end

(* CR-someday amokhov: Add more interesting kinds, for example, to allow the
   user to specify file patterns like "*.ml" for directory targets. *)
module Kind = struct
  type t =
    | File
    | Directory
end

module Static = struct
  (* Change to take the full pair as path *)
  type ('path, 'kind) named = {
    name : string option;
    path : 'path * 'kind;  (* Now takes the tuple *)
  }

  type 'path t = {
    targets : ('path, Kind.t) named list;  (* This now matches *)
    multiplicity : Multiplicity.t;
  }
end

type 'a t =
  | Static of 'a Static.t
  | Infer

(* Move decode_target before decode_named_target *)
let decode_target ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let file =
    let+ file = String_with_vars.decode in
    file, Kind.File
  in
  let dir =
    let+ dir = sum ~force_parens:true [ "dir", String_with_vars.decode ] in
    if not allow_directory_targets
    then
      User_error.raise
        ~loc:(String_with_vars.loc dir)
        [ Pp.text "Directory targets require the 'directory-targets' extension" ];
    dir, Kind.Directory
  in
  file <|> dir
;;

let decode_named_target ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let named =
    enter (
      let* loc = loc in
      let* sexp_opt = peek in
      match sexp_opt with
      | Some (sexp : Dune_sexp.Ast.t) ->
        (match sexp with
        | Atom (loc, atom) when String.is_prefix (Dune_sexp.Atom.to_string atom) ~prefix:":" ->
          let* () = junk in
          let+ target = decode_target ~allow_directory_targets in
          let atom_str = Dune_sexp.Atom.to_string atom in
          let name = 
            match String.drop_prefix atom_str ~prefix:":" with
            | Some name -> name
            | None -> 
                User_error.raise ~loc [ Pp.text "Expected ':' prefix but couldn't extract name" ]
          in
          { Static.name = Some name;
            path = target;  (* target is already the (path, kind) tuple *)
          }
        | _ ->
          let+ target = decode_target ~allow_directory_targets in
          { Static.name = None;
            path = target;  (* target is already the (path, kind) tuple *)
          })
      | None ->
        let+ () = return () in
        User_error.raise ~loc [ Pp.text "Expected an S-expression but got nothing" ]
    )
  in
  named

  let decode_static ~allow_directory_targets =
    let open Dune_sexp.Decoder in
    let+ syntax_version = Dune_sexp.Syntax.get_exn Stanza.syntax
    and+ targets = repeat (decode_named_target ~allow_directory_targets) in
    (* Check for variables in targets if using older syntax *)
    if syntax_version < (1, 3) then
      List.iter targets ~f:(fun target ->
        let path = fst target.Static.path in  (* Extract just the path from the tuple *)
        if String_with_vars.has_pforms path then
          Dune_sexp.Syntax.Error.since
            (String_with_vars.loc path)
            Stanza.syntax
            (1, 3)
            ~what:"Using variables in the targets field");
    
    (* Check for duplicate names *)
    let names = List.filter_map targets ~f:(fun t -> t.Static.name) in
    if List.length names <> String.Set.cardinal (String.Set.of_list names) then
      User_error.raise ~loc:(Loc.of_pos __POS__) [ Pp.text "Duplicate target names found" ];
    
    Static { targets; multiplicity = Multiple }
;;

let decode_one_static ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let+ () = Dune_sexp.Syntax.since Stanza.syntax (1, 11)
  and+ target = decode_named_target ~allow_directory_targets in
  Static { 
    targets = [ target ];  (* Directly use the target from decode_named_target *)
    multiplicity = One 
  }
;;

let field ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  fields_mutually_exclusive
    ~default:Infer
    [ "target", decode_one_static ~allow_directory_targets
    ; "targets", decode_static ~allow_directory_targets
    ]
;;
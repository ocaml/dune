open Stdune


module Hashtbl = Stdlib.Hashtbl

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
  type 'path t = {
  targets : ('path * Kind.t * string option) list;
  multiplicity : Multiplicity.t;
}

end

type 'a t =
  | Static of 'a Static.t
  | Infer

  let named_targets : (string, String_with_vars.t) Hashtbl.t = Hashtbl.create 16
let decode_target ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let base_target =
    let file = 
      let+ file = String_with_vars.decode in
      (file, Kind.File, None)
    in
    let dir =
      let+ dir = sum ~force_parens:true [ "dir", String_with_vars.decode ] in
      if not allow_directory_targets then
        User_error.raise ~loc:(String_with_vars.loc dir)
          [ Pp.text "Directory targets require extension" ];
      (dir, Kind.Directory, None)
    in
    file <|> dir
  in
    
  let named_target =
    let+ name, (target, kind, _) = 
      sum ~force_parens:true [ "named", pair string base_target ] in
    Hashtbl.add named_targets name target;
    (target, kind, Some name)
  in
  
  named_target <|> base_target
;;

(* src/dune_lang/targets_spec.ml *)
let decode_static ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let+ syntax_version = Dune_sexp.Syntax.get_exn Stanza.syntax
  and+ targets = repeat (decode_target ~allow_directory_targets) in
  if syntax_version < (1, 3) then
    List.iter targets ~f:(fun (target, (_ : Kind.t), _) ->
      if String_with_vars.has_pforms target then
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


let field ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  fields_mutually_exclusive
    ~default:Infer
    [ "target", decode_one_static ~allow_directory_targets
    ; "targets", decode_static ~allow_directory_targets
    ]
;;

let get_target_by_name name =
  Hashtbl.find_opt named_targets name
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
  type 'path t =
    { targets : ('path * Kind.t) list
    ; multiplicity : Multiplicity.t
    }
end

type 'a t =
  | Static of 'a Static.t
  | Infer

let decode_target =
  let open Dune_sexp.Decoder in
  let file =
    let+ file = String_with_vars.decode in
    file, Kind.File
  in
  let dir =
    let+ dir =
      sum
        ~force_parens:true
        [ "dir", Dune_sexp.Syntax.since Stanza.syntax (3, 13) >>> String_with_vars.decode
        ]
    in
    dir, Kind.Directory
  in
  file <|> dir
;;

let decode_static =
  let open Dune_sexp.Decoder in
  let+ syntax_version = Dune_sexp.Syntax.get_exn Stanza.syntax
  and+ targets = repeat decode_target in
  if syntax_version < (1, 3)
  then
    List.iter targets ~f:(fun (target, (_ : Kind.t)) ->
      if String_with_vars.has_pforms target
      then
        Dune_sexp.Syntax.Error.since
          (String_with_vars.loc target)
          Stanza.syntax
          (1, 3)
          ~what:"Using variables in the targets field");
  Static { targets; multiplicity = Multiple }
;;

let decode_one_static =
  let open Dune_sexp.Decoder in
  let+ () = Dune_sexp.Syntax.since Stanza.syntax (1, 11)
  and+ target = decode_target in
  Static { targets = [ target ]; multiplicity = One }
;;

let field =
  let open Dune_sexp.Decoder in
  fields_mutually_exclusive
    ~default:Infer
    [ "target", decode_one_static; "targets", decode_static ]
;;

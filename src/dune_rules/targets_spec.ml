open! Dune_engine
open Import

module Multiplicity = struct
  type t =
    | One
    | Multiple

  let check_variable_matches_field ~loc ~field ~variable =
    let error field variable =
      User_error.raise ~loc
        [ Pp.textf
            "You can only use the variable %%{%s} if you defined the list of \
             targets using the field [%s] (not [%s])"
            variable variable field
        ]
    in
    match (field, variable) with
    | One, One
    | Multiple, Multiple ->
      ()
    | One, Multiple -> error "target" "targets"
    | Multiple, One -> error "targets" "target"
end

(* CR-someday amokhov: Add more interesting tags, for example, to allow the user
   to specify file patterns like "*.ml" for directory targets. *)
module Tag = struct
  type t =
    | None
    | Star
end

module Static = struct
  type 'path t =
    { targets : 'path list
    ; multiplicity : Multiplicity.t
    }
end

type 'a t =
  | Static of 'a Static.t
  | Infer

let decode_static =
  let open Dune_lang.Decoder in
  let+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax
  and+ targets = repeat String_with_vars.decode in
  if syntax_version < (1, 3) then
    List.iter targets ~f:(fun target ->
        if String_with_vars.has_pforms target then
          Dune_lang.Syntax.Error.since
            (String_with_vars.loc target)
            Stanza.syntax (1, 3) ~what:"Using variables in the targets field");
  Static { targets; multiplicity = Multiple }

let decode_one_static =
  let open Dune_lang.Decoder in
  let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 11)
  and+ target = String_with_vars.decode in
  Static { targets = [ target ]; multiplicity = One }

let field =
  let open Dune_lang.Decoder in
  fields_mutually_exclusive ~default:Infer
    [ ("targets", decode_static); ("target", decode_one_static) ]

let has_target_directory = function
  | Infer -> false
  | Static { targets; _ } ->
    List.exists targets ~f:(fun target ->
        match String_with_vars.last_text_part target with
        | None -> false
        | Some part -> Option.is_some (String.drop_suffix ~suffix:"/*" part))

let untag = function
  | Infer -> Infer
  | Static { targets; multiplicity } ->
    let targets = List.map targets ~f:fst in
    Static { targets; multiplicity }

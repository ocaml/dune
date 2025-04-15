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

module Named_target = struct
  type 'path t =
    | Anonymous of 'path * Kind.t
    | Named of string * ('path * Kind.t)
end

module Static = struct
  type 'path t =
    { targets : ('path * Kind.t) list
    ; multiplicity : Multiplicity.t
    ; named_targets : (string * 'path) list
    }
end

type 'a t =
  | Static of 'a Static.t
  | Infer

let decode_target ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let base_decode =
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
  in
  let named =
    enter (pair string base_decode)
    >>| fun (name, (path, kind)) -> Named_target.Named (name, (path, kind))
  in
  let anonymous =
    base_decode >>| fun (path, kind) -> Named_target.Anonymous (path, kind)
  in
  anonymous <|> named
;;

(* Remove any concrete type assumptions *)
let extract_named_targets targets_named =
  let named_targets =
    List.filter_map targets_named ~f:(function
      | Named_target.Anonymous _ -> None
      | Named_target.Named (name, (path, _)) -> Some (name, path))
  in
  List.fold_left named_targets ~init:String.Map.empty ~f:(fun acc (name, path) ->
    match String.Map.add acc name path with
    | Ok map -> map
    | Error _ -> User_error.raise [ Pp.textf "Duplicate named target: %s" name ])
;;

let decode_static ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let+ syntax_version = Dune_sexp.Syntax.get_exn Stanza.syntax
  and+ targets_named = repeat (decode_target ~allow_directory_targets) in
  (* Check syntax version constraints *)
  if syntax_version < (1, 3)
  then
    List.iter targets_named ~f:(function
      | Named_target.Anonymous (target, _) when String_with_vars.has_pforms target ->
        Dune_sexp.Syntax.Error.since
          (String_with_vars.loc target)
          Stanza.syntax
          (1, 3)
          ~what:"Using variables in the targets field"
      | Named_target.Named (_, (target, _)) when String_with_vars.has_pforms target ->
        Dune_sexp.Syntax.Error.since
          (String_with_vars.loc target)
          Stanza.syntax
          (1, 3)
          ~what:"Using variables in named targets"
      | _ -> ());
  (* Convert from Named_target.t list to (path * Kind.t) list *)
  let targets =
    List.map targets_named ~f:(function
      | Named_target.Anonymous (path, kind) -> path, kind
      | Named_target.Named (_, (path, kind)) -> path, kind)
  in
  (* Extract named targets for variable expansion *)
  let named_targets =
    List.filter_map targets_named ~f:(function
      | Named_target.Named (name, (path, _)) -> Some (name, path)
      | Named_target.Anonymous _ -> None)
  in
  Static { targets; multiplicity = Multiple; named_targets }
;;

let decode_one_static ~allow_directory_targets =
  let open Dune_sexp.Decoder in
  let+ () = Dune_sexp.Syntax.since Stanza.syntax (1, 11)
  and+ target_named = decode_target ~allow_directory_targets in
  let target =
    match target_named with
    | Named_target.Anonymous (path, kind) -> path, kind
    | Named_target.Named (_, (path, kind)) -> path, kind
  in
  let named_targets =
    match target_named with
    | Named_target.Anonymous _ -> []
    | Named_target.Named (name, (path, _)) -> [ name, path ]
  in
  Static { targets = [ target ]; multiplicity = One; named_targets }
;;

let decode ~allow_directory_targets string_decoder =
  let open Dune_sexp.Decoder in
  let+ path = string_decoder in
  let kind = Kind.File in 
  Static { targets = [(path, kind)]; multiplicity = One; named_targets = [] }
;;

let field_one ~allow_directory_targets name decode =
  let open Dune_sexp.Decoder in
  field name (
    decode >>= fun name_value ->
    decode_one_static ~allow_directory_targets >>= fun static_spec ->
    match static_spec with
    | Static { targets = [(path, kind)]; multiplicity; named_targets } ->
      let new_target = (path, kind) in
      let new_named_targets = [(name, path)] in
      return (Static {
        targets = [new_target];
        multiplicity;
        named_targets = new_named_targets @ named_targets
      })
  )

  let field_many ~allow_directory_targets name decode =
    let open Dune_sexp.Decoder in
    field name (
      decode >>= fun name_value ->
      decode_static ~allow_directory_targets >>= fun static_spec ->
      let named_targets = [(name_str, path)] in
      return { static_spec with named_targets = named_targets @ static_spec.named_targets }
    )

let field ~allow_directory_targets name decode =
  let open Dune_sexp.Decoder in
  match name with
  | "target" -> field_one ~allow_directory_targets name decode
  | "targets" -> field_many ~allow_directory_targets name decode
  | _ ->
    field
      name
      (decode
        >>= fun name_value ->
        let name_str = String_with_vars.to_string name_value in
        return (Static { 
          targets = [(name_value, Kind.File)]; 
          multiplicity = One; 
          named_targets = [(name_str, name_value)] 
        }))
;;

open Import

module Build = struct
  type t = Path.Build.t

  let anonymous_actions_dir_basename = ".actions"
  let anonymous_actions_dir = Path.Build.(relative root) anonymous_actions_dir_basename
end

type target_kind =
  | Regular of Context_name.t * Path.Source.t
  | Alias of Context_name.t * Path.Source.t
  | Anonymous_action of Context_name.t
  | Other of Path.Build.t

module Target_dir = struct
  type context_related =
    | Root
    | With_context of Context_name.t * Path.Source.t

  let build_dir = function
    | Root -> Path.Build.root
    | With_context (ctx, s) -> Path.Build.append_source (Context_name.build_dir ctx) s
  ;;

  type t =
    | Anonymous_action of context_related
    | Regular of context_related
    | Invalid of Path.Build.t

  (* _build/foo or _build/install/foo where foo is invalid *)

  let of_target fn =
    match Path.Build.extract_first_component fn with
    | None -> Regular Root
    | Some (name, sub) ->
      if name = Build.anonymous_actions_dir_basename
      then (
        match Path.Local.split_first_component sub with
        | None -> Anonymous_action Root
        | Some (ctx, fn) ->
          let ctx = Context_name.of_string ctx in
          Anonymous_action (With_context (ctx, Path.Source.of_local fn)))
      else (
        match Context_name.of_string_opt name with
        | None -> Invalid fn
        | Some ctx -> Regular (With_context (ctx, Path.Source.of_local sub)))
  ;;
end

type 'build path_kind =
  | Source of Path.Source.t
  | External of Path.External.t
  | Build of 'build

let is_digest s = String.length s = 32

let analyse_target (fn as original_fn) : target_kind =
  match Target_dir.of_target fn with
  | Invalid _ | Regular Root | Anonymous_action Root -> Other fn
  | Regular (With_context (ctx, src_dir)) -> Regular (ctx, src_dir)
  | Anonymous_action (With_context (ctx, fn)) ->
    if Path.Source.is_root fn
    then Other original_fn
    else (
      let basename = Path.Source.basename fn in
      match String.rsplit2 basename ~on:'-' with
      | None -> if is_digest basename then Anonymous_action ctx else Other original_fn
      | Some (basename, suffix) ->
        if is_digest suffix
        then Alias (ctx, Path.Source.relative (Path.Source.parent_exn fn) basename)
        else Other original_fn)
;;

let describe_target fn =
  let ctx_suffix name =
    if Context_name.is_default name
    then ""
    else sprintf " (context %s)" (Context_name.to_string name)
  in
  match analyse_target fn with
  | Alias (ctx, p) ->
    sprintf "alias %s%s" (Path.Source.to_string_maybe_quoted p) (ctx_suffix ctx)
  | Anonymous_action ctx -> sprintf "<internal-action>%s" (ctx_suffix ctx)
  | Regular (ctx, fn) ->
    sprintf "%s%s" (Path.Source.to_string_maybe_quoted fn) (ctx_suffix ctx)
  | Other fn -> Path.Build.to_string_maybe_quoted fn
;;

let describe_path (p : Path.t) =
  match p with
  | External _ | In_source_tree _ -> Path.to_string_maybe_quoted p
  | In_build_dir p -> describe_target p
;;

let analyse_path (fn : Path.t) =
  match fn with
  | In_source_tree src -> Source src
  | External e -> External e
  | In_build_dir build -> Build (analyse_target build)
;;

let analyse_dir (fn : Path.t) =
  match fn with
  | In_source_tree src -> Source src
  | External e -> External e
  | In_build_dir build -> Build (Target_dir.of_target build)
;;

type t = Path.t

let encode p =
  (* CR rgrinberg: only reason this lives here is to implement
     [$ dune rules]. Seems like it should just live there along with all the
     other encoders in the engine. *)
  let make constr arg =
    Dune_sexp.List [ Dune_sexp.atom constr; Dune_sexp.atom_or_quoted_string arg ]
  in
  let open Path in
  match p with
  | In_build_dir p -> make "In_build_dir" (Path.Build.to_string p)
  | In_source_tree p -> make "In_source_tree" (Path.Source.to_string p)
  | External p -> make "External" (Path.External.to_string p)
;;

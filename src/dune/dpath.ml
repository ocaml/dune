open Stdune

type target_kind =
  | Regular of Context_name.t * Path.Source.t
  | Alias of Context_name.t * Path.Source.t
  | Install of Context_name.t * Path.Source.t
  | Other of Path.Build.t

type path_kind =
  | Source of Path.Source.t
  | External of Path.External.t
  | Build of target_kind

let analyse_target (fn as original_fn) : target_kind =
  match Path.Build.extract_first_component fn with
  | Some (".aliases", sub) -> (
    match Path.Local.split_first_component sub with
    | None -> Other fn
    | Some (ctx, fn) ->
      if Path.Local.is_root fn then
        Other original_fn
      else
        let basename =
          match String.rsplit2 (Path.Local.basename fn) ~on:'-' with
          | None -> assert false
          | Some (name, digest) ->
            assert (String.length digest = 32);
            name
        in
        let ctx = Context_name.of_string ctx in
        Alias
          ( ctx
          , Path.Source.relative
              (Path.Source.of_local (Path.Local.parent_exn fn))
              basename ) )
  | Some ("install", sub) -> (
    match Path.Local.split_first_component sub with
    | None -> Other original_fn
    | Some (ctx, fn) -> (
      match Context_name.of_string_opt ctx with
      | None -> Other original_fn
      | Some ctx -> Install (ctx, Path.Source.of_local fn) ) )
  | Some (ctx, sub) -> (
    match Context_name.of_string_opt ctx with
    | None -> Other fn
    | Some ctx -> Regular (ctx, Path.Source.of_local sub) )
  | None -> Other fn

let describe_target fn =
  let ctx_suffix name =
    if Context_name.is_default name then
      ""
    else
      sprintf " (context %s)" (Context_name.to_string name)
  in
  match analyse_target fn with
  | Alias (ctx, p) ->
    sprintf "alias %s%s"
      (Path.Source.to_string_maybe_quoted p)
      (ctx_suffix ctx)
  | Install (ctx, p) ->
    sprintf "install %s%s"
      (Path.Source.to_string_maybe_quoted p)
      (ctx_suffix ctx)
  | Regular (ctx, fn) ->
    sprintf "%s%s" (Path.Source.to_string_maybe_quoted fn) (ctx_suffix ctx)
  | Other fn -> Path.Build.to_string_maybe_quoted fn

let describe_path (p : Path.t) =
  match p with
  | External _
  | In_source_tree _ ->
    Path.to_string_maybe_quoted p
  | In_build_dir p -> describe_target p

let analyse_path (fn : Path.t) =
  match fn with
  | In_source_tree src -> Source src
  | External e -> External e
  | In_build_dir build -> Build (analyse_target build)

type t = Path.t

let encode p =
  let make constr arg =
    Dune_lang.List
      [ Dune_lang.atom constr; Dune_lang.atom_or_quoted_string arg ]
  in
  let open Path in
  match p with
  | In_build_dir p -> make "In_build_dir" (Path.Build.to_string p)
  | In_source_tree p -> make "In_source_tree" (Path.Source.to_string p)
  | External p -> make "External" (Path.External.to_string p)

let decode =
  let open Dune_lang.Decoder in
  let external_ =
    plain_string (fun ~loc t ->
        if Filename.is_relative t then
          User_error.raise ~loc [ Pp.text "Absolute path expected" ]
        else
          Path.parse_string_exn ~loc t)
  in
  sum
    [ ("In_build_dir", string >>| Path.(relative build_dir))
    ; ("In_source_tree", string >>| Path.(relative root))
    ; ("External", external_)
    ]

module Local = struct
  let encode ~dir:from p =
    let open Dune_lang.Encoder in
    string (Path.reach ~from p)

  let decode ~dir =
    let open Dune_lang.Decoder in
    let+ error_loc, path = located string in
    Path.relative ~error_loc dir path
end

module Build = struct
  type t = Path.Build.t

  let encode p =
    let str = Path.reach ~from:Path.build_dir (Path.build p) in
    Dune_lang.atom_or_quoted_string str

  let decode =
    let open Dune_lang.Decoder in
    let+ base = string in
    Path.Build.(relative root) base

  let is_dev_null = Fn.const false
end

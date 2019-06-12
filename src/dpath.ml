open Stdune

type target_kind =
  | Regular of string * Path.Source.t
  | Alias   of string * Path.Source.t
  | Install of string * Path.Source.t
  | Other of Path.Build.t

type path_kind =
  | Source of Path.Source.t
  | External of Path.External.t
  | Build of target_kind

let analyse_target (fn as original_fn) : target_kind =
  match Path.Build.extract_first_component fn with
  | Some (".aliases", sub) ->
    (match Path.Local.split_first_component sub with
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
         Alias (ctx,
                Path.Source.relative
                  (Path.Source.of_local (Path.Local.parent_exn fn))
                  basename))
  | Some ("install", sub) ->
    (match Path.Local.split_first_component sub with
     | None -> Other fn
     | Some (ctx, fn) ->
       Install (ctx, Path.Source.of_local fn))
  | Some (ctx, sub) ->
    Regular (ctx, Path.Source.of_local sub)
  | None ->
    Other fn

let describe_target fn =
  let ctx_suffix = function
    | "default" -> ""
    | ctx -> sprintf " (context %s)" ctx
  in
  match analyse_target fn with
  | Alias (ctx, p) ->
    sprintf "alias %s%s" (Path.Source.to_string_maybe_quoted p) (ctx_suffix ctx)
  | Install (ctx, p) ->
    sprintf "install %s%s" (Path.Source.to_string_maybe_quoted p) (ctx_suffix ctx)
  | Regular (ctx, fn) ->
    sprintf "%s%s" (Path.Source.to_string_maybe_quoted fn) (ctx_suffix ctx)
  | Other fn ->
    Path.Build.to_string_maybe_quoted fn

let describe_path (p : Path.t) =
  match p with
  | External _ | In_source_tree _ ->
    Path.to_string_maybe_quoted p
  | In_build_dir p -> describe_target p

let analyse_path (fn : Path.t) = match fn with
  | In_source_tree src -> Source src
  | External e -> External e
  | In_build_dir build -> Build (analyse_target build)

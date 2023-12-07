open Import
open Memo.O

module Node = struct
  type t =
    { value : Dune_env.t
    ; parent : t option Memo.t
    }

  let by_context dir =
    let open Memo.O in
    let+ context = Context.DB.by_dir dir in
    let { Context.Env_nodes.context; workspace } = Context.env_nodes context in
    match context, workspace with
    | None, None -> None
    | Some value, None | None, Some value -> Some { value; parent = Memo.return None }
    | Some context, Some workspace ->
      Some
        { value = context
        ; parent = Memo.return (Some { value = workspace; parent = Memo.return None })
        }
  ;;

  let in_dir ~dir =
    Only_packages.stanzas_in_dir dir
    >>| function
    | None -> None
    | Some stanzas ->
      List.find_map stanzas.stanzas ~f:(fun stanza ->
        match Stanza.repr stanza with
        | Dune_env.T config -> Some config
        | _ -> None)
  ;;

  let rec by_dir dir =
    let parent =
      let* scope = Scope.DB.find_by_dir dir in
      if Path.Build.equal dir (Scope.root scope)
      then by_context dir
      else (
        match Path.Build.parent dir with
        | None -> by_context dir
        | Some parent -> by_dir parent)
    in
    in_dir ~dir
    >>= function
    | Some value -> Memo.return (Some { value; parent })
    | None -> parent
  ;;
end

let value ~default ~f =
  let rec loop = function
    | None -> Memo.return default
    | Some { Node.value; parent } ->
      let* next =
        f value
        >>| function
        | Some x -> `Ok x
        | None -> `Parent
      in
      (match next with
       | `Ok x -> Memo.return x
       | `Parent -> parent >>= loop)
  in
  fun ~dir -> Node.by_dir dir >>= loop
;;

let profile ~dir =
  let name, _ = Path.Build.extract_build_context_exn dir in
  let context = Context_name.of_string name in
  Per_context.profile context
;;

let value ~default ~dir ~f =
  let profile = lazy (profile ~dir) in
  value ~default ~dir ~f:(fun stanza ->
    let* profile = Lazy.force profile in
    match Dune_env.find_opt stanza ~profile with
    | None -> Memo.return None
    | Some stanza -> f stanza)
;;

let bin_annot ~dir =
  value ~default:true ~dir ~f:(fun (t : Dune_env.config) -> Memo.return t.bin_annot)
;;

let inline_tests ~dir =
  value ~default:None ~dir ~f:(fun (t : Dune_env.config) ->
    Memo.return
    @@
    match t.inline_tests with
    | None -> None
    | Some s -> Some (Some s))
  >>= function
  | Some s -> Memo.return s
  | None ->
    let+ profile = profile ~dir in
    if Profile.is_inline_test profile then Dune_env.Inline_tests.Enabled else Disabled
;;

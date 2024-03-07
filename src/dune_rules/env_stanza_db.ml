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
    Dune_load.stanzas_in_dir dir
    >>= function
    | None -> Memo.return None
    | Some stanzas ->
      Dune_file.find_stanzas stanzas Dune_env.key
      >>| (function
       | [ config ] -> Some config
       | _ -> None)
  ;;

  let rec by_dir dir =
    let parent =
      let* project = Dune_load.find_project ~dir in
      if Path.Source.equal
           (Path.Build.drop_build_context_exn dir)
           (Dune_project.root project)
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

let value_opt ~dir ~f =
  value ~default:None ~dir ~f:(fun t ->
    f t
    >>| function
    | None -> None
    | Some s -> Some (Some s))
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

module Inherit = struct
  let for_context
    (type a)
    ~name
    ~(root : Context_name.t -> Dune_project.t -> a Memo.t)
    (context : Context_name.t)
    ~(f : parent:a Memo.t -> dir:Path.Build.t -> Dune_env.config -> a Memo.t)
    =
    let for_context =
      Memo.Lazy.create (fun () ->
        let+ context = Context.DB.get context in
        let profile = Context.profile context in
        let { Context.Env_nodes.context; workspace } = Context.env_nodes context in
        let make env = Option.bind env ~f:(Dune_env.find_opt ~profile) in
        [ make workspace; make context ] |> List.filter_opt)
    in
    let root =
      Memo.create
        (sprintf "%s-root" name)
        ~input:(module Path.Source)
        (fun dir ->
          let* projects_by_root = Dune_load.projects_by_root ()
          and* envs = Memo.Lazy.force for_context in
          let project = Path.Source.Map.find_exn projects_by_root dir in
          let root = root context project in
          let dir = Path.Build.append_source (Context_name.build_dir context) dir in
          List.fold_left envs ~init:root ~f:(fun acc env -> f ~parent:acc ~dir env))
      |> Memo.exec
    in
    let module Non_rec = struct
      module rec Rec : sig
        val memo : Path.Build.t -> a Memo.t
      end = struct
        let f path =
          let* env =
            Node.in_dir ~dir:path
            >>= function
            | None -> Memo.return None
            | Some stanza ->
              let+ profile = Context.DB.get context >>| Context.profile in
              Dune_env.find_opt stanza ~profile
          in
          let parent =
            let* parent_path =
              match Path.Build.parent path with
              | None -> Code_error.raise "invalid path" []
              | Some parent ->
                let+ project = Dune_load.find_project ~dir:path in
                let without_context = Path.Build.drop_build_context_exn path in
                if Path.Source.equal (Dune_project.root project) without_context
                then `Root without_context
                else `Parent parent
            in
            match parent_path with
            | `Root without_context -> root without_context
            | `Parent p -> Rec.memo p
          in
          match env with
          | None -> parent
          | Some stanza -> f ~parent ~dir:path stanza
        ;;

        let memo = Memo.exec (Memo.create name ~input:(module Path.Build) f)
      end
    end
    in
    Staged.stage Non_rec.Rec.memo
  ;;

  let inherited ~name ~root ~f =
    let by_context =
      Per_context.create_by_name ~name:(sprintf "inherited-%s" name) (fun ctx ->
        Memo.return (for_context ~name ~root ctx ~f))
      |> Staged.unstage
    in
    Staged.stage (fun path ->
      match Install.Context.of_path path with
      | None ->
        Code_error.raise
          "path is not allowed inherited nodes"
          [ "path", Path.Build.to_dyn path ]
      | Some ctx ->
        let* for_ctx = by_context ctx in
        Staged.unstage for_ctx path)
  ;;
end

include Inherit

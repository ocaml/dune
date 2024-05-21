open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

include struct
  open Dune_pkg
  module Checksum = Checksum
  module Rev_store = Rev_store
  module Pkg = Lock_dir.Pkg
  module OpamUrl = OpamUrl
  module Source = Source
end

let context_name = Context_name.of_string "_fetch"
let context = Build_context.create ~name:context_name
let digest_of_url url = OpamUrl.to_string url |> Digest.string

let make_target ~kind url_or_checksum =
  let basename =
    match kind with
    | `File -> "file"
    | `Directory -> "dir"
  in
  let dir1, dir2 =
    match url_or_checksum with
    | `Url url -> "url", digest_of_url url |> Digest.to_string
    | `Checksum checksum -> "checksum", Checksum.to_string checksum
  in
  Path.Build.L.relative context.build_dir [ dir1; dir2; basename ]
;;

type kind =
  [ `File
  | `Directory
  ]

let resolve_url =
  (* Before we fetch any git repo, we make sure to convert the URL to fetch the
     git object directly. The object is used to compute the fetch action digest
     which is helpful to avoid refetching the same objects. *)
  let git url =
    Memo.of_reproducible_fiber
    @@
    let open Fiber.O in
    let+ git_object =
      let* rev_store = Rev_store.get in
      OpamUrl.resolve url ~loc:Loc.none rev_store
      >>| function
      | Ok (Resolved r) -> (r :> Rev_store.Object.t)
      | Ok (Unresolved r) -> r
      | Error m -> raise (User_error.E m)
    in
    (* We fetch the object directly to make sure that there are no races
       between us resolving a branch or tag and then upstream modifying its
       meaning. *)
    Dune_pkg.OpamUrl.set_rev url git_object
  in
  let memo =
    Memo.create "resolve-git-url" ~input:(module OpamUrl) ~cutoff:OpamUrl.equal git
  in
  fun (url : OpamUrl.t) ->
    match url.backend with
    | `git -> Memo.exec memo url
    | _ -> Memo.return url
;;

module Spec = struct
  type ('path, 'target) t =
    { target : 'target
    ; url : Loc.t * OpamUrl.t
    ; checksum : (Loc.t * Checksum.t) option
    ; kind : kind
    }

  let name = "source-fetch"
  let version = 1
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode { target; url = _, url; checksum; kind } _ encode_target : Dune_lang.t =
    List
      ([ Dune_lang.atom_or_quoted_string name
       ; encode_target target
       ; Dune_lang.atom_or_quoted_string (OpamUrl.to_string url)
       ; Dune_lang.atom_or_quoted_string
           (match kind with
            | `File -> "file"
            | `Directory -> "directory")
       ]
       @ (match OpamUrl.rev url with
          | None -> []
          | Some rev -> [ Dune_lang.atom_or_quoted_string rev ])
       @
       match checksum with
       | None -> []
       | Some (_, checksum) ->
         [ Checksum.to_string checksum |> Dune_lang.atom_or_quoted_string ])
  ;;

  let action { target; url = loc_url, url; checksum; kind } ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let* () = Fiber.return () in
    (let checksum = Option.map checksum ~f:snd in
     Dune_pkg.Fetch.fetch
       ~unpack:
         (match kind with
          | `File -> false
          | `Directory -> true)
       ~checksum
       ~target:(Path.build target)
       ~url:(loc_url, url))
    >>= function
    | Ok () -> Fiber.return ()
    | Error (Checksum_mismatch actual_checksum) ->
      (match checksum with
       | None ->
         User_error.raise
           ~loc:loc_url
           [ Pp.text "No checksum provided. It should be:"; Checksum.pp actual_checksum ]
       | Some (loc, _) ->
         User_error.raise
           ~loc
           [ Pp.text "Invalid checksum, got"; Dune_pkg.Checksum.pp actual_checksum ])
    | Error (Unavailable message) ->
      let loc = loc_url in
      (match message with
       | None -> User_error.raise ~loc [ Pp.text "Unknown fetch failure" ]
       | Some msg -> User_error.raise ~loc [ User_message.pp msg ])
  ;;
end

let action ~url ~checksum ~target ~kind =
  let module M = struct
    type path = Path.t
    type target = Path.Build.t

    module Spec = Spec

    let v = { Spec.target; checksum; url; kind }
  end
  in
  Action.Extension (module M)
;;

let extract_checksums_and_urls (lockdir : Dune_pkg.Lock_dir.t) =
  Package.Name.Map.fold
    lockdir.packages
    ~init:(Checksum.Map.empty, Dune_digest.Map.empty)
    ~f:(fun package acc ->
      let sources =
        let sources = package.info.extra_sources |> List.rev_map ~f:snd in
        match package.info.source with
        | None -> sources
        | Some source -> source :: sources
      in
      List.fold_left sources ~init:acc ~f:(fun (checksums, urls) (source : Source.t) ->
        match Source.kind source with
        | `Directory_or_archive _ -> checksums, urls
        | `Fetch ->
          let url = source.url in
          (match source.checksum with
           | Some ((_, checksum) as checksum_with_loc) ->
             Checksum.Map.set checksums checksum (url, checksum_with_loc), urls
           | None -> checksums, Digest.Map.set urls (digest_of_url (snd url)) url)))
;;

let find_checksum, find_url =
  let all =
    Memo.lazy_ (fun () ->
      Per_context.list ()
      >>= Memo.parallel_map ~f:Lock_dir.get
      >>| List.fold_left
            ~init:(Checksum.Map.empty, Digest.Map.empty)
            ~f:(fun (checksums, urls) (lockdir : Dune_pkg.Lock_dir.t) ->
              let checksums', urls' = extract_checksums_and_urls lockdir in
              Checksum.Map.superpose checksums checksums', Digest.Map.superpose urls urls'))
  in
  let find_url digest =
    let+ _, urls = Memo.Lazy.force all in
    match Digest.Map.find urls digest with
    | Some x -> x
    | None -> User_error.raise [ Pp.textf "unknown digest %s" (Digest.to_string digest) ]
  in
  let find_checksum checksum =
    let+ checksums, _ = Memo.Lazy.force all in
    match Checksum.Map.find checksums checksum with
    | Some x -> x
    | None ->
      User_error.raise [ Pp.textf "unknown checksum %s" (Checksum.to_string checksum) ]
  in
  find_checksum, find_url
;;

let gen_rules_for_checksum_or_url (loc_url, (url : OpamUrl.t)) checksum =
  let checksum_or_url =
    match checksum with
    | Some (_, checksum) -> `Checksum checksum
    | None -> `Url url
  in
  let directory_targets =
    let target_dir = make_target ~kind:`Directory checksum_or_url in
    Path.Build.Map.singleton target_dir Loc.none
  in
  let rules =
    Rules.collect_unit
    @@ fun () ->
    let* url = resolve_url url in
    (* CR-rgrinberg: it's possible to share the downloading step between the
       directory and file actions. Though it's unlikely to be of any use in real
       world situations. *)
    let rule =
      let info = Rule.Info.of_loc_opt (Some loc_url) in
      fun { Action_builder.With_targets.build; targets } ->
        Rules.Produce.rule (Rule.make ~info ~targets build)
    in
    let make_target = make_target checksum_or_url in
    let action ~target ~kind =
      action ~url:(loc_url, url) ~checksum ~target ~kind
      |> Action.Full.make
      |> Action_builder.return
      |> Action_builder.with_no_targets
    in
    let dir_rule =
      let target = make_target ~kind:`Directory in
      action ~target ~kind:`Directory
      |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
    in
    let file_rule =
      let target = make_target ~kind:`File in
      action ~target ~kind:`File
      |> Action_builder.With_targets.add ~file_targets:[ target ]
    in
    let open Memo.O in
    let+ () = rule dir_rule
    and+ () = rule file_rule in
    ()
  in
  Gen_rules.make rules ~directory_targets
;;

let gen_rules ~dir ~components =
  match components with
  | [] ->
    Memo.return Rules.empty
    |> Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Gen_rules.Build_only_sub_dirs.singleton
              ~dir
              (Subdir_set.of_list [ "checksum"; "url" ]))
    |> Memo.return
  | [ ("url" | "checksum") ] ->
    Memo.return Rules.empty
    |> Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
    |> Memo.return
  | [ "checksum"; checksum ] ->
    let checksum = Dune_pkg.Checksum.parse_string_exn (Loc.none, checksum) in
    let+ url, checksum = find_checksum checksum in
    gen_rules_for_checksum_or_url url (Some checksum)
  | [ "url"; digest ] ->
    let+ url =
      match Digest.from_hex digest with
      | Some s -> find_url s
      | None -> User_error.raise [ Pp.textf "invalid digest %s" digest ]
    in
    gen_rules_for_checksum_or_url url None
  | _ -> Memo.return Gen_rules.no_rules
;;

module Copy = struct
  module Spec = struct
    type ('path, 'target) t =
      { src_dir : 'path
      ; dst_dir : 'target
      }

    let name = "copy-dir"
    let version = 1
    let bimap t f g = { src_dir = f t.src_dir; dst_dir = g t.dst_dir }
    let is_useful_to ~memoize = memoize

    let encode { src_dir; dst_dir } path target =
      Dune_lang.List
        [ Dune_lang.atom_or_quoted_string name; path src_dir; target dst_dir ]
    ;;

    let action { src_dir; dst_dir } ~ectx:_ ~eenv:_ =
      let open Fiber.O in
      let+ () = Fiber.return () in
      Path.mkdir_p (Path.build dst_dir);
      let dst_dir = Path.build dst_dir in
      Fpath.traverse
        ~init:()
        ~dir:(Path.to_string src_dir)
        ~on_dir:(fun ~dir fname () ->
          Path.L.relative dst_dir [ dir; fname ] |> Path.mkdir_p)
        ~on_file:(fun ~dir fname () ->
          let src = Path.L.relative src_dir [ dir; fname ] in
          let dst = Path.L.relative dst_dir [ dir; fname ] in
          Io.copy_file ~src ~dst ())
    ;;
  end

  let action ~src_dir ~dst_dir =
    let module M = struct
      type path = Path.t
      type target = Path.Build.t

      module Spec = Spec

      let v = { Spec.dst_dir; src_dir }
    end
    in
    Action.Extension (module M)
  ;;
end

let fetch ~target kind (source : Source.t) =
  let source_kind = Source.kind source in
  let src =
    match source_kind with
    | `Directory_or_archive p -> Path.external_ p
    | `Fetch ->
      let url_or_checksum =
        match source.checksum with
        | Some (_, checksum) -> `Checksum checksum
        | None -> `Url (snd source.url)
      in
      Path.build (make_target ~kind url_or_checksum)
  in
  let open Action_builder.With_targets.O in
  (* [Action_builder.copy] already adds this dependency for us,
     so this is only useful for the [`Directory] clause *)
  Dep.file src
  |> Action_builder.dep
  |> Action_builder.with_no_targets
  >>>
  match kind with
  | `File -> Action_builder.copy ~src ~dst:target
  | `Directory ->
    let action =
      match source_kind with
      | `Fetch -> Copy.action ~src_dir:src ~dst_dir:target
      | `Directory_or_archive _ ->
        (* For local sources, we don't need an intermediate step copying to the
           .fetch context. This would just add pointless additional overhead. *)
        action ~url:source.url ~checksum:source.checksum ~target ~kind
    in
    Action.Full.make action
    |> Action_builder.With_targets.return
    |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

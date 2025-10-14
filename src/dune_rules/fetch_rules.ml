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
  module Ocamlformat = Ocamlformat
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
  let version = 2
  let bimap t _ g = { t with target = g t.target }
  let is_useful_to ~memoize = memoize

  let encode { target; url = _, url; checksum; kind } _ encode_target : Sexp.t =
    List
      ([ encode_target target
       ; Sexp.Atom (OpamUrl.to_string url)
       ; Atom
           (match kind with
            | `File -> "file"
            | `Directory -> "directory")
       ]
       @ (match OpamUrl.rev url with
          | None -> []
          | Some rev -> [ Sexp.Atom rev ])
       @
       match checksum with
       | None -> []
       | Some (_, checksum) -> [ Atom (Checksum.to_string checksum) ])
  ;;

  let action { target; url = loc_url, url; checksum; kind } ~ectx:_ ~eenv:_ =
    let open Fiber.O in
    let* () = Fiber.return () in
    let target = Path.build target in
    (let checksum = Option.map checksum ~f:snd in
     Dune_pkg.Fetch.fetch
       ~unpack:
         (match kind with
          | `File -> false
          | `Directory -> true)
       ~checksum
       ~target
       ~url:(loc_url, url))
    >>= function
    | Ok () ->
      (match kind with
       | `File -> ()
       | `Directory ->
         (* Delete any broken symlinks from the unpacked archive. Dune can't
            handle broken symlinks in the _build directory, but some opam
            package contain broken symlinks. The logic here is applied to the
            contents of package source archives but not to packages whose source
            is in a local directory (e.g. when a package is pinned from the
            filesystem). Broken symlinks are excluded while copying files from
            local directories into the build directory, and the logic for
            excluding them lives in [Pkg_rules.source_rules]. *)
         let target_abs = Path.to_absolute_filename target in
         Fpath.traverse
           ~init:()
           ~dir:target_abs
           ~on_dir:(fun ~dir:_ _ () -> ())
           ~on_file:(fun ~dir:_ _ () -> ())
           ~on_broken_symlink:(fun ~dir fname () ->
             let path = Filename.concat target_abs (Filename.concat dir fname) in
             Fpath.rm_rf path));
      Fiber.return ()
    | Error (Checksum_mismatch actual_checksum) ->
      (match checksum with
       | None ->
         User_error.raise
           ~loc:loc_url
           [ Pp.text "No checksum provided. It should be:"; Checksum.pp actual_checksum ]
       | Some (loc, _) ->
         let loc = Dune_pkg.Lock_dir.loc_in_source_tree loc in
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

module A = Action_ext.Make (Spec)

let action ~url ~checksum ~target ~kind = A.action { Spec.target; checksum; url; kind }

let extract_checksums_and_urls (lockdir : Dune_pkg.Lock_dir.t) =
  Dune_pkg.Lock_dir.Packages.to_pkg_list lockdir.packages
  |> List.fold_left
       ~init:(Checksum.Map.empty, Dune_digest.Map.empty)
       ~f:(fun acc (package : Lock_dir.Pkg.t) ->
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
  let add_checksums_and_urls (checksums, urls) lockdir =
    let checksums', urls' = extract_checksums_and_urls lockdir in
    Checksum.Map.superpose checksums checksums', Digest.Map.superpose urls urls'
  in
  let all =
    Memo.lazy_ (fun () ->
      let* init =
        Memo.List.fold_left
          Dune_pkg.Dev_tool.all
          ~init:(Checksum.Map.empty, Digest.Map.empty)
          ~f:(fun acc dev_tool ->
            let dir = Lock_dir.dev_tool_source_lock_dir dev_tool in
            let exists =
              (* Note we use [Path.Untracked] here rather than [Fs_memo] because a tool's
                 lockdir may be generated part way through a build. *)
              Path.Untracked.exists (Path.source dir)
            in
            match exists with
            | false -> Memo.return acc
            | true -> Lock_dir.of_dev_tool dev_tool >>| add_checksums_and_urls acc)
      in
      Per_context.list ()
      >>= Memo.parallel_map ~f:(fun ctx_name ->
        let* active = Lock_dir.lock_dir_active ctx_name in
        match active with
        | true -> Lock_dir.get ctx_name
        | false ->
          Memo.return
          @@ Error
               (User_message.make
                  [ Pp.textf
                      "Context %S has no lock dir"
                      (Context_name.to_string ctx_name)
                  ]))
      >>| List.filter_map ~f:Result.to_option
      >>| List.fold_left ~init ~f:add_checksums_and_urls)
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
  let loc_url = Dune_pkg.Lock_dir.loc_in_source_tree loc_url in
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
    (* CR-someday rgrinberg: it's possible to share the downloading step between the
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
      |> Action.Full.make ~can_go_in_shared_cache:true
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
    let version = 2
    let bimap t f g = { src_dir = f t.src_dir; dst_dir = g t.dst_dir }
    let is_useful_to ~memoize = memoize

    let encode { src_dir; dst_dir } path target : Sexp.t =
      List [ path src_dir; target dst_dir ]
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
        ~on_broken_symlink:(fun ~dir:_ _fname () -> ())
    ;;
  end

  module A = Action_ext.Make (Spec)

  let action ~src_dir ~dst_dir = A.action { Spec.dst_dir; src_dir }
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
    Action.Full.make ~can_go_in_shared_cache:true action
    |> Action_builder.With_targets.return
    |> Action_builder.With_targets.add_directories ~directory_targets:[ target ]
;;

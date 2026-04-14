open Import

(* CR-someday amokhov: We probably want to add a new variant [Dir] to provide
   first-class support for depending on directory targets. *)
module T = struct
  type t =
    | Env of Env.Var.t
    | File of Path.t
    | Alias of Alias.t
    | File_selector of File_selector.t
    | Universe

  module Stable_for_digest = struct
    type t =
      | Env of string
      | File of Digest.t (* Digest of the underlying [Path.t] *)
      | Alias of
          { dir : string
          ; name : string
          }
      | File_selector of Digest.t (* Digest of the underlying [File_selector.t] *)
      | Universe

    let digest d = function
      | Env var ->
        Digest.Manual.int d 0;
        Digest.Manual.string d var
      | File digest ->
        Digest.Manual.int d 1;
        Digest.Manual.digest d digest
      | Alias { dir; name } ->
        Digest.Manual.int d 2;
        Digest.Manual.string d dir;
        Digest.Manual.string d name
      | File_selector digest ->
        Digest.Manual.int d 3;
        Digest.Manual.digest d digest
      | Universe -> Digest.Manual.int d 4
    ;;
  end

  let env e = Env e
  let file f = File f
  let alias a = Alias a
  let universe = Universe
  let file_selector g = File_selector g

  let compare x y =
    match x, y with
    | Env x, Env y -> Env.Var.compare x y
    | Env _, _ -> Lt
    | _, Env _ -> Gt
    | File x, File y -> Path.compare x y
    | File _, _ -> Lt
    | _, File _ -> Gt
    | Alias x, Alias y -> Alias.compare x y
    | Alias _, _ -> Lt
    | _, Alias _ -> Gt
    | File_selector x, File_selector y -> File_selector.compare x y
    | File_selector _, _ -> Lt
    | _, File_selector _ -> Gt
    | Universe, Universe -> Ordering.Eq
  ;;

  let to_dyn t =
    let open Dyn in
    match t with
    | File_selector g -> variant "File_selector" [ File_selector.to_dyn g ]
    | Env e -> variant "Env" [ string e ]
    | File f -> variant "File" [ Path.to_dyn f ]
    | Alias a -> variant "Alias" [ Alias.to_dyn a ]
    | Universe -> variant "Universe" []
  ;;
end

include T

module Map = struct
  module M = Map.Make (T)
  include M
  include Memo.Make_parallel_map (M)

  let has_universe t = mem t Universe
end

let as_in_build_dir_no_source = function
  | Path.In_source_tree _ ->
    Code_error.raise "we don't depend on paths from the source" []
  | External _ -> None
  | In_build_dir p -> Some p
;;

module Fact = struct
  module Files = struct
    type t =
      { files : Path.Set.t
      ; empty_dirs : Path.Build.Set.t (* For [File_selector]s that match no files *)
      ; digest : Digest.t (* Includes [empty_dirs], [files] and their content digests *)
      }

    let to_dyn { files; empty_dirs; digest } =
      Dyn.Record
        [ "files", Path.Set.to_dyn files
        ; "empty_dirs", Path.Build.Set.to_dyn empty_dirs
        ; "digest", Digest.to_dyn digest
        ]
    ;;

    let is_empty t = Path.Set.is_empty t.files && Path.Build.Set.is_empty t.empty_dirs
    let compare a b = Digest.compare a.digest b.digest
    let equal a b = Digest.equal a.digest b.digest

    (* The caller should ensure that [files] and [digests] are listed in the same order *)
    let combined_digest ?empty_dir (files : Path.t list) (digests : Digest.t list) =
      let d = Digest.Manual.create () in
      Digest.Manual.option
        d
        ~f:(fun d path -> Digest.Manual.repr d Path.Build.repr path)
        empty_dir;
      Digest.Manual.list d files ~f:(fun d path -> Digest.Manual.repr d Path.repr path);
      Digest.Manual.list d digests ~f:Digest.Manual.digest;
      Digest.Manual.get d
    ;;

    let create files ~build_file =
      let open Memo.O in
      let empty_dir =
        if Filename_set.is_empty files
        then Path.as_in_build_dir (Filename_set.dir files)
        else None
      in
      let empty_dirs =
        match empty_dir with
        | None -> Path.Build.Set.empty
        | Some dir -> Path.Build.Set.singleton dir
      in
      let files = Filename_set.to_list files in
      let+ digests = Memo.parallel_map files ~f:build_file in
      { files = Path.Set.of_list files
      ; empty_dirs
      ; digest = combined_digest ?empty_dir files digests
      }
    ;;

    let of_file_digest_map file_digest_map =
      let files = Path.Set.of_keys file_digest_map in
      let digests = Path.Map.values file_digest_map in
      { files
      ; empty_dirs = Path.Build.Set.empty
      ; digest = combined_digest (Path.Set.to_list files) digests
      }
    ;;

    let necessary_dirs_for_sandboxing { files; empty_dirs; digest = _ } =
      let f (path : Path.t) acc =
        match as_in_build_dir_no_source path with
        | None -> acc
        | Some p -> Path.Build.Set.add acc (Path.Build.parent_exn p)
      in
      Path.Set.fold files ~init:empty_dirs ~f
    ;;

    let empty =
      { files = Path.Set.empty
      ; empty_dirs = Path.Build.Set.empty
      ; digest =
          (let d = Digest.Manual.create () in
           Digest.Manual.list d [] ~f:Digest.Manual.digest;
           Digest.Manual.get d)
      }
    ;;

    let group ts files =
      let ts = if Path.Map.is_empty files then ts else of_file_digest_map files :: ts in
      (* Sort and de-dup so that the result is resilient to code changes *)
      let ts =
        List.filter_map ts ~f:(fun t -> if is_empty t then None else Some (t.digest, t))
        |> Digest.Map.of_list_reduce ~f:(fun t _ -> t)
        |> Digest.Map.values
      in
      match ts with
      | [] -> empty
      | [ t ] -> t
      | ts ->
        { files = Path.Set.union_map ts ~f:(fun t -> t.files)
        ; empty_dirs = Path.Build.Set.union_map ts ~f:(fun t -> t.empty_dirs)
        ; digest =
            (let d = Digest.Manual.create () in
             Digest.Manual.list
               d
               (List.map ts ~f:(fun t -> t.digest))
               ~f:Digest.Manual.digest;
             Digest.Manual.get d)
        }
    ;;
  end

  type t =
    | Nothing
    | File of Path.t * Digest.t
    | File_selector of
        { file_selector_digest : Digest.t
        ; facts : Files.t
        }
    | Alias of Files.t

  let to_dyn = function
    | Nothing -> Dyn.Variant ("Nothing", [])
    | File (path, digest) ->
      Dyn.Variant
        ( "File"
        , [ Dyn.Record [ "path", Path.to_dyn path; "digest", Digest.to_dyn digest ] ] )
    | File_selector { file_selector_digest; facts } ->
      Dyn.Variant
        ( "File_selector"
        , [ Dyn.Record
              [ "file_selector_digest", Digest.to_dyn file_selector_digest
              ; "facts", Files.to_dyn facts
              ]
          ] )
    | Alias facts -> Dyn.Variant ("Alias", [ Dyn.Record [ "facts", Files.to_dyn facts ] ])
  ;;

  module Stable_for_digest = struct
    type t =
      | Env of string * string option
      | File of
          { path_digest : Digest.t
          ; file_digest : Digest.t
          }
      | File_selector of
          { file_selector_digest : Digest.t
          ; facts_digest : Digest.t
          }
      | Alias of Digest.t

    let digest d = function
      | Env (var, value) ->
        Digest.Manual.int d 0;
        Digest.Manual.string d var;
        Digest.Manual.option d ~f:Digest.Manual.string value
      | File { path_digest; file_digest } ->
        Digest.Manual.int d 1;
        Digest.Manual.digest d path_digest;
        Digest.Manual.digest d file_digest
      | File_selector { file_selector_digest; facts_digest } ->
        Digest.Manual.int d 2;
        Digest.Manual.digest d file_selector_digest;
        Digest.Manual.digest d facts_digest
      | Alias digest ->
        Digest.Manual.int d 3;
        Digest.Manual.digest d digest
    ;;
  end

  let compare a b =
    let open Ordering.O in
    match a, b with
    | Nothing, Nothing -> Eq
    | Nothing, _ -> Lt
    | _, Nothing -> Gt
    | File (f1, d1), File (f2, d2) ->
      let= () = Path.compare f1 f2 in
      Digest.compare d1 d2
    | File _, _ -> Lt
    | _, File _ -> Gt
    | ( File_selector { file_selector_digest = d1; facts = f1 }
      , File_selector { file_selector_digest = d2; facts = f2 } ) ->
      let= () = Digest.compare d1 d2 in
      Files.compare f1 f2
    | File_selector _, _ -> Lt
    | _, File_selector _ -> Gt
    | Alias f1, Alias f2 -> Files.compare f1 f2
  ;;

  let equal a b =
    match compare a b with
    | Eq -> true
    | _ -> false
  ;;

  let nothing = Nothing
  let file fn digest = File (fn, digest)

  let file_selector fs facts =
    File_selector { file_selector_digest = File_selector.digest fs; facts }
  ;;

  let alias _alias files = Alias files
end

module Set = struct
  module M = Set.Of_map (T) (Map)
  include M

  let of_files l = of_list_map l ~f:file
  let of_files_set = Path.Set.fold ~init:empty ~f:(fun f acc -> add acc (file f))
  let add_paths t paths = Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  (* This is to force the rules to be loaded for directories without files when
     depending on [(source_tree x)]. Otherwise, we wouldn't clean up stale
     directories in directories that contain no file. *)
  let dir_without_files_dep dir =
    file_selector (File_selector.of_predicate_lang ~dir Predicate_lang.false_)
  ;;

  let of_source_files ~files ~empty_directories =
    let init = Path.Set.fold files ~init:empty ~f:(fun path acc -> add acc (file path)) in
    Path.Set.fold empty_directories ~init ~f:(fun path acc ->
      add acc (dir_without_files_dep path))
  ;;

  let digest t digest =
    iter t ~f:(fun dep ->
      match dep with
      | Env var -> Stable_for_digest.digest digest (Stable_for_digest.Env var)
      | Universe -> Stable_for_digest.digest digest Stable_for_digest.Universe
      | File p ->
        Stable_for_digest.digest
          digest
          (Stable_for_digest.File (Path.to_string p |> Digest.string))
      | File_selector fs ->
        Stable_for_digest.digest
          digest
          (Stable_for_digest.File_selector (File_selector.digest fs))
      | Alias a ->
        Stable_for_digest.digest
          digest
          (Stable_for_digest.Alias
             { dir = Path.Build.to_string (Alias.dir a)
             ; name = Alias.Name.to_string (Alias.name a)
             }))
  ;;
end

module Facts = struct
  type t = Fact.t Map.t

  let singleton = Map.singleton
  let equal x y = Map.equal ~equal:Fact.equal x y
  let empty = Map.empty
  let record_facts set ~f = Map.parallel_map set ~f:(fun dep () -> f dep)

  let union a b =
    Map.union a b ~f:(fun _ a b ->
      assert (a = b);
      Some a)
  ;;

  let union_all xs = List.fold_left xs ~init:Map.empty ~f:union
  let to_dyn = Map.to_dyn Fact.to_dyn

  let paths t ~expand_aliases =
    Map.fold t ~init:Path.Set.empty ~f:(fun fact acc ->
      match (fact : Fact.t) with
      | Nothing -> acc
      | File (path, _digest) -> Path.Set.add acc path
      | File_selector { file_selector_digest = _; facts } ->
        Path.Set.union acc facts.files
      | Alias facts -> if expand_aliases then Path.Set.union acc facts.files else acc)
  ;;

  let group_paths_as_fact_files ts =
    let fact_files, paths =
      List.fold_left ts ~init:([], Path.Map.empty) ~f:(fun acc t ->
        Map.fold t ~init:acc ~f:(fun fact ((acc_ff, acc_paths) as acc) ->
          match (fact : Fact.t) with
          | Nothing -> acc
          | File (p, d) -> acc_ff, Path.Map.set acc_paths p d
          | File_selector { file_selector_digest = _; facts } | Alias facts ->
            facts :: acc_ff, acc_paths))
    in
    Fact.Files.group fact_files paths
  ;;

  let necessary_dirs_for_sandboxing t =
    Map.fold t ~init:Path.Build.Set.empty ~f:(fun fact acc ->
      match (fact : Fact.t) with
      | Nothing -> acc
      | File (p, _) ->
        (match as_in_build_dir_no_source p with
         | None -> acc
         | Some p ->
           let p = Path.Build.parent_exn p in
           Path.Build.Set.add acc p)
      | File_selector { file_selector_digest = _; facts } | Alias facts ->
        Path.Build.Set.union_all [ acc; Fact.Files.necessary_dirs_for_sandboxing facts ])
  ;;

  let digest t digest ~env =
    Map.iteri t ~f:(fun dep fact ->
      match dep with
      | Env var ->
        Fact.Stable_for_digest.digest
          digest
          (Fact.Stable_for_digest.Env (var, Env.get env var))
      | Universe -> ()
      | File _ | File_selector _ | Alias _ ->
        (match (fact : Fact.t) with
         | Nothing -> ()
         | File (p, d) ->
           Fact.Stable_for_digest.digest
             digest
             (Fact.Stable_for_digest.File
                { path_digest = Digest.string (Path.to_string p); file_digest = d })
         | File_selector { file_selector_digest; facts } ->
           Fact.Stable_for_digest.digest
             digest
             (Fact.Stable_for_digest.File_selector
                { file_selector_digest; facts_digest = facts.digest })
         | Alias ps ->
           Fact.Stable_for_digest.digest digest (Fact.Stable_for_digest.Alias ps.digest)))
  ;;
end

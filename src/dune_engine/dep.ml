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
      | File of string
      | Alias of
          { dir : string
          ; name : string
          }
      | File_selector of Dyn.t
      | Universe
  end

  let env e = Env e

  let file f = File f

  let alias a = Alias a

  let universe = Universe

  let file_selector g = File_selector g

  let compare x y =
    match (x, y) with
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

  let encode t =
    let open Dune_sexp.Encoder in
    match t with
    | File_selector g -> pair string File_selector.encode ("glob", g)
    | Env e -> pair string string ("Env", e)
    | File f -> pair string Dpath.encode ("File", f)
    | Alias a -> pair string Alias.encode ("Alias", a)
    | Universe -> string "Universe"

  let to_dyn t = Dyn.String (Dune_sexp.to_string (encode t))
end

include T

module Map = struct
  module M = Map.Make (T)
  include M
  include Memo.Make_map_traversals (M)

  let has_universe t = mem t Universe
end

let as_in_build_dir_no_source = function
  | Path.In_source_tree _ ->
    Code_error.raise "we don't depend on paths from the source" []
  | External _ -> None
  | In_build_dir p -> Some p

module Fact = struct
  (* CR-someday amokhov: Find a better name, perhaps, [Files_and_dirs]? *)
  module Files = struct
    type t =
      { files : Digest.t Path.Map.t
      ; dirs : Digest.t Path.Map.t (* Only for file selectors for now *)
      ; digest : Digest.t
      }

    let to_dyn { files; dirs; digest } =
      Dyn.Record
        [ ("files", Path.Map.to_dyn Digest.to_dyn files)
        ; ("dirs", Path.Map.to_dyn Digest.to_dyn dirs)
        ; ("digest", Digest.to_dyn digest)
        ]

    let is_empty t = Path.Map.is_empty t.files && Path.Map.is_empty t.dirs

    let compare a b = Digest.compare a.digest b.digest

    let equal a b = Digest.equal a.digest b.digest

    let paths t = t.files

    let make ~files ~dirs =
      { files
      ; dirs
      ; digest =
          Digest.generic
            ( Path.Map.to_list_map files ~f:(fun p d -> (Path.to_string p, d))
            , Path.Map.to_list_map dirs ~f:(fun p d -> (Path.to_string p, d)) )
      }

    let necessary_dirs_for_sandboxing { files; dirs; digest = _ } =
      let f (path : Path.t) (_ : Digest.t) acc =
        match as_in_build_dir_no_source path with
        | None -> acc
        | Some p -> Path.Build.Set.add acc (Path.Build.parent_exn p)
      in
      let init = Path.Map.foldi files ~init:Path.Build.Set.empty ~f in
      Path.Map.foldi dirs ~init ~f

    let empty = lazy (make ~files:Path.Map.empty ~dirs:Path.Map.empty)

    let group ts files =
      let ts =
        if Path.Map.is_empty files then ts
        else make ~files ~dirs:Path.Map.empty :: ts
      in
      (* Sort and de-dup so that the result is resilient to code changes *)
      let ts =
        List.filter_map ts ~f:(fun t ->
            if is_empty t then None else Some (t.digest, t))
        |> Digest.Map.of_list_reduce ~f:(fun t _ -> t)
        |> Digest.Map.values
      in
      match ts with
      | [] -> Lazy.force empty
      | [ t ] -> t
      | t :: l ->
        { files =
            List.fold_left l ~init:t.files ~f:(fun acc t ->
                Path.Map.union t.files acc ~f:(fun _ d1 d2 ->
                    assert (Digest.equal d1 d2);
                    Some d1))
        ; dirs =
            List.fold_left l ~init:t.dirs ~f:(fun acc t ->
                Path.Map.union t.dirs acc ~f:(fun _ d1 d2 ->
                    assert (Digest.equal d1 d2);
                    Some d1))
        ; digest = Digest.generic (List.map ts ~f:(fun t -> t.digest))
        }
  end

  type t =
    | Nothing
    | File of Path.t * Digest.t
    | File_selector of Dyn.t * Files.t
    | Alias of Files.t

  let to_dyn = function
    | Nothing -> Dyn.Variant ("Nothing", [])
    | File (path, digest) ->
      Dyn.Variant
        ( "File"
        , [ Dyn.Record
              [ ("path", Path.to_dyn path); ("digest", Digest.to_dyn digest) ]
          ] )
    | File_selector (dyn, files) ->
      Dyn.Variant
        ( "File_selector"
        , [ Dyn.Record [ ("dyn", dyn); ("files", Files.to_dyn files) ] ] )
    | Alias files ->
      Dyn.Variant ("Alias", [ Dyn.Record [ ("files", Files.to_dyn files) ] ])

  module Stable_for_digest = struct
    type file = string * Digest.t

    type t =
      | Env of string * string option
      | File of file
      | File_selector of Dyn.t * Digest.t
      | Alias of Digest.t
  end

  let compare a b =
    let open Ordering.O in
    match (a, b) with
    | Nothing, Nothing -> Eq
    | Nothing, _ -> Lt
    | _, Nothing -> Gt
    | File (f1, d1), File (f2, d2) ->
      let= () = Path.compare f1 f2 in
      Digest.compare d1 d2
    | File _, _ -> Lt
    | _, File _ -> Gt
    | File_selector (d1, f1), File_selector (d2, f2) ->
      let= () = Dyn.compare d1 d2 in
      Files.compare f1 f2
    | File_selector _, _ -> Lt
    | _, File_selector _ -> Gt
    | Alias f1, Alias f2 -> Files.compare f1 f2

  let equal a b =
    match compare a b with
    | Eq -> true
    | _ -> false

  let nothing = Nothing

  let file fn digest = File (fn, digest)

  let file_selector fs files =
    let id = File_selector.to_dyn fs in
    File_selector (id, files)

  let alias _alias files = Alias files
end

module Facts = struct
  type t = Fact.t Map.t

  let empty = Map.empty

  let union a b =
    Map.union a b ~f:(fun _ a b ->
        assert (a = b);
        Some a)

  let union_all xs = List.fold_left xs ~init:Map.empty ~f:union

  let to_dyn = Map.to_dyn Fact.to_dyn

  let paths t =
    Map.fold t ~init:Path.Map.empty ~f:(fun fact acc ->
        match (fact : Fact.t) with
        | Nothing -> acc
        | File (p, d) -> Path.Map.set acc p d
        | File_selector (_, ps) | Alias ps ->
          Path.Map.union acc ps.files ~f:(fun _ a _ -> Some a))

  let paths_without_expanding_aliases t =
    Map.fold t ~init:Path.Map.empty ~f:(fun fact acc ->
        match (fact : Fact.t) with
        | Nothing | Alias _ -> acc
        | File (p, d) -> Path.Map.set acc p d
        | File_selector (_, ps) ->
          Path.Map.union acc ps.files ~f:(fun _ a _ -> Some a))

  let group_paths_as_fact_files ts =
    let fact_files, paths =
      List.fold_left ts ~init:([], Path.Map.empty) ~f:(fun acc t ->
          Map.fold t ~init:acc ~f:(fun fact ((acc_ff, acc_paths) as acc) ->
              match (fact : Fact.t) with
              | Nothing -> acc
              | File (p, d) -> (acc_ff, Path.Map.set acc_paths p d)
              | File_selector (_, ps) | Alias ps -> (ps :: acc_ff, acc_paths)))
    in
    Fact.Files.group fact_files paths

  let necessary_dirs_for_sandboxing t =
    Map.fold t ~init:Path.Build.Set.empty ~f:(fun fact acc ->
        match (fact : Fact.t) with
        | Nothing -> acc
        | File (p, _) -> (
          match as_in_build_dir_no_source p with
          | None -> acc
          | Some p ->
            let p = Path.Build.parent_exn p in
            Path.Build.Set.add acc p)
        | File_selector (_, ps) | Alias ps ->
          Path.Build.Set.union_all
            [ acc
            ; Path.Map.keys ps.dirs
              |> List.filter_map ~f:as_in_build_dir_no_source
              |> Path.Build.Set.of_list
            ; Fact.Files.necessary_dirs_for_sandboxing ps
            ])

  let digest t ~env =
    let facts =
      let file (p, d) = (Path.to_string p, d) in
      Map.foldi t ~init:[]
        ~f:(fun dep fact acc : Fact.Stable_for_digest.t list ->
          match dep with
          | Env var -> Env (var, Env.get env var) :: acc
          | Universe -> acc
          | File _ | File_selector _ | Alias _ -> (
            match (fact : Fact.t) with
            | Nothing -> acc
            | File (p, d) -> File (file (p, d)) :: acc
            | File_selector (id, ps) -> File_selector (id, ps.digest) :: acc
            | Alias ps -> Alias ps.digest :: acc))
    in
    Digest.generic facts
end

module Set = struct
  module M = Set.Of_map (T) (Map)
  include M

  let of_files l = of_list_map l ~f:file

  let of_files_set =
    Path.Set.fold ~init:empty ~f:(fun f acc -> add acc (file f))

  let add_paths t paths =
    Path.Set.fold paths ~init:t ~f:(fun p set -> add set (File p))

  let encode t = Dune_sexp.Encoder.list encode (to_list t)

  (* This is to force the rules to be loaded for directories without files when
     depending on [(source_tree x)]. Otherwise, we wouldn't clean up stale
     directories in directories that contain no file. *)
  let dir_without_files_dep dir =
    file_selector
      (File_selector.create ~dir File_selector.Predicate_with_id.false_)

  let of_source_files ~files ~empty_directories =
    let init =
      Path.Set.fold files ~init:empty ~f:(fun path acc -> add acc (file path))
    in
    Path.Set.fold empty_directories ~init ~f:(fun path acc ->
        add acc (dir_without_files_dep path))

  let digest t =
    fold t ~init:[] ~f:(fun dep acc : Stable_for_digest.t list ->
        match dep with
        | Env var -> Env var :: acc
        | Universe -> Universe :: acc
        | File p -> File (Path.to_string p) :: acc
        | File_selector fs -> File_selector (File_selector.to_dyn fs) :: acc
        | Alias a ->
          Alias
            { dir = Path.Build.to_string (Alias.dir a)
            ; name = Alias.Name.to_string (Alias.name a)
            }
          :: acc)
    |> Digest.generic
end

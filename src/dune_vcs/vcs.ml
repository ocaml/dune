open Stdune
module Utils = Dune_engine.Utils
module Process = Dune_engine.Process

module Kind = struct
  type t =
    | Git
    | Hg

  let of_dir_name = function
    | ".git" -> Some Git
    | ".hg" -> Some Hg
    | _ -> None
  ;;

  let of_dir_contents set =
    match Filename.Set.find set ~f:(fun s -> Option.is_some (of_dir_name s)) with
    | None -> None
    | Some s -> Some (Option.value_exn (of_dir_name s))
  ;;

  let to_dyn t =
    Dyn.Variant
      ( (match t with
         | Git -> "Git"
         | Hg -> "Hg")
      , [] )
  ;;

  let equal = ( = )
end

module T = struct
  type t =
    { root : Path.t
    ; kind : Kind.t
    }

  let to_dyn { root; kind } =
    Dyn.record [ "root", Path.to_dyn root; "kind", Kind.to_dyn kind ]
  ;;

  let equal { root = ra; kind = ka } { root = rb; kind = kb } =
    Path.equal ra rb && Kind.equal ka kb
  ;;

  (* No need to hash the kind as there is only only kind per directory *)
  let hash t = Path.hash t.root
end

include T

let git, hg =
  let get prog =
    lazy
      (match Bin.which ~path:(Env_path.path Env.initial) prog with
       | Some x -> x
       | None -> Utils.program_not_found prog ~loc:None)
  in
  get "git", get "hg"
;;

let select git hg t =
  Memo.of_non_reproducible_fiber
    (match t.kind with
     | Git -> git t
     | Hg -> hg t)
;;

let prog t =
  Lazy.force
    (match t.kind with
     | Git -> git
     | Hg -> hg)
;;

let run t args =
  let open Fiber.O in
  let+ s =
    Process.run_capture ~display:Quiet Strict (prog t) args ~dir:t.root ~env:Env.initial
  in
  String.trim s
;;

let git_accept () : (_, _) Process.Failure_mode.t =
  Accept (Predicate.create (fun x -> Int.equal x 0 || Int.equal x 128))
;;

let run_git t args =
  let res =
    Process.run_capture
      (git_accept ())
      ~display:Quiet
      (prog t)
      args
      ~dir:t.root
      ~env:Env.initial
      ~stderr_to:(Process.Io.file Dev_null.path Out)
  in
  let open Fiber.O in
  let+ res = res in
  match res with
  | Ok s -> Some (String.trim s)
  | Error 128 -> None
  | Error _ -> assert false
;;

let hg_describe t =
  let open Fiber.O in
  let* s = run t [ "log"; "--rev"; "."; "-T"; "{latesttag} {latesttagdistance}" ] in
  let+ id = run t [ "id"; "-i" ] in
  let id, dirty_suffix =
    match String.drop_suffix id ~suffix:"+" with
    | Some id -> id, "-dirty"
    | None -> id, ""
  in
  let s =
    let s, dist = Option.value_exn (String.rsplit2 s ~on:' ') in
    match s with
    | "null" -> id
    | _ ->
      (match Int.of_string dist with
       | Some 1 -> s
       | Some n -> sprintf "%s-%d-%s" s (n - 1) id
       | None -> sprintf "%s-%s-%s" s dist id)
  in
  s ^ dirty_suffix
;;

let make_fun name ~git ~hg =
  let memo = Memo.create name ~input:(module T) (select git hg) in
  Staged.stage (Memo.exec memo)
;;

let describe =
  Staged.unstage
  @@ make_fun
       "vcs-describe"
       ~git:(fun t -> run_git t [ "describe"; "--always"; "--dirty"; "--abbrev=7" ])
       ~hg:(fun x ->
         let open Fiber.O in
         let+ res = hg_describe x in
         Some res)
;;

let commit_id =
  Staged.unstage
  @@ make_fun
       "vcs-commit-id"
       ~git:(fun t -> run_git t [ "rev-parse"; "HEAD" ])
       ~hg:(fun t ->
         let open Fiber.O in
         let+ res = run t [ "id"; "-i" ] in
         Some res)
;;

let files =
  let run_zero_separated_hg t args =
    Process.run_capture_zero_separated
      Strict
      (prog t)
      args
      ~display:Quiet
      ~dir:t.root
      ~env:Env.initial
  in
  let run_zero_separated_git t args =
    let open Fiber.O in
    let+ res =
      Process.run_capture_zero_separated
        (git_accept ())
        (prog t)
        args
        ~display:Quiet
        ~dir:t.root
        ~env:Env.initial
    in
    match res with
    | Ok s -> s
    | Error 128 -> []
    | Error _ -> assert false
  in
  let f run args t =
    let open Fiber.O in
    let f =
      let prefix =
        Path.to_absolute_filename t.root |> Path.External.of_string |> Path.external_
      in
      let source_root =
        Path.to_absolute_filename (Path.source Path.Source.root)
        |> Path.External.of_string
        |> Path.external_
      in
      match Path.drop_prefix source_root ~prefix with
      | None -> fun p -> Some (Path.Source.of_string p)
      | Some prefix ->
        fun p ->
          (match
             let p = Path.Local.of_string p in
             Path.Local.descendant p ~of_:prefix
           with
           | None -> None
           | Some p -> Some (Path.Source.of_local p))
    in
    run t args >>| List.filter_map ~f
  in
  Staged.unstage
  @@ make_fun
       "vcs-files"
       ~git:(f run_zero_separated_git [ "ls-tree"; "-z"; "-r"; "--name-only"; "HEAD" ])
       ~hg:(f run_zero_separated_hg [ "files"; "-0" ])
;;

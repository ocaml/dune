open Import
open Memo.O

let resolve_program sctx ~loc ~dir name =
  Super_context.resolve_program_memo
    sctx
    ~loc
    ~dir
    ~where:Original_path
    ~hint:"opam install melange"
    name
;;

let melc sctx ~loc ~dir = resolve_program sctx ~loc ~dir "melc"
let melobjinfo sctx ~loc ~dir = resolve_program sctx ~loc ~dir "melobjinfo"

let available sctx ~dir =
  let+ melc = melc sctx ~loc:None ~dir in
  Result.is_ok melc
;;

let where =
  let impl bin =
    let* _ = Build_system.build_file bin in
    let+ where =
      Memo.of_reproducible_fiber
      @@ Process.run_capture_line ~display:Quiet Strict bin [ "--where" ]
    in
    Bin.parse_path where
  in
  let memo =
    Memo.create "melange-where" ~input:(module Path) ~cutoff:(List.equal Path.equal) impl
  in
  fun sctx ~loc ~dir ->
    let* env = Super_context.env_node sctx ~dir >>= Env_node.external_env in
    match Env.get env (Env.Var.of_string "MELANGELIB") with
    | Some value ->
      let dirs = Bin.parse value |> List.filter ~f:(fun s -> not (String.is_empty s)) in
      if List.exists dirs ~f:Filename.is_relative
      then
        let* melc = melc sctx ~loc ~dir in
        match melc with
        | Error _ -> Memo.return []
        | Ok melc ->
          let+ _ = Build_system.build_file melc in
          let bin_dir =
            Path.to_absolute_filename melc
            |> Unix.realpath
            |> Path.of_string
            |> Path.parent_exn
          in
          List.map dirs ~f:(Path.relative bin_dir)
      else Memo.return (List.map dirs ~f:Path.of_string)
    | None ->
      let* melc = melc sctx ~loc ~dir in
      (match melc with
       | Error _ -> Memo.return []
       | Ok melc -> Memo.exec memo melc)
;;

let stdlib_glob =
  let glob kinds =
    List.map kinds ~f:(fun kind -> Lib_mode.Cm_kind.ext (Melange kind))
    |> Glob.matching_extensions
  in
  let cmi = glob [ Melange.Cm_kind.Cmi ] in
  let cmj = glob [ Cmi; Cmj ] in
  function
  | Melange.Cm_kind.Cmi -> cmi
  | Cmj -> cmj
;;

let stdlib_deps sctx ~loc ~dir ~cm_kind =
  let* dirs = where sctx ~loc ~dir in
  let+ dirs = Memo.List.map dirs ~f:Ocaml_toolchain.localize_stdlib_dir in
  let glob = stdlib_glob cm_kind in
  List.map dirs ~f:(fun dir -> File_selector.of_glob ~dir glob |> Dep.file_selector)
  |> Dep.Set.of_list
;;

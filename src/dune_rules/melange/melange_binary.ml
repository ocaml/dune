open Import
open Memo.O

let melc sctx ~loc ~dir =
  Super_context.resolve_program_memo
    sctx
    ~loc
    ~dir
    ~where:Original_path
    ~hint:"opam install melange"
    "melc"
;;

let available sctx ~dir =
  let+ melc = melc sctx ~loc:None ~dir in
  Result.is_ok melc
;;

let effective_lib_modes sctx ~dir (modes : Lib_mode.Map.Set.t) =
  let has_ocaml_mode = modes.ocaml.byte || modes.ocaml.native in
  if modes.melange && has_ocaml_mode
  then
    let+ available = available sctx ~dir in
    if available then modes else { modes with melange = false }
  else Memo.return modes
;;

let where =
  let impl bin =
    let* _ = Build_system.build_file bin in
    let+ where =
      Memo.of_reproducible_fiber
      @@ Process.run_capture_line ~display:Quiet Strict bin [ "--where" ]
    in
    where
  in
  let memo = Memo.create "melange-where" ~input:(module Path) ~cutoff:String.equal impl in
  fun sctx ~loc ~dir ->
    let* env = Super_context.env_node sctx ~dir >>= Env_node.external_env in
    let+ melange_dirs =
      match Env.get env "MELANGELIB" with
      | Some p -> Memo.return (Some p)
      | None ->
        let* melc = melc sctx ~loc ~dir in
        (match melc with
         | Error _ -> Memo.return None
         | Ok melc ->
           let+ res = Memo.exec memo melc in
           Some res)
    in
    match melange_dirs with
    | None -> []
    | Some dirs -> Bin.parse_path dirs
;;

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

let supports_small_heap_limit =
  let module Input = struct
    type t = Path.t * Path.Build.t * Env.t

    let equal = Tuple.T3.equal Path.equal Path.Build.equal Env.equal
    let hash = Tuple.T3.hash Path.hash Path.Build.hash Env.hash
    let repr = Repr.triple Path.repr Path.Build.repr (Repr.abstract Env.to_dyn)
    let to_dyn = Repr.to_dyn repr
  end
  in
  let impl (bin, dir, env) =
    let* () = Build_system.build_file bin in
    Memo.of_reproducible_fiber
    @@ let open Fiber.O in
       Fiber.Temp.with_temp_file
         ~prefix:"melange"
         ~suffix:"runtime"
         ~dir:(Path.of_string (Filename.get_temp_dir_name ()))
         ~f:(function
           | Error exn -> raise exn
           | Ok path ->
             let+ (), exit_code =
               Process.run
                 ~dir:(Path.build dir)
                 ~display:Quiet
                 ~env:
                   (Env.add env ~var:(Env.Var.of_string "OCAMLRUNPARAM") ~value:"Xhelp")
                 ~stdout_to:(Process.Io.null Out)
                 ~stderr_to:(Process.Io.file path Out)
                 Return
                 bin
                 [ "--version" ]
             in
             exit_code = 0
             && List.exists
                  (Io.lines_of_file path)
                  ~f:(String.starts_with ~prefix:"small_heap_limit "))
  in
  let memo =
    Memo.create "melange-small-heap-limit" ~input:(module Input) ~cutoff:Bool.equal impl
  in
  fun bin ~dir ~env -> Memo.exec memo (bin, dir, env)
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
      match Env.get env (Env.Var.of_string "MELANGELIB") with
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

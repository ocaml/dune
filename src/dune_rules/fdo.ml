open Import

type phase =
  | All
  | Compile
  | Emit

let linear_ext = ".cmir-linear"
let linear_fdo_ext = linear_ext ^ "-fdo"
let fdo_profile s = Path.extend_basename s ~suffix:".fdo-profile"
let linker_script s = Path.extend_basename s ~suffix:".linker-script"

let phase_flags = function
  | None -> []
  | Some All -> [ "-g"; "-function-sections" ]
  | Some Compile -> [ "-g"; "-stop-after"; "scheduling"; "-save-ir-after"; "scheduling" ]
  | Some Emit -> [ "-g"; "-start-from"; "emit"; "-function-sections" ]
;;

(* CR-soon gyorsh: add a rule to use profile with c/cxx profile if available,
   similarly to opt_rule for ocaml modules. The profile will have to be
   generated externally from perf data for example using google's autofdo
   toolset: create_gcov for gcc or create_llvm_prof for llvm. *)
let c_flags (ctx : Context.t) =
  match Context.fdo_target_exe ctx with
  | None -> []
  | Some _ -> [ "-ffunction-sections" ]
;;

let cxx_flags = c_flags

(* Location of ocamlfdo binary tool is independent of the module, but may depend
   on the context. *)
let ocamlfdo_binary sctx dir =
  Super_context.resolve_program
    sctx
    ~dir
    ~where:Original_path
    ~loc:None
    "ocamlfdo"
    ~hint:"opam install ocamlfdo"
;;

(* FDO flags are context dependent. *)
let get_flags var =
  let f (ctx : Context.t) =
    let open Memo.O in
    let+ env = Context.installed_env ctx in
    Env.get env var |> Option.value ~default:"" |> String.extract_blank_separated_words
  in
  let memo =
    Memo.create var ~input:(module Context) ~cutoff:(List.equal String.equal) f
  in
  Memo.exec memo
;;

let ocamlfdo_flags = get_flags "OCAMLFDO_FLAGS"

module Mode = struct
  type t =
    | If_exists
    | Always
    | Never

  let to_string = function
    | If_exists -> "if-exists"
    | Always -> "always"
    | Never -> "never"
  ;;

  let default = If_exists
  let all = [ If_exists; Never; Always ]
  let var = "OCAMLFDO_USE_PROFILE"

  let of_env (env : Env.t) =
    match Env.get env var with
    | None -> default
    | Some v ->
      (match List.find_opt ~f:(fun s -> String.equal v (to_string s)) all with
       | Some v -> v
       | None ->
         User_error.raise
           [ Pp.textf
               "Failed to parse environment variable: %s=%s\n\
                Permitted values: if-exists always never\n\
                Default: %s"
               var
               v
               (to_string default)
           ])
  ;;
end

let get_profile (ctx : Context.t) =
  let open Action_builder.O in
  let path = Context.fdo_target_exe ctx |> Option.value_exn |> fdo_profile in
  let some () =
    let+ () = Action_builder.dep (Dep.file path) in
    Some path
  in
  let none () = Action_builder.return None in
  Context.installed_env ctx
  |> Action_builder.of_memo
  >>| Mode.of_env
  >>= function
  | Never -> none ()
  | Always -> some ()
  | If_exists -> Action_builder.if_file_exists path ~then_:(some ()) ~else_:(none ())
;;

let opt_rule cctx m =
  let sctx = Compilation_context.super_context cctx in
  let ctx = Compilation_context.context cctx in
  let dir = Compilation_context.dir cctx in
  let obj_dir = Compilation_context.obj_dir cctx in
  let linear = Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml Cmx) ~ext:linear_ext in
  let linear_fdo =
    Obj_dir.Module.obj_file obj_dir m ~kind:(Ocaml Cmx) ~ext:linear_fdo_ext
  in
  let flags =
    let open Action_builder.O in
    let+ profile = get_profile ctx in
    match profile with
    | None -> Command.Args.As [ "-md5-unit"; "-extra-debug"; "-q" ]
    | Some fdo_profile_path ->
      Command.Args.S
        [ A "-fdo-profile"
        ; Dep fdo_profile_path
        ; As [ "-md5-unit"; "-reorder-blocks"; "opt"; "-q" ]
        ]
  in
  let ocamlfdo_binary = ocamlfdo_binary sctx dir in
  let ocamlfdo_flags = ocamlfdo_flags ctx |> Action_builder.of_memo in
  Super_context.add_rule
    sctx
    ~dir
    (Command.run_dyn_prog
       ~dir:(Path.build dir)
       ocamlfdo_binary
       [ A "opt"
       ; Hidden_targets [ linear_fdo ]
       ; Dep (Path.build linear)
       ; Command.Args.dyn ocamlfdo_flags
       ; Dyn flags
       ])
;;

module Linker_script = struct
  type t = Path.t Memo.t option

  let ocamlfdo_linker_script_flags = get_flags "OCAMLFDO_LINKER_SCRIPT_FLAGS"

  let linker_script_rule cctx fdo_target_exe =
    let sctx = Compilation_context.super_context cctx in
    let ctx = Compilation_context.context cctx in
    let dir = Compilation_context.dir cctx in
    let linker_script = linker_script fdo_target_exe in
    let linker_script_path =
      Path.Build.(relative (Context.build_dir ctx) (Path.to_string linker_script))
    in
    let flags =
      let open Action_builder.O in
      let+ get_profile = get_profile ctx in
      match get_profile with
      | Some fdo_profile_path -> Command.Args.S [ A "-fdo-profile"; Dep fdo_profile_path ]
      | None -> As []
    in
    let open Memo.O in
    let ocamlfdo_binary = ocamlfdo_binary sctx dir in
    let ocamlfdo_linker_script_flags =
      Action_builder.of_memo @@ ocamlfdo_linker_script_flags ctx
    in
    let+ () =
      Super_context.add_rule
        sctx
        ~dir
        (Command.run_dyn_prog
           ~dir:(Path.build (Context.build_dir ctx))
           ocamlfdo_binary
           [ A "linker-script"
           ; A "-o"
           ; Target linker_script_path
           ; Dyn flags
           ; A "-q"
           ; Command.Args.dyn ocamlfdo_linker_script_flags
           ])
    in
    linker_script
  ;;

  let create cctx name =
    let ctx = Compilation_context.context cctx in
    match Context.fdo_target_exe ctx with
    | None -> None
    | Some fdo_target_exe ->
      if let ocaml = Compilation_context.ocaml cctx in
         Path.equal name fdo_target_exe
         && (Ocaml.Version.supports_function_sections ocaml.version
             || Ocaml_config.is_dev_version ocaml.ocaml_config)
      then Some (linker_script_rule cctx fdo_target_exe)
      else None
  ;;

  let flags t =
    let open Memo.O in
    let open Command.Args in
    match t with
    | None -> Memo.return (As [])
    | Some linker_script ->
      let+ linker_script = linker_script in
      S [ A "-ccopt"; Concat ("", [ A "-Xlinker --script="; Dep linker_script ]) ]
  ;;
end

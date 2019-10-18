open! Stdune
module CC = Compilation_context

type phase =
  | All
  | Compile
  | Emit

let linear_ext = ".cmir-linear"

let linear_fdo_ext = linear_ext ^ "-fdo"

let fdo_profile_filename s = s ^ ".fdo-profile"

let linker_script_filename s = s ^ ".linker-script"

let linker_script_hot_filename s = s ^ ".linker-script-hot"

let perf_data_filename s = s ^ ".perf.data"

let phase_flags = function
  | None -> []
  | Some All -> [ "-g"; "-function-sections" ]
  | Some Compile ->
    [ "-g"; "-stop-after"; "scheduling"; "-save-ir-after"; "scheduling" ]
  | Some Emit -> [ "-g"; "-start-from"; "emit"; "-function-sections" ]

(* Location of ocamlfdo binary tool is independent of the module, but may
   depend on the context. If it isn't cached elsewhere, we should do it here.
   CR gyorsh: is it cached? *)
let ocamlfdo_binary sctx dir =
  let ocamlfdo =
    Super_context.resolve_program sctx ~dir ~loc:None "ocamlfdo"
      ~hint:"try: opam install ocamlfdo"
  in
  match ocamlfdo with
  | Error e -> Action.Prog.Not_found.raise e
  | Ok _ -> ocamlfdo

(* CR gyorsh: this should also be cached *)
let fdo_use_profile (ctx : Context.t) name fdo_profile =
  let fdo_profile_src = Path.Source.(relative root fdo_profile) in
  let profile_exists = File_tree.file_exists fdo_profile_src in
  match Env.get ctx.env "OCAMLFDO_USE_PROFILE" with
  | None
  | Some "if-exists" ->
    profile_exists
  | Some "always" ->
    if profile_exists then
      true
    else
      User_error.raise
        [ Pp.textf
            "Cannot build %s: OCAMLFDO_USE_PROFILE=always but profile file %s \
             does not exist."
            name fdo_profile
        ]
  | Some "never" -> false
  | Some other ->
    User_error.raise
      [ Pp.textf
          "Failed to parse environment variable: OCAMLFDO_USE_PROFILE=%s\n\
           Permitted values: if-exists always never\n\
           Default: if-exists"
          other
      ]

let opt_rule cctx m fdo_target_exe =
  let sctx = CC.super_context cctx in
  let ctx = CC.context cctx in
  let dir = CC.dir cctx in
  let obj_dir = CC.obj_dir cctx in
  let linear = Obj_dir.Module.obj_file obj_dir m ~kind:Cmx ~ext:linear_ext in
  let linear_fdo =
    Obj_dir.Module.obj_file obj_dir m ~kind:Cmx ~ext:linear_fdo_ext
  in
  let fdo_profile = fdo_profile_filename fdo_target_exe in
  let name = Module_name.to_string (Module.name m) in
  let use_profile = fdo_use_profile ctx name fdo_profile in
  let flags =
    let open Command.Args in
    if use_profile then
      S
        [ A "-fdo-profile"
        ; Dep (Path.build (Path.Build.relative ctx.build_dir fdo_profile))
        ; As [ "-md5-unit"; "-reorder-blocks"; "opt"; "-q" ]
        ]
    else
      As [ "-md5-unit"; "-extra-debug"; "-q" ]
  in
  let ocamlfdo_flags =
    Env.get ctx.env "OCAMLFDO_FLAGS"
    |> Option.value ~default:"" |> String.extract_blank_separated_words
  in
  Super_context.add_rule sctx ~dir
    (Command.run ~dir:(Path.build dir) (ocamlfdo_binary sctx dir)
       [ A "opt"
       ; Hidden_targets [ linear_fdo ]
       ; Dep (Path.build linear)
       ; As ocamlfdo_flags
       ; flags
       ])

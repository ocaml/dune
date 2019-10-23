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

let phase_flags = function
  | None -> []
  | Some All -> [ "-g"; "-function-sections" ]
  | Some Compile ->
    [ "-g"; "-stop-after"; "scheduling"; "-save-ir-after"; "scheduling" ]
  | Some Emit -> [ "-g"; "-start-from"; "emit"; "-function-sections" ]

(* CR-soon gyorsh: add a rule to use profile with c/cxx profile if available,
   similarly to opt_rule for ocaml modules. The profile will have to be
   generated externally from perf data for example using google's autofdo
   toolset: create_gcov for gcc or create_llvm_prof for llvm. *)
let c_flags (ctx : Context.t) =
  match ctx.fdo_target_exe with
  | None -> []
  | Some _ -> [ "-ffunction-sections" ]

let cxx_flags = c_flags

(* Location of ocamlfdo binary tool is independent of the module, but may
   depend on the context. If it isn't cached elsewhere, we should do it here.
   CR gyorsh: is it cached? *)
let ocamlfdo_binary sctx dir =
  Super_context.resolve_program sctx ~dir ~loc:None "ocamlfdo"
    ~hint:"try: opam install ocamlfdo"

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

module Linker_script = struct
  type t = Path.t option

  let linker_script_rule cctx fdo_target_exe =
    let sctx = CC.super_context cctx in
    let ctx = CC.context cctx in
    let dir = CC.dir cctx in
    let linker_script_hot = linker_script_hot_filename fdo_target_exe in
    let fdo_profile = fdo_profile_filename fdo_target_exe in
    let linker_script = linker_script_filename fdo_target_exe in
    let linker_script_path =
      Path.Build.(relative ctx.build_dir linker_script)
    in
    let extra_flags =
      Env.get ctx.env "OCAMLFDO_LINKER_SCRIPT_FLAGS"
      |> Option.value ~default:"" |> String.extract_blank_separated_words
    in
    let use_profile = fdo_use_profile ctx linker_script fdo_profile in
    let flags =
      let open Command.Args in
      if use_profile then
        let fdo_profile_path =
          Path.build (Path.Build.relative ctx.build_dir fdo_profile)
        in
        S [ A "-fdo-profile"; Dep fdo_profile_path ]
      else if
        File_tree.file_exists Path.Source.(relative root linker_script_hot)
      then (
        let linker_script_hot_path =
          Path.build (Path.Build.relative ctx.build_dir linker_script_hot)
        in
        User_warning.emit
          ~hints:[ Pp.textf "To ignore %s, rename it." linker_script_hot ]
          [ Pp.textf
              "Linker script generation with ocamlfdo cannot get hot function \
               layout from profile, because either OCAMLFDO_USE_PROFILE=never \
               or %s not found. Hot functions layout from file %s will be \
               used."
              fdo_profile linker_script_hot
          ];
        S [ A "-linker-script-hot"; Dep linker_script_hot_path ]
      ) else
        As []
    in
    Super_context.add_rule sctx ~dir
      (Command.run ~dir:(Path.build ctx.build_dir) (ocamlfdo_binary sctx dir)
         [ A "linker-script"
         ; A "-o"
         ; Target linker_script_path
         ; flags
         ; A "-q"
         ; As extra_flags
         ]);
    Path.build linker_script_path

  let create cctx name =
    let ctx = CC.context cctx in
    match ctx.fdo_target_exe with
    | None -> None
    | Some fdo_target_exe ->
      if
        String.equal name fdo_target_exe
        && ( Ocaml_version.supports_function_sections ctx.version
           || Ocaml_config.is_dev_version ctx.ocaml_config )
      then
        Some (linker_script_rule cctx fdo_target_exe)
      else
        None

  let flags t =
    let open Command.Args in
    match t with
    | None -> As []
    | Some linker_script ->
      S
        [ A "-ccopt"
        ; Concat ("", [ A "-Xlinker --script="; Dep linker_script ])
        ]
end

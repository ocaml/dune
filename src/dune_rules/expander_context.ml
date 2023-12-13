open Import
open Memo.O

let make loc =
  Expander0.Expansion_result.Need_full_expander
    (fun t ->
      Without
        (let open Memo.O in
         let* context = Context.DB.get (Expander0.context t) in
         let+ make =
           let path = Context.path context in
           Make_prog.which loc (Context.name context) ~path
         in
         [ Value.Path make ]))
;;

let string s = [ Value.String s ]

let with_context f =
  Expander0.Expansion_result.Need_full_expander
    (fun t -> Without (f (Expander0.context t)))
;;

let profile =
  with_context (fun ctx ->
    Context.DB.get ctx >>| Context.profile >>| Profile.to_string >>| string)
;;

let context_name =
  with_context (fun ctx -> Context_name.to_string ctx |> string |> Memo.return)
;;

let workspace_root =
  with_context (fun ctx ->
    [ Value.Dir (Context_name.build_dir ctx |> Path.build) ] |> Memo.return)
;;

let lib_config (f : Lib_config.t -> Value.t) _loc =
  with_context (fun ctx ->
    let+ lib_config =
      let+ ocaml = Context.DB.get ctx >>= Context.ocaml in
      ocaml.lib_config
    in
    [ f lib_config ])
;;

let ocaml_config f =
  with_context (fun ctx ->
    let+ ocaml_config =
      let+ ocaml = Context.DB.get ctx >>= Context.ocaml in
      ocaml.ocaml_config
    in
    f ocaml_config)
;;

let c_compiler_and_flags ocaml_config =
  Ocaml_config.c_compiler ocaml_config :: Ocaml_config.ocamlc_cflags ocaml_config
;;

let toolchain loc =
  with_context (fun ctx ->
    let+ ctx = Context.DB.get ctx in
    match Context.findlib_toolchain ctx with
    | Some toolchain -> [ Value.String (Context_name.to_string toolchain) ]
    | None -> User_error.raise ~loc [ Pp.text "No toolchain defined for this context" ])
;;

let with_ocaml f _loc =
  with_context (fun ctx -> Context.DB.get ctx >>= Context.ocaml >>| f)
;;

let no_loc f _loc = f

let get_prog = function
  | Ok p -> [ Value.Path p ]
  | Error err -> Action.Prog.Not_found.raise err
;;

let ocaml_config_macro loc macro =
  ocaml_config (fun ocaml_config ->
    let s = Pform.Macro_invocation.Args.whole macro in
    match Ocaml_config.by_name ocaml_config s with
    | None ->
      User_error.raise ~loc [ Pp.textf "Unknown ocaml configuration variable %S" s ]
    | Some v ->
      (match v with
       | Bool x -> string (string_of_bool x)
       | Int x -> string (string_of_int x)
       | String x -> string x
       | Words x -> Value.L.strings x
       | Prog_and_args x -> Value.L.strings (x.prog :: x.args)))
;;

let relative loc d macro =
  let s = Pform.Macro_invocation.Args.whole macro in
  Path.build (Path.Build.relative ~error_loc:loc d s)
;;

let dep_form loc macro =
  Expander0.Expansion_result.Need_full_expander
    (fun expander ->
      With
        (let dir = Expander0.dir expander in
         relative loc dir macro |> Expander0.Deps.dep))
;;

let exe loc macro =
  Expander0.Expansion_result.Need_full_expander
    (fun expander ->
      With
        (let open Action_builder.O in
         let dir = Expander0.dir expander in
         let path =
           let s = Pform.Macro_invocation.Args.whole macro in
           Path.Build.relative ~error_loc:loc dir s
         in
         let dep p =
           let+ () = Action_builder.path p in
           [ Value.Path p ]
         in
         match Expander0.expanding_what expander with
         | Deps_like_field -> dep (Path.build path)
         | Nothing_special | User_action _ | User_action_without_targets _ ->
           let* context =
             Action_builder.of_memo @@ Context.DB.get (Expander0.context expander)
           in
           Path.build path |> Context.map_exe context |> dep))
;;

let forms =
  [ Pform.Macro.Ocaml_config, ocaml_config_macro; Exe, exe; Dep, dep_form ]
  |> Pform.Macro.Map.of_list_exn
;;

let vars =
  let ocaml_config f _loc = ocaml_config f in
  [ Pform.Var.Make, make
  ; Profile, no_loc profile
  ; Context_name, no_loc context_name
  ; Workspace_root, no_loc workspace_root
  ; Ext_lib, lib_config (fun l -> Value.String l.ext_lib)
  ; Ext_obj, lib_config (fun l -> Value.String l.ext_obj)
  ; Ext_dll, lib_config (fun l -> Value.String l.ext_dll)
  ; Ocaml_stdlib_dir, lib_config (fun l -> Value.Dir l.stdlib_dir)
  ; ( Ccomp_type
    , lib_config (fun l -> Value.String (Ocaml_config.Ccomp_type.to_string l.ccomp_type))
    )
  ; ( Cpp
    , ocaml_config (fun ocaml_config ->
        c_compiler_and_flags ocaml_config @ [ "-E" ] |> Value.L.strings) )
  ; ( Pa_cpp
    , ocaml_config (fun ocaml_config ->
        c_compiler_and_flags ocaml_config @ [ "-undef"; "-traditional"; "-x"; "c"; "-E" ]
        |> Value.L.strings) )
  ; ( Arch_sixtyfour
    , ocaml_config (fun ocaml_config ->
        64 = Ocaml_config.word_size ocaml_config |> string_of_bool |> string) )
  ; ( Ocaml_version
    , ocaml_config (fun ocaml_config ->
        Ocaml_config.version_string ocaml_config |> string) )
  ; ( Ext_asm
    , ocaml_config (fun ocaml_config -> Ocaml_config.ext_asm ocaml_config |> string) )
  ; ( Ext_plugin
    , ocaml_config (fun ocaml_config ->
        (if Ocaml_config.natdynlink_supported ocaml_config then Mode.Native else Byte)
        |> Mode.plugin_ext
        |> string) )
  ; ( Os_type
    , ocaml_config (fun ocaml_config ->
        Ocaml_config.os_type ocaml_config |> Ocaml_config.Os_type.to_string |> string) )
  ; ( Architecture
    , ocaml_config (fun ocaml_config -> Ocaml_config.architecture ocaml_config |> string)
    )
  ; System, ocaml_config (fun ocaml_config -> Ocaml_config.system ocaml_config |> string)
  ; Model, ocaml_config (fun ocaml_config -> Ocaml_config.model ocaml_config |> string)
  ; ( Ext_exe
    , ocaml_config (fun ocaml_config -> Ocaml_config.ext_exe ocaml_config |> string) )
  ; Toolchain, toolchain
  ; Ocaml, with_ocaml (fun ocaml -> get_prog ocaml.ocaml)
  ; Ocamlc, with_ocaml (fun ocaml -> [ Value.Path ocaml.ocamlc ])
  ; Ocamlopt, with_ocaml (fun ocaml -> get_prog ocaml.ocamlopt)
  ; Ocaml_bin_dir, with_ocaml (fun ocaml -> [ Value.Dir ocaml.bin_dir ])
  ]
  |> Pform.Var.Map.of_list_exn
;;

let () = Expander0.Source.make vars forms
let linkme = ()

open Import
open Jbuild_types

module BS = Build_system
module Build = BS.Build
open Build.O

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

let g () =
  if !Clflags.g then
    ["-g"]
  else
    []

let ocaml_compile_flags () =
  Arg_spec.As ("-w" :: !Clflags.warnings :: g ())

let g () = Arg_spec.As (g ())

let cm_files modules ~dir ~cm_kind =
  List.map modules ~f:(fun (m : Module.t) -> Module.cm_file m ~dir cm_kind)

let find_module ~dir modules name =
  String_map.find_exn name modules
    ~string_of_key:(sprintf "%S")
    ~desc:(fun _ ->
      sprintf "<module name to module info in %s>" (Path.to_string dir))

let find_deps ~dir dep_graph name =
  String_map.find_exn name dep_graph
    ~string_of_key:(sprintf "%S")
    ~desc:(fun _ -> sprintf "<dependency graph in %s>" (Path.to_string dir))

let modules_of_names ~dir ~modules names =
  List.map names ~f:(find_module ~dir modules)

let obj_name_of_basename fn =
  match String.index fn '.' with
  | None -> fn
  | Some i -> String.sub fn ~pos:0 ~len:i

module type Params = sig
  val context  : Context.t
  val stanzas  : (Path.t * Jbuild_types.Stanza.t list) list
  val packages : string list
end

module Gen(P : Params) = struct
  type dir =
    { src_dir : Path.t
    ; ctx_dir : Path.t
    ; stanzas : Stanza.t list
    }

  module P = struct
    include P

    let stanzas =
      List.map stanzas
        ~f:(fun (dir, stanzas) ->
          { src_dir = dir
          ; ctx_dir = Path.append context.build_dir dir
          ; stanzas
          })
  end

  let ctx = P.context

  include (
  struct
    let findlib = Findlib.create ctx

    module Lib_db = struct
      open Lib_db

      let t = create findlib (List.map P.stanzas ~f:(fun d -> (d.ctx_dir, d.stanzas)))

      let find name = find t name

      let top_closure names =
        let v = lazy (Lib_db.top_closure t names) in
        Build.record_lib_deps names
        >>>
        (Build.arr (fun () -> Lazy.force v))

      let top_closure_dyn =
        Build.arr (Lib_db.top_closure t)
    end

    module Named_artifacts = struct
      open Named_artifacts

      let t = create findlib (List.map P.stanzas ~f:(fun d -> (d.ctx_dir, d.stanzas)))

      let binary name = Build.arr (fun _  -> binary t name)
      let in_findlib name =
        let pkg =
          match String.lsplit2 name ~on:':' with
          | None -> invalid_arg "Named_artifacts.in_findlib"
          | Some (pkg, _) -> pkg
        in
        Build.record_lib_deps [pkg]
        >>>
        (Build.arr (fun () -> in_findlib t name))
    end
  end : sig
    module Lib_db : sig
      val find            : string -> Lib.t
      val top_closure     : string list -> (unit, Lib.t list) Build.t
      val top_closure_dyn : (string list, Lib.t list) Build.t
    end

    module Named_artifacts : sig
      [@@@warning "-32"]
      val binary     : string -> (unit, Path.t) Build.t
      val in_findlib : string -> (unit, Path.t) Build.t
    end
  end)

  module Build = struct
    include Build

    [@@@warning "-32"]

    let run ?(dir=ctx.build_dir) ?stdout_to ?(env=ctx.env) ?extra_targets prog args =
      Build.run ~dir ?stdout_to ~env ?extra_targets prog args

    let run_capture ?(dir=ctx.build_dir) ?(env=ctx.env) prog args =
      Build.run_capture ~dir ~env prog args

    let run_capture_lines ?(dir=ctx.build_dir) ?(env=ctx.env) prog args =
      Build.run_capture_lines ~dir ~env prog args

    let bash ?dir ?stdout_to ?env ?extra_targets cmd =
      run (Dep (Path.absolute "/bin/bash")) ?dir ?stdout_to ?env ?extra_targets
        [ As ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd] ]
  end

  (* +-----------------------------------------------------------------+
     | Tools                                                           |
     +-----------------------------------------------------------------+ *)

  let ppx_metaquot = Named_artifacts.in_findlib "ppx_tools:ppx_metaquot"
  let ppx_rewriter = Named_artifacts.in_findlib "ppx_tools:rewriter"

  (* +-----------------------------------------------------------------+
     | User variables                                                  |
     +-----------------------------------------------------------------+ *)

  (* Expand some $-vars within action strings of rules defined in jbuild files *)
  let dollar_var_map =
    let ocamlopt =
      match ctx.ocamlopt with
      | None -> Path.relative ctx.ocaml_bin "ocamlopt"
      | Some p -> p
    in
    [ "-verbose"       , "" (*"-verbose";*)
    ; "CPP"            , ctx.bytecomp_c_compiler ^ " -E"
    ; "PA_CPP"         , ctx.bytecomp_c_compiler ^ " -undef -traditional -x c -E"
    ; "CC"             , ctx.bytecomp_c_compiler
    ; "CXX"            , ctx.bytecomp_c_compiler
    ; "ocaml_bin"      , Path.to_string ctx.ocaml_bin
    ; "OCAML"          , Path.to_string ctx.ocaml
    ; "OCAMLC"         , Path.to_string ctx.ocamlc
    ; "OCAMLOPT"       , Path.to_string ocamlopt
    ; "ocaml_version"  , ctx.version
    ; "ocaml_where"    , Path.to_string ctx.stdlib_dir
    ; "ARCH_SIXTYFOUR" , string_of_bool ctx.arch_sixtyfour
    ; "PORTABLE_INT63" , "true"
    ] |> String_map.of_alist
    |> function
    | Ok x -> x
    | Error _ -> assert false

  let root_var_lookup ~dir var_name =
    match var_name with
    | "ROOT" -> Some (Path.reach ~from:dir Path.root)
    | _ -> String_map.find var_name dollar_var_map

  let expand_vars ~dir s =
    String_with_vars.expand s ~f:(root_var_lookup ~dir)

  (* +-----------------------------------------------------------------+
     | User deps                                                       |
     +-----------------------------------------------------------------+ *)

  module Dep_conf_interpret = struct
    include Dep_conf

    type res =
      | Path of Path.t
      | Err of string

    let to_path ~dir = function
      | File  s -> Path (Path.relative dir (expand_vars ~dir s))
      | Alias s -> Path (Alias.file (Alias.make ~dir (expand_vars ~dir s)))
      | Glob_files _ ->
        Err "glob_files not yet implemented"
      | Files_recursively_in _ ->
        Err "files_recursively_in not yet implemented"

    let _dep ~dir t =
      match to_path ~dir t with
      | Path p -> Build.path p
      | Err msg -> Build.arr (fun _ -> die "%s" msg)

    let dep_of_list ~dir ts =
      let rec loop acc = function
        | [] -> Build.path_set acc
        | t :: ts ->
          match to_path ~dir t with
          | Path p -> loop (Path.Set.add p acc) ts
          | Err msg -> Build.arr (fun _ -> die "%s" msg)
      in
      loop Path.Set.empty ts

    let only_plain_file ~dir = function
      | File s -> Some (expand_vars ~dir s)
      | Alias _ -> None
      | Glob_files _ -> None
      | Files_recursively_in _ -> None
  end

  (* +-----------------------------------------------------------------+
     | ocamldep stuff                                                  |
     +-----------------------------------------------------------------+ *)

  let parse_deps ~dir lines ~modules ~alias_module =
    List.map lines ~f:(fun line ->
      match String.index line ':' with
      | None -> die "`ocamldep` in %s returned invalid line: %S" (Path.to_string dir) line
      | Some i ->
        let unit =
          let basename =
            String.sub line ~pos:0 ~len:i
            |> Filename.basename
          in
          let module_basename =
            match String.index basename '.' with
            | None -> basename
            | Some i -> String.sub basename ~pos:0 ~len:i
          in
          String.capitalize_ascii module_basename
        in
        let deps =
          String.split_words (String.sub line ~pos:(i + 1)
                                ~len:(String.length line - (i + 1)))
          |> List.filter ~f:(fun m -> m <> unit && String_map.mem m modules)
        in
        let deps =
          match alias_module with
          | None -> deps
          | Some (m : Module.t) -> m.name :: deps
        in
        (unit, deps))
    |> String_map.of_alist
    |> function
    | Ok x -> begin
        match alias_module with
        | None -> x
        | Some m -> String_map.add x ~key:m.name ~data:[]
      end
    | Error (unit, _, _) ->
      die
        "`ocamldep` in %s returned %s several times" (Path.to_string dir) unit

  module Ocamldep_vfile =
    Vfile_kind.Make
      (struct type t = string list String_map.t end)
      (functor (C : Sexp.Combinators) -> struct
        open C
        let t = string_map (list string)
      end)

  let ocamldep_rules ~ml_kind ~dir ~item ~modules ~alias_module =
    let suffix = Ml_kind.suffix ml_kind in
    let vdepends =
      let fn = Path.relative dir (sprintf "%s.depends%s.sexp" item suffix) in
      BS.Vspec.T (fn, (module Ocamldep_vfile))
    in
    let files =
      List.filter_map (String_map.values modules) ~f:(fun m -> Module.file ~dir m ml_kind)
      |> List.map ~f:(fun fn ->
        match ml_kind, Filename.ext (Path.to_string fn) with
        | Impl, Some ".ml"  -> Arg_spec.Dep fn
        | Intf, Some ".mli" -> Dep fn
        | Impl, _ -> S [A "-impl"; Dep fn]
        | Intf, _ -> S [A "-intf"; Dep fn])
    in
    BS.rule
      (Build.run_capture_lines (Dep ctx.ocamldep) [A "-modules"; S files]
       >>^ parse_deps ~dir ~modules ~alias_module
       >>> Build.store_vfile vdepends);
    Build.vpath vdepends

  module Dep_closure =
    Top_closure.Make(String)(struct
      type t = string
      type graph = Path.t * t list String_map.t
      let key t = t
      let deps t (dir, map) = find_deps ~dir map t
    end)

  let dep_closure ~dir dep_graph names =
    match Dep_closure.top_closure (dir, dep_graph) names with
    | Ok names -> names
    | Error cycle ->
      die "dependency cycle between modules in %s:\n   %s" (Path.to_string dir)
        (String.concat cycle ~sep:"\n-> ")

  let names_to_top_closed_cm_files ~dir ~dep_graph ~modules ~mode names =
    dep_closure ~dir dep_graph names
    |> modules_of_names ~dir ~modules
    |> cm_files ~dir ~cm_kind:(Mode.cm_kind mode)

  (* +-----------------------------------------------------------------+
     | Preprocessing stuff                                             |
     +-----------------------------------------------------------------+ *)

  let ocamldep_rules ~dir ~item ~modules ~alias_module =
    Ml_kind.Dict.of_func (ocamldep_rules ~dir ~item ~modules ~alias_module)

  let pp_fname fn =
    match Filename.split_ext fn with
    | None -> fn ^ ".pp"
    | Some (fn, ext) ->
      (* We need to to put the .pp before the .ml so that the compiler realises that
         [foo.pp.mli] is the interface for [foo.pp.ml] *)
      fn ^ ".pp" ^ ext

  let pped_module ~dir (m : Module.t) ~f =
    let ml_pp_fname = pp_fname m.ml_fname in
    f Ml_kind.Impl (Path.relative dir m.ml_fname) (Path.relative dir ml_pp_fname);
    let mli_pp_fname =
      Option.map m.mli_fname ~f:(fun fname ->
        let pp_fname = pp_fname fname in
        f Intf (Path.relative dir fname) (Path.relative dir pp_fname);
        pp_fname)
    in
    { m with
      ml_fname  = ml_pp_fname
    ; mli_fname = mli_pp_fname
    }

  let ppx_drivers = Hashtbl.create 32

  let get_ppx_driver pps =
    let names =
      Pp_set.elements pps
      |> List.map ~f:Pp.to_string
    in
    let key = String.concat names ~sep:"+" in
    match Hashtbl.find ppx_drivers key with
    | Some x -> x
    | None ->
      let mode = Mode.best ctx in
      let compiler = Option.value_exn (Mode.compiler mode ctx) in
      let libs =
        Lib_db.top_closure ("ppx_driver" :: names)
        >>>
        Build.arr (fun libs ->
          List.filter libs ~f:(fun lib ->
            Lib.best_name lib <> "ppx_driver.runner")
          @ [Lib_db.find "ppx_driver.runner"]
        )
      in
      let ppx_dir = Path.relative ctx.build_dir (sprintf ".ppx/%s" key) in
      let exe = Path.relative ppx_dir "ppx.exe" in
      BS.rule
        (libs
         >>>
         Build.run (Dep compiler)
           [ A "-linkall"
           ; A "-o"; Target exe
           ; Dyn (Lib.link_flags ~mode)
           ]);
      Hashtbl.add ppx_drivers ~key ~data:(exe, libs);
      (exe, libs)

  let specific_args_for_ppx_rewriters ~dir (libs : Lib.t list) =
    let uses_inline_test = ref false in
    let uses_inline_bench = ref false in
    let uses_here = ref false in
    List.iter libs ~f:(fun lib ->
      match Lib.best_name lib with
      | "ppx_here" | "ppx_assert" -> uses_here := true
      | "ppx_expect" -> uses_inline_test := true; uses_here := true
      | "ppx_inline_test" -> uses_inline_test := true
      | "ppx_bench" -> uses_inline_bench := true
      | _ -> ());
    Arg_spec.S
      [ S (if !uses_here
           then [A "-dirname"; Path dir]
           else [])
      ; S (if !uses_inline_test(* && drop_test*)
           then [ A "-inline-test-drop-with-deadcode" ]
           else [])
      ; S (if !uses_inline_bench (*&& drop_bench*)
           then [ A "-bench-drop-with-deadcode" ]
           else [])
      ]

  (* Generate rules to build the .pp files and return a new module map where all filenames
     point to the .pp files *)
  let pped_modules ~dir ~modules ~preprocess ~preprocessor_deps =
    let preprocessor_deps = Dep_conf_interpret.dep_of_list ~dir preprocessor_deps in
    String_map.map modules ~f:(fun (m : Module.t) ->
      match Preprocess_map.find m.name preprocess with
      | No_preprocessing -> m
      | Metaquot ->
        pped_module m ~dir ~f:(fun kind src dst ->
          BS.rule
            (preprocessor_deps
             >>>
             Build.fanout ppx_rewriter ppx_metaquot
             >>>
             Build.run (Dyn fst)
               [ Dyn (fun (_, ppx_metaquot) -> Dep ppx_metaquot)
               ; A "-o"; Target dst
               ; Ml_kind.flag kind; Dep src
               ]))
      | Command cmd ->
        pped_module m ~dir ~f:(fun _kind src dst ->
          BS.rule
            (preprocessor_deps
             >>>
             Build.path src
             >>>
             Build.bash ~stdout_to:dst ~dir
               (sprintf "%s %s" (expand_vars ~dir cmd)
                  (Filename.quote (Path.reach src ~from:dir)))))
      | Pps { pps; flags } ->
        let ppx_exe, libs = get_ppx_driver pps in
        pped_module m ~dir ~f:(fun kind src dst ->
          BS.rule
            (preprocessor_deps
             >>>
             libs
             >>>
             Build.run
               (Dep ppx_exe)
               [ Dyn (specific_args_for_ppx_rewriters ~dir)
               ; As flags
               ; A "-o"; Target dst
               ; Ml_kind.flag kind; Dep src
               ])
        )
    )

  module Libs_vfile =
    Vfile_kind.Make_full
      (struct type t = Lib.t list end)
      (struct
        open Sexp.To_sexp
        let t l = list string (List.map l ~f:Lib.best_name)
      end)
      (struct
        open Sexp.Of_sexp
        let t sexp = List.map (list string sexp) ~f:Lib_db.find
      end)

  let requires_including_runtime_deps ~dir ~item ~libraries ~ppx_runtime_libraries
        ~preprocess =
    let all_pps =
      Preprocess_map.pps preprocess
      |> Pp_set.elements
      |> List.map ~f:Pp.to_string
    in
    let vrequires =
      let fn = Path.relative dir (item ^ ".requires.sexp") in
      BS.Vspec.T (fn, (module Libs_vfile))
    in
    BS.rule
      (Build.record_lib_deps (libraries @ ppx_runtime_libraries)
       >>>
       Lib_db.top_closure all_pps
       >>>
       Build.arr (fun pps_libs ->
         String_set.elements
           (String_set.union
              (String_set.of_list libraries)
              (Lib.ppx_runtime_libraries pps_libs)))
       >>>
       Lib_db.top_closure_dyn
       >>>
       Build.store_vfile vrequires);
    Build.vpath vrequires

  (* +-----------------------------------------------------------------+
     | Ordered set lang evaluation                                     |
     +-----------------------------------------------------------------+ *)

  let expand_and_eval_set ~dir set ~standard =
    match Ordered_set_lang.Unexpanded.files set |> String_set.elements with
    | [] ->
      let set = Ordered_set_lang.Unexpanded.expand set ~files_contents:String_map.empty in
      Build.return (Ordered_set_lang.eval_with_standard set ~standard)
    | files ->
      let paths = List.map files ~f:(Path.relative dir) in
      Build.paths paths
      >>>
      Build.arr (fun () ->
        let files_contents =
          List.map2 files paths ~f:(fun fn path ->
            (fn, Sexp_load.single (Path.to_string path) (fun x -> x)))
          |> String_map.of_alist_exn
        in
        let set = Ordered_set_lang.Unexpanded.expand set ~files_contents in
        Ordered_set_lang.eval_with_standard set ~standard)

  (* +-----------------------------------------------------------------+
     | ml/mli compilation                                              |
     +-----------------------------------------------------------------+ *)

  let lib_cm_all ~dir (lib : Library.t) cm_kind =
    Path.relative dir
      (sprintf "%s%s-all" lib.name (Cm_kind.ext cm_kind))

  let lib_dependencies (libs : Lib.t list) ~(cm_kind : Cm_kind.t) =
    List.concat_map libs ~f:(function
      | External _ -> []
      | Internal (dir, lib) ->
        match cm_kind with
        | Cmi | Cmo ->
          [lib_cm_all ~dir lib Cmi]
        | Cmx ->
          [lib_cm_all ~dir lib Cmx])

  let build_cm ?(flags=Arg_spec.S[]) ~cm_kind ~dep_graph ~requires
        ~(modules : Module.t String_map.t) ~dir ~alias_module (m : Module.t) =
    Option.iter (Cm_kind.compiler cm_kind ctx) ~f:(fun compiler ->
      Option.iter (Module.cm_source ~dir m cm_kind) ~f:(fun src ->
        let ml_kind = Cm_kind.source cm_kind in
        let dst = Module.cm_file m ~dir cm_kind in
        let extra_args, extra_deps, extra_targets =
          match cm_kind, m.mli_fname with
          (* If there is no mli, [ocamlY -c file.ml] produces both the
             .cmY and .cmi. We choose to use ocamlc to produce the cmi
             and to produce the cmx we have to wait to avoid race
             conditions. *)
          | Cmo, None -> [], [], [Module.cm_file m ~dir Cmi]
          | Cmx, None ->
            (* Change [-intf-suffix] so that the compiler thinks the
               cmi exists and reads it instead of re-creating it, which
               could create a race condition. *)
            ([ "-intf-suffix"
             ; match Filename.ext m.ml_fname with
             | None -> ""
             | Some ext -> ext
             ],
             [Module.cm_file m ~dir Cmi], [])
          | Cmi, None -> assert false
          | Cmi, Some _ -> [], [], []
          (* We need the .cmi to build either the .cmo or .cmx *)
          | (Cmo | Cmx), Some _ -> [], [Module.cm_file m ~dir Cmi], []
        in
        let dep_graph = Ml_kind.Dict.get dep_graph ml_kind in
        let other_cm_files =
          Build.dyn_paths
            (dep_graph >>^ (fun dep_graph ->
               let deps =
                 List.map (find_deps ~dir dep_graph m.name) ~f:(find_module ~dir modules)
               in
               List.concat_map
                 deps
                 ~f:(fun m ->
                   match cm_kind with
                   | Cmi | Cmo -> [Module.cm_file m ~dir Cmi]
                   | Cmx -> [Module.cm_file m ~dir Cmi; Module.cm_file m ~dir Cmx])))
        in
        let extra_targets, cmt_args =
          match cm_kind with
          | Cmx -> (extra_targets, Arg_spec.S [])
          | Cmi | Cmo ->
            let fn = Option.value_exn (Module.cmt_file m ~dir ml_kind) in
            (fn :: extra_targets, A "-bin-annot")
        in
        BS.rule
          (Build.paths extra_deps >>>
           other_cm_files >>>
           requires >>>
           Build.dyn_paths (Build.arr (lib_dependencies ~cm_kind)) >>>
           Build.run (Dep compiler)
             ~extra_targets
             [ ocaml_compile_flags ()
             ; cmt_args
             ; Dyn Lib.include_flags
             ; flags
             ; As extra_args
             ; A "-I"; Path dir
             ; (match alias_module with
                | None -> S []
                | Some (m : Module.t) -> As ["-open"; m.name])
             ; A "-o"; Target dst
             ; A "-c"; Ml_kind.flag ml_kind; Dep src
             ])))

  let build_module ?flags m ~dir ~dep_graph ~modules ~requires ~alias_module =
    List.iter Cm_kind.all ~f:(fun cm_kind ->
      build_cm ?flags ~dir ~dep_graph ~modules m ~cm_kind ~requires ~alias_module)

  let build_modules ~dir ~dep_graph ~modules ~requires ~alias_module =
    String_map.iter
      (match alias_module with
       | None -> modules
       | Some (m : Module.t) -> String_map.remove m.name modules)
      ~f:(fun ~key:_ ~data:m ->
        build_module m ~dir ~dep_graph ~modules ~requires ~alias_module)

  (* +-----------------------------------------------------------------+
     | Interpretation of [modules] fields                              |
     +-----------------------------------------------------------------+ *)

  let parse_modules ~dir ~all_modules ~modules_written_by_user =
    if Ordered_set_lang.is_standard modules_written_by_user then
      all_modules
    else begin
      let units =
        Ordered_set_lang.eval_with_standard
          modules_written_by_user
          ~standard:(String_map.keys all_modules)
      in
      List.iter units ~f:(fun unit ->
        if not (String_map.mem unit all_modules) then
          die "no implementation for module %s in %s"
            unit (Path.to_string dir));
      let units = String_set.of_list units in
      String_map.filter all_modules ~f:(fun unit _ -> String_set.mem unit units)
    end

  (* +-----------------------------------------------------------------+
     | Library stuff                                                   |
     +-----------------------------------------------------------------+ *)

  let lib_archive (lib : Library.t) ~dir ~ext = Path.relative dir (lib.name ^ ext)

  let stubs_archive (lib : Library.t) ~dir =
    Path.relative dir (sprintf "lib%s_stubs%s" lib.name ctx.ext_lib)

  let dll (lib : Library.t) ~dir =
    Path.relative dir (sprintf "dll%s_stubs%s" lib.name ctx.ext_dll)

  let build_lib (lib : Library.t) ~dir ~mode ~modules ~dep_graph =
    Option.iter (Mode.compiler mode ctx) ~f:(fun compiler ->
      let target = lib_archive lib ~dir ~ext:(Mode.compiled_lib_ext mode) in
      let dep_graph = Ml_kind.Dict.get dep_graph Impl in
      let stubs_flags =
        match lib.c_names with
        | [] -> []
        | _  ->
          let stubs_name = lib.name ^ "_stubs" in
          match mode with
          | Byte -> ["-dllib"; stubs_name; "-cclib"; stubs_name]
          | Native -> ["-cclib"; stubs_name]
      in
      BS.rule
        (Build.fanout3
           (dep_graph >>>
            Build.arr (fun dep_graph ->
              names_to_top_closed_cm_files
                ~dir
                ~dep_graph
                ~modules
                ~mode
                (String_map.keys modules)))
           (expand_and_eval_set ~dir lib.library_flags ~standard:[])
           (expand_and_eval_set ~dir lib.cclibs        ~standard:[])
         >>>
         Build.run (Dep compiler)
           ~extra_targets:(
             match mode with
             | Byte -> []
             | Native -> [lib_archive lib ~dir ~ext:ctx.ext_lib])
           [ g ()
           ; A "-a"; A "-o"; Target target
           ; As stubs_flags
           ; Dyn (fun (_, libflags, cclibs) ->
               S [ As libflags
                 ; S (List.map cclibs ~f:(fun flag ->
                     Arg_spec.S [A "-cclib"; A flag]))])
           ; Dyn (fun (cm_files, _, _) -> Deps cm_files)
           ]))

  let mk_lib_cm_all (lib : Library.t) ~dir ~modules cm_kind =
    let deps = cm_files ~dir (String_map.values modules) ~cm_kind in
    BS.rule (Build.paths deps >>>
            Build.return "" >>>
            Build.echo (lib_cm_all lib ~dir cm_kind))

  let build_c_file (lib : Library.t) ~dir c_name =
    let src = Path.relative dir (c_name ^ ".c") in
    let dst = Path.relative dir (c_name ^ ctx.ext_obj) in
    BS.rule
      (expand_and_eval_set ~dir lib.c_flags ~standard:[]
       >>>
       Build.run
         (* We have to execute the rule in the library directory as the .o is produced in
            the current directory *)
         ~dir
         (Dep ctx.ocamlc)
         [ g ()
         ; Dyn (fun c_flags ->
             As (List.concat_map c_flags ~f:(fun f -> ["-ccopt"; f])))
         ; A "-o"; Target dst
         ; Dep src
         ]);
    dst

  (* Hack for the install file *)
  let modules_by_lib : (string, Module.t list) Hashtbl.t = Hashtbl.create 32

  let library_rules (lib : Library.t) ~dir ~all_modules ~files =
    let modules = parse_modules ~dir ~all_modules ~modules_written_by_user:lib.modules in
    let main_module_name = String.capitalize_ascii lib.name in
    let modules =
      String_map.map modules ~f:(fun (m : Module.t) ->
        if m.name = main_module_name then
          { m with obj_name = obj_name_of_basename m.ml_fname }
        else
          { m with obj_name = sprintf "%s__%s" lib.name m.name })
    in
    let alias_module =
      if String_map.cardinal modules = 1 &&
         String_map.mem main_module_name modules then
        None
      else
        let suf =
          if String_map.mem main_module_name modules then
            "__"
          else
            ""
        in
        Some
          { Module.
            name      = main_module_name ^ suf
          ; ml_fname  = lib.name ^ suf ^ ".ml-gen"
          ; mli_fname = None
          ; obj_name  = lib.name ^ suf
          }
    in
    (* Preprocess before adding the alias module as it doesn't need preprocessing *)
    let modules =
      pped_modules ~dir ~modules ~preprocess:lib.preprocess
        ~preprocessor_deps:lib.preprocessor_deps
    in
    let modules =
      match alias_module with
      | None -> modules
      | Some m -> String_map.add modules ~key:m.name ~data:m
    in
    Hashtbl.add modules_by_lib
      ~key:lib.name
      ~data:(String_map.values modules);

    let dep_graph = ocamldep_rules ~dir ~item:lib.name ~modules ~alias_module in

    Option.iter alias_module ~f:(fun m ->
      BS.rule
        (Build.return
           (String_map.values (String_map.remove m.name modules)
            |> List.map ~f:(fun (m : Module.t) ->
              sprintf "module %s = %s\n" m.name (Module.real_unit_name m))
            |> String.concat ~sep:"")
         >>> Build.echo (Path.relative dir m.ml_fname)));

    let requires =
      requires_including_runtime_deps ~dir ~item:lib.name
        ~libraries:lib.libraries
        ~preprocess:lib.preprocess
        ~ppx_runtime_libraries:lib.ppx_runtime_libraries
    in

    build_modules ~dir ~dep_graph ~modules ~requires ~alias_module;
    Option.iter alias_module ~f:(fun m ->
      build_module m
        ~flags:(As ["-no-alias-deps"; "-w"; "-49"])
        ~dir
        ~modules:(String_map.singleton m.name m)
        ~dep_graph:(Ml_kind.Dict.make_both (Build.return (String_map.singleton m.name [])))
        ~requires:(
          if String_map.is_empty modules then
            (* Just so that we setup lib dependencies for empty libraries *)
            requires
          else
            Build.return [])
        ~alias_module:None);

    if not (List.is_empty lib.c_names) then begin
      let h_files =
        String_set.elements files
        |> List.filter_map ~f:(fun fn ->
          if String.is_suffix fn ~suffix:".h" then
            Some (Path.relative dir fn)
          else
            None)
      in
      let o_files = List.map lib.c_names ~f:(build_c_file lib ~dir) in
      let targets = [ stubs_archive lib ~dir; dll lib ~dir ] in
      BS.rule
        (Build.paths h_files
         >>>
         Build.run
           ~extra_targets:targets
           (Dep ctx.ocamlmklib)
           [ g ()
           ; A "-o"
           ; Path (Path.relative dir (sprintf "%s_stubs" lib.name))
           ; Deps o_files
           ]);
    end;

    List.iter Cm_kind.all ~f:(mk_lib_cm_all lib ~dir ~modules);

    List.iter Mode.all ~f:(fun mode ->
      build_lib lib ~dir ~mode ~modules ~dep_graph);

    Option.iter ctx.ocamlopt ~f:(fun ocamlopt ->
      let src = lib_archive lib ~dir ~ext:(Mode.compiled_lib_ext Native) in
      let dst = lib_archive lib ~dir ~ext:".cmxs" in
      BS.rule
        (Build.run
           (Dep ocamlopt)
           [ g ()
           ; A "-shared"; A "-linkall"
           ; A "-I"; Path dir
           ; A "-o"; Target dst
           ; Dep src
           ])
    )

  (* +-----------------------------------------------------------------+
     | Executables stuff                                               |
     +-----------------------------------------------------------------+ *)

  let build_exe ~dir ~requires ~name ~mode ~modules ~dep_graph =
    Option.iter (Mode.compiler mode ctx) ~f:(fun compiler ->
      let dep_graph = Ml_kind.Dict.get dep_graph Impl in
      let exe = Path.relative dir (name ^ Mode.exe_ext mode) in
      BS.rule
        (Build.fanout
           (requires
            >>> Build.dyn_paths (Build.arr (Lib.archive_files ~mode)))
           (dep_graph
            >>> Build.arr (fun dep_graph ->
              names_to_top_closed_cm_files
                ~dir
                ~dep_graph
                ~modules
                ~mode
                [String.capitalize name]))
         >>>
         Build.run
           (Dep compiler)
           [ g ()
           ; A "-o"; Target exe
           ; Dyn (fun (libs, _) -> Lib.link_flags libs ~mode)
           ; Dyn (fun (_, cm_files) -> Deps cm_files)
           ]))

  let executables_rules (exes : Executables.t) ~dir ~all_modules =
    let modules = parse_modules ~dir ~all_modules ~modules_written_by_user:exes.modules in
    let modules =
      String_map.map modules ~f:(fun (m : Module.t) ->
        { m with obj_name = obj_name_of_basename m.ml_fname })
    in
(*    List.iter exes.names ~f:(fun name ->
      if not (String_map.mem (String.capitalize name) modules) then
        die "executable %s in %s doesn't have a corresponding .ml file"
          name (Path.to_string dir));
*)
    let modules =
      pped_modules ~dir ~modules ~preprocess:exes.preprocess
        ~preprocessor_deps:[]
    in
    let item = List.hd exes.names in
    let dep_graph = ocamldep_rules ~dir ~item ~modules ~alias_module:None in

    let requires =
      requires_including_runtime_deps ~dir ~item
        ~libraries:exes.libraries
        ~preprocess:exes.preprocess
        ~ppx_runtime_libraries:[]
    in

    build_modules ~dir ~dep_graph ~modules ~requires ~alias_module:None;

    if exes.link_executables then
      List.iter exes.names ~f:(fun name ->
        List.iter Mode.all ~f:(fun mode ->
          build_exe ~dir ~requires ~name ~mode ~modules ~dep_graph))


  (* +-----------------------------------------------------------------+
     | User actions                                                    |
     +-----------------------------------------------------------------+ *)

  module User_action_interpret : sig
    val expand
      :  User_action.Unexpanded.t
      -> dir:Path.t
      -> targets:string list
      -> deps:Dep_conf.t list
      -> (unit, string User_action.t) Build.t

    val run
      :  dir:Path.t
      -> targets:Path.t list
      -> (string User_action.t, unit) Build.t
  end = struct
    module U = User_action.Unexpanded

    let extract_artifacts t =
      U.fold t ~init:String_map.empty ~f:(fun acc var ->
        let module N = Named_artifacts in
        match String.lsplit2 var ~on:':' with
        | Some ("bin"     , s) -> String_map.add acc ~key:var ~data:(N.binary     s)
        | Some ("findlib" , s) -> String_map.add acc ~key:var ~data:(N.in_findlib s)
        | _ -> acc)

    let expand t ~artifact_map ~dir ~targets ~deps =
      let dep_exn name = function
        | Some dep -> dep
        | None -> die "cannot use ${%s} with files_recursively_in" name
      in
      let lookup var_name =
        match String_map.find var_name artifact_map with
        | Some path -> Some (Path.reach ~from:dir path)
        | None ->
          match var_name with
          | "@" -> Some (String.concat ~sep:" " targets)
          | "<" -> Some (match deps with [] -> "" | dep1::_ -> dep_exn var_name dep1)
          | "^" ->
            let deps = List.map deps ~f:(dep_exn var_name) in
            Some (String.concat ~sep:" " deps)
          | _ -> root_var_lookup ~dir var_name
      in
      U.expand t ~f:lookup

    let expand t ~dir ~targets ~deps =
      let deps = List.map deps ~f:(Dep_conf_interpret.only_plain_file ~dir) in
      let needed_artifacts = extract_artifacts t in
      if String_map.is_empty needed_artifacts then
        let s = expand t ~dir ~artifact_map:String_map.empty ~targets ~deps in
        Build.return s
      else begin
        Build.all (List.map (String_map.bindings needed_artifacts) ~f:(fun (name, artifact) ->
          artifact
          >>>
          Build.arr (fun path -> (name, path))))
        >>>
        Build.dyn_paths (Build.arr (List.map ~f:snd))
        >>>
        Build.arr (fun artifacts ->
          let artifact_map = String_map.of_alist_exn artifacts in
          expand t ~dir ~artifact_map ~targets ~deps)
      end

    let run ~dir ~targets =
      Build.arr (User_action.to_action ~dir ~env:ctx.env)
      >>>
      Build.action ~targets
  end

  (* +-----------------------------------------------------------------+
     | User rules                                                      |
     +-----------------------------------------------------------------+ *)

  let user_rule (rule : Rule.t) ~dir =
    let targets = List.map rule.targets ~f:(Path.relative dir) in
    BS.rule
      (Dep_conf_interpret.dep_of_list ~dir rule.deps
       >>>
       User_action_interpret.expand
         rule.action
         ~dir
         ~targets:rule.targets
         ~deps:rule.deps
       >>>
       User_action_interpret.run
         ~dir
         ~targets)

  (* +-----------------------------------------------------------------+
     | lex/yacc                                                        |
     +-----------------------------------------------------------------+ *)

  let ocamllex_rules (conf : Ocamllex.t) ~dir =
    List.iter conf.names ~f:(fun name ->
      let src = Path.relative dir (name ^ ".mll"   ) in
      let tmp = Path.relative dir (name ^ ".tmp.ml") in
      let dst = Path.relative dir (name ^ ".ml"    ) in
      BS.rule
        (Build.run (Dep ctx.ocamllex) [A "-q"; A "-o"; Path tmp; Dep src]
         >>>
         Build.create_file ~target:dst (fun () ->
           let repl = Path.to_string (Path.append ctx.build_dir dst) in
           let tmp = Path.to_string tmp in
           let dst = Path.to_string dst in
           Rewrite_generated_file.rewrite ~src:tmp ~dst ~repl;
           Sys.remove tmp)))

  let ocamlyacc_rules (conf : Ocamlyacc.t) ~dir =
    List.iter conf.names ~f:(fun name ->
      let src  = Path.relative dir (name ^ ".mly"    ) in
      let tmp  = Path.relative dir (name ^ ".tmp.ml" ) in
      let tmpi = Path.relative dir (name ^ ".tmp.mli") in
      let dst  = Path.relative dir (name ^ ".ml"     ) in
      let dsti = Path.relative dir (name ^ ".mli"    ) in
      BS.rule
        (Build.run
           (Dep ctx.ocamlyacc)
           [ A "-b"
           ; Path (Path.relative dir (name ^ ".tmp"))
           ; Dep src
           ]
         >>>
         Build.create_files ~targets:[dst; dsti] (fun () ->
           let repl = Path.to_string (Path.append ctx.build_dir dst) in
           let tmp = Path.to_string tmp in
           let dst = Path.to_string dst in
           Rewrite_generated_file.rewrite ~src:tmp ~dst ~repl;
           Sys.remove tmp;

           let repli = Path.to_string (Path.append ctx.build_dir dsti) in
           let tmpi = Path.to_string tmpi in
           let dsti = Path.to_string dsti in
           with_file_in tmpi ~f:(fun ic ->
             with_file_out dsti ~f:(fun oc ->
               Printf.fprintf oc "# 1 \"%s\"\n" repli;
               copy_channels ic oc));
           Sys.remove tmpi)))

  (* +-----------------------------------------------------------------+
     | Modules listing                                                 |
     +-----------------------------------------------------------------+ *)

  let guess_modules ~dir ~files =
    let ml_files, mli_files =
      String_set.elements files
      |> List.filter_map ~f:(fun fn ->
        if Filename.check_suffix fn ".ml" then
          Some (Inl fn)
        else if Filename.check_suffix fn ".mli" then
          Some (Inr fn)
        else
          None)
      |> List.partition_map ~f:(fun x -> x)
    in
    let parse_one_set files =
      List.map files ~f:(fun fn ->
        (String.capitalize_ascii (Filename.chop_extension fn),
         fn))
      |> String_map.of_alist
      |> function
      | Ok x -> x
      | Error (name, f1, f2) ->
        die "too many for module %s in %s: %s and %s"
          name (Path.to_string dir) f1 f2
    in
    let impls = parse_one_set ml_files  in
    let intfs = parse_one_set mli_files in
    String_map.merge impls intfs ~f:(fun name ml_fname mli_fname ->
      match ml_fname with
      | None ->
        die "module %s in %s doesn't have a corresponding .ml file"
          name (Path.to_string dir)
      | Some ml_fname ->
        Some
          { Module.
            name
          ; ml_fname  = ml_fname
          ; mli_fname = mli_fname
          ; obj_name  = ""
          })

  (* +-----------------------------------------------------------------+
     | Stanza                                                          |
     +-----------------------------------------------------------------+ *)

  let rules { src_dir; ctx_dir; stanzas } =
    let files = lazy (Path.readdir src_dir |> Array.to_list |> String_set.of_list) in
    let all_modules = lazy (
      let files_produced_by_rules =
        List.concat_map stanzas ~f:(fun stanza ->
          match (stanza : Stanza.t) with
          | Rule      rule -> rule.targets
          | Ocamllex  conf -> List.map conf.names ~f:(fun name -> name ^ ".ml")
          | Ocamlyacc conf -> List.concat_map conf.names ~f:(fun name ->
            [ name ^ ".ml"; name ^ ".mli" ])
          | _ -> [])
        |> String_set.of_list
      in
      guess_modules ~dir:src_dir
        ~files:(String_set.union (Lazy.force files) files_produced_by_rules))
    in
    List.iter stanzas ~f:(fun stanza ->
      let dir = ctx_dir in
      match (stanza : Stanza.t) with
      | Library     lib  -> library_rules     lib  ~dir ~all_modules:(Lazy.force all_modules) ~files:(Lazy.force files)
      | Executables exes -> executables_rules exes ~dir ~all_modules:(Lazy.force all_modules)
      | Rule        rule -> user_rule         rule ~dir
      | Ocamllex    conf -> ocamllex_rules    conf ~dir
      | Ocamlyacc   conf -> ocamlyacc_rules   conf ~dir
      | Provides _ | Other -> ())

  let () = List.iter P.stanzas ~f:rules

  (* +-----------------------------------------------------------------+
     | Installation                                                    |
     +-----------------------------------------------------------------+ *)

  let lib_install_files ~dir (lib : Library.t) : Install.Entry.t list =
    let files =
      let modules =
        Hashtbl.find_exn modules_by_lib lib.name
          ~string_of_key:(sprintf "%S")
          ~table_desc:(fun _ ->
            sprintf "<module table for context %s>"
              (Path.to_string ctx.build_dir))
      in
      List.concat
        [ List.concat_map modules ~f:(fun m ->
            List.concat
              [ [ Module.cm_file m ~dir Cmi
                ; Module.cm_file m ~dir Cmx ]
              ; List.filter_map Ml_kind.all ~f:(Module.cmt_file m ~dir)
              ; [ match Module.file m ~dir Intf with
                  | Some fn -> fn
                  | None    -> Path.relative dir m.ml_fname ]
              ])
        ; [ lib_archive ~dir lib ~ext:".cma" ]
        ; (match lib.c_names with
           | [] -> []
           | _  -> [ stubs_archive ~dir lib ])
        ; (match ctx.ocamlopt with
           | None -> []
           | Some _ ->
             [ lib_archive ~dir lib ~ext:".cmxa"
             ; lib_archive ~dir lib ~ext:ctx.ext_lib
             ])
        ]
    in
    let dlls =
      match lib.c_names with
      | [] -> []
      | _  -> [dll ~dir lib]
    in
    List.concat
      [ List.map files ~f:(fun src ->
          { Install.Entry. src; dst = None; section = Lib })
      ; List.map dlls  ~f:(fun src ->
          { Install.Entry. src; dst = None; section = Stublibs })
      ]

  let install_file package =
    let entries =
      List.concat_map P.stanzas ~f:(fun { ctx_dir = dir; stanzas; _ } ->
        List.concat_map stanzas ~f:(function
          | Library ({ public_name = Some name; _ } as lib)
            when Findlib.root_package_name name = package ->
            lib_install_files ~dir lib
          | _ -> []))
    in
    let entries =
      let meta = Path.of_string "META" in
      if Path.exists meta then
        { Install.Entry.
          src = meta
        ; dst = None
        ; section = Lib
        } :: entries
      else
        entries
    in
    let fn = Path.relative ctx.build_dir (package ^ ".install") in
    BS.rule
      (Build.path_set (Install.files entries) >>>
       Build.create_file ~target:fn (fun () ->
         Install.write_install_file fn entries))

  let () = List.iter P.packages ~f:install_file

  let () =
    if Path.basename ctx.build_dir = "default" then
      List.iter P.packages ~f:(fun pkg ->
        let fn = pkg ^ ".install" in
        BS.copy_rule
          ~src:(Path.relative ctx.build_dir fn)
          ~dst:(Path.relative Path.root     fn))
end

let gen ~context ~stanzas ~packages =
  let module M =
    Gen(struct
      let context  = context
      let stanzas  = stanzas
      let packages = packages
    end)
  in
  ()

open Import
open Jbuild_types
open Build.O

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

let g () =
  if !Clflags.g then
    ["-g"]
  else
    []

module Ocaml_flags = struct
  let default_ocamlc_flags   = g
  let default_ocamlopt_flags = g

  let dev_mode_warnings =
    "@a" ^
    String.concat ~sep:""
      (List.map ~f:(sprintf "-%d")
         [ 4
         ; 29
         ; 40
         ; 41
         ; 42
         ; 44
         ; 45
         ; 48
         ; 58
         ; 59
         ])

  let default_flags () =
    if !Clflags.dev_mode then
      [ "-w"; dev_mode_warnings ^ !Clflags.warnings
      ; "-strict-sequence"
      ; "-strict-formats"
      ]
    else
      [ "-w"; !Clflags.warnings ]

  type t =
    { common   : string list
    ; specific : string list Mode.Dict.t
    }

  let make { Buildable. flags; ocamlc_flags; ocamlopt_flags; _ } =
    let eval = Ordered_set_lang.eval_with_standard in
    { common   = eval flags ~standard:(default_flags ())
    ; specific =
        { byte   = eval ocamlc_flags   ~standard:(default_ocamlc_flags ())
        ; native = eval ocamlopt_flags ~standard:(default_ocamlopt_flags ())
        }
    }

  let get t mode = Arg_spec.As (t.common @ Mode.Dict.get t.specific mode)

  let get_for_cm t ~cm_kind = get t (Mode.of_cm_kind cm_kind)

  let default () =
    { common = default_flags ()
    ; specific =
        { byte   = default_ocamlc_flags   ()
        ; native = default_ocamlopt_flags ()
        }
    }
end

let default_c_flags = g ()
let default_cxx_flags = g ()

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
  val context   : Context.t
  val file_tree : File_tree.t
  val stanzas   : (Path.t * Jbuild_types.Stanza.t list) list
  val packages  : Package.t String_map.t
  val filter_out_optional_stanzas_with_missing_deps : bool
  val alias_store : Alias.Store.t
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

  let findlib = ctx.findlib

  module Mode = struct
    include Mode

    let choose byte native = function
      | Byte   -> byte
      | Native -> native

    let compiler t = choose (Some ctx.ocamlc) ctx.ocamlopt t

    let best =
      match ctx.ocamlopt with
      | Some _ -> Native
      | None   -> Byte
  end

  module Cm_kind = struct
    include Cm_kind

    let compiler = function
      | Cmi | Cmo -> Some ctx.ocamlc
      | Cmx -> ctx.ocamlopt
  end

  module Lib_db = struct
    open Lib_db

    let t = create findlib (List.map P.stanzas ~f:(fun d -> (d.ctx_dir, d.stanzas)))

    let find name = Build.arr (fun () -> find t name)

    module Libs_vfile =
      Vfile_kind.Make_full
        (struct type t = Lib.t list end)
        (struct
          open Sexp.To_sexp
          let t l = list string (List.map l ~f:Lib.best_name)
        end)
        (struct
          open Sexp.Of_sexp
          let t sexp = List.map (list string sexp) ~f:(Lib_db.find t)
        end)

    let vrequires ~dir ~item =
      let fn = Path.relative dir (item ^ ".requires.sexp") in
    Build.Vspec.T (fn, (module Libs_vfile))

    let load_requires ~dir ~item =
      Build.vpath (vrequires ~dir ~item)

    let vruntime_deps ~dir ~item =
      let fn = Path.relative dir (item ^ ".runtime-deps.sexp") in
    Build.Vspec.T (fn, (module Libs_vfile))

    let load_runtime_deps ~dir ~item =
      Build.vpath (vruntime_deps ~dir ~item)

    let with_fail ~fail build =
      match fail with
      | None -> build
      | Some f -> Build.fail f >>> build

    let closure ~dir ~dep_kind lib_deps =
      let internals, externals, fail = Lib_db.interpret_lib_deps t ~dir lib_deps in
      with_fail ~fail
        (Build.record_lib_deps ~dir ~kind:dep_kind lib_deps
         >>>
         Build.all
           (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
              load_requires ~dir ~item:lib.name))
         >>^ (fun internal_deps ->
           let externals =
             List.map (Findlib.closure externals) ~f:(fun pkg ->
               Lib.External pkg)
           in
           Lib.remove_dups_preserve_order
             (List.concat (externals :: internal_deps) @
              List.map internals ~f:(fun x -> Lib.Internal x))))

    let closed_ppx_runtime_deps_of ~dir ~dep_kind lib_deps =
      let internals, externals, fail = Lib_db.interpret_lib_deps t ~dir lib_deps in
      with_fail ~fail
        (Build.record_lib_deps ~dir ~kind:dep_kind lib_deps
         >>>
         Build.all
           (List.map internals ~f:(fun ((dir, lib) : Lib.Internal.t) ->
              load_runtime_deps ~dir ~item:lib.name))
         >>^ (fun libs ->
           let externals =
             List.map (Findlib.closed_ppx_runtime_deps_of externals)
               ~f:(fun pkg -> Lib.External pkg)
           in
           Lib.remove_dups_preserve_order (List.concat (externals :: libs))))

    let internal_libs_without_non_installable_optional_ones =
      internal_libs_without_non_installable_optional_ones t

    let select_rules ~dir lib_deps =
      List.map (Lib_db.resolve_selects t lib_deps) ~f:(fun { dst_fn; src_fn } ->
        let src = Path.relative dir src_fn in
        let dst = Path.relative dir dst_fn in
        Build.path src
        >>>
        Build.create_files ~targets:[dst] (fun () ->
          let src_fn = Path.to_string src in
          let dst_fn = Path.to_string dst in
          with_file_in src_fn ~f:(fun ic ->
            with_file_out dst_fn ~f:(fun oc ->
              Printf.fprintf oc "# 1 \"%s\"\n" src_fn;
              copy_channels ic oc))))

    (* Hides [t] so that we don't resolve things statically *)
    let t = ()
    let _ = t
  end

  module Artifacts = struct
    open Artifacts

    let t = create ctx (List.map P.stanzas ~f:(fun d -> (d.ctx_dir, d.stanzas)))

    let binary name = binary t name
    let file_of_lib ?use_provides ~dir name =
      let lib, file =
        match String.lsplit2 name ~on:':' with
        | None ->
          Loc.fail (Loc.in_file (Path.to_string (Path.relative dir "jbuild")))
            "invalid ${lib:...} form: %s" name
        | Some x -> x
      in
      (lib, file_of_lib t ~lib ~file ?use_provides)

    (* Hides [t] so that we don't resolve things statically *)
    let t = ()
    let _ = t
  end

  (* Hides [findlib] so that we don't resolve things statically *)
  let findlib = ()
  let _ = findlib

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

    let system ?dir ?stdout_to ?env ?extra_targets cmd ~needed_to =
      let path, arg, fail = Utils.system_shell ~needed_to in
      let build =
        run (Dep path) ?dir ?stdout_to ?env ?extra_targets
          [ As [arg; cmd] ]
      in
      match fail with
      | None -> build
      | Some fail -> Build.fail fail >>> build
  end

  module Alias = struct
    include Alias

    let add_deps t deps = add_deps P.alias_store t deps
  end

  let all_rules = ref []
  let known_targets_by_src_dir_so_far = ref Path.Map.empty

  let add_rule build =
    let rule = Build_interpret.Rule.make build in
    all_rules := rule :: !all_rules;
    known_targets_by_src_dir_so_far :=
      List.fold_left rule.targets ~init:!known_targets_by_src_dir_so_far
        ~f:(fun acc target ->
          match Path.extract_build_context (Build_interpret.Target.path target) with
          | None -> acc
          | Some (_, path) ->
            let dir = Path.parent path in
            let fn = Path.basename path in
            let files =
              match Path.Map.find dir acc with
              | None -> String_set.singleton fn
              | Some set -> String_set.add fn set
            in
            Path.Map.add acc ~key:dir ~data:files)

  let sources_and_targets_known_so_far ~src_path =
    let sources =
      match File_tree.find_dir P.file_tree src_path with
      | None -> String_set.empty
      | Some dir -> File_tree.Dir.files dir
    in
    match Path.Map.find src_path !known_targets_by_src_dir_so_far with
    | None -> sources
    | Some set -> String_set.union sources set

  (* +-----------------------------------------------------------------+
     | User variables                                                  |
     +-----------------------------------------------------------------+ *)

  let cxx_compiler, cxx_flags =
    match String.split_words ctx.bytecomp_c_compiler with
    | [] -> assert false
    | prog :: flags ->
      let comp =
        if Filename.is_relative prog then
          match Bin.which prog with
          | None -> Path.of_string "g++"
          | Some p -> p
        else
          Path.of_string prog
      in
      let flags =
        List.filter flags ~f:(fun s -> not (String.is_prefix s ~prefix:"-std="))
      in
      (comp, flags)

  (* Expand some $-vars within action strings of rules defined in jbuild files *)
  let dollar_var_map =
    let ocamlopt =
      match ctx.ocamlopt with
      | None -> Path.relative ctx.ocaml_bin "ocamlopt"
      | Some p -> p
    in
    let make =
      match Bin.make with
      | None -> "make"
      | Some p -> Path.to_string p
    in
    [ "-verbose"       , "" (*"-verbose";*)
    ; "CPP"            , ctx.bytecomp_c_compiler ^ " -E"
    ; "PA_CPP"         , ctx.bytecomp_c_compiler ^ " -undef -traditional -x c -E"
    ; "CC"             , ctx.bytecomp_c_compiler
    ; "CXX"            , String.concat ~sep:" " (Path.to_string cxx_compiler :: cxx_flags)
    ; "ocaml_bin"      , Path.to_string ctx.ocaml_bin
    ; "OCAML"          , Path.to_string ctx.ocaml
    ; "OCAMLC"         , Path.to_string ctx.ocamlc
    ; "OCAMLOPT"       , Path.to_string ocamlopt
    ; "ocaml_version"  , ctx.version
    ; "ocaml_where"    , Path.to_string ctx.stdlib_dir
    ; "ARCH_SIXTYFOUR" , string_of_bool ctx.arch_sixtyfour
    ; "PORTABLE_INT63" , "true"
    ; "MAKE"           , make
    ] |> String_map.of_alist
    |> function
    | Ok x -> x
    | Error _ -> assert false

  let root_var_lookup ~dir var_name =
    match var_name with
    | "ROOT" -> Some (Path.reach ~from:dir ctx.build_dir)
    | _ -> String_map.find var_name dollar_var_map

  let expand_vars ~dir s =
    String_with_vars.expand s ~f:(root_var_lookup ~dir)

  (* +-----------------------------------------------------------------+
     | User deps                                                       |
     +-----------------------------------------------------------------+ *)

  module Dep_conf_interpret = struct
    include Dep_conf

    let dep ~dir = function
      | File  s -> Build.path (Path.relative dir (expand_vars ~dir s))
      | Alias s -> Build.path (Alias.file (Alias.make ~dir (expand_vars ~dir s)))
      | Glob_files s -> begin
          let path = Path.relative dir (expand_vars ~dir s) in
          let dir = Path.parent path in
          let s = Path.basename path in
          match Glob_lexer.parse_string s with
          | Ok re ->
            Build.paths_glob ~dir (Re.compile re)
          | Error (_pos, msg) ->
            die "invalid glob in %s/jbuild: %s" (Path.to_string dir) msg
        end
      | Files_recursively_in s ->
        let path = Path.relative dir (expand_vars ~dir s) in
        Build.files_recursively_in ~dir:path

    let dep_of_list ~dir ts =
      let rec loop acc = function
        | [] -> acc
        | t :: ts ->
          loop (acc >>> dep ~dir t) ts
      in
      loop (Build.return ()) ts

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
      Build.Vspec.T (fn, (module Ocamldep_vfile))
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
    add_rule
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

  let build_ppx_driver ~dir ~dep_kind ~target ~runner pp_names =
    let mode = Mode.best in
    let compiler = Option.value_exn (Mode.compiler mode) in
    let libs =
      Build.fanout
        (Lib_db.closure ~dir ~dep_kind (Direct "ppx_driver" ::
                                        List.map pp_names ~f:Lib_dep.direct))
        (Lib_db.find runner)
      >>^ (fun (libs, runner) ->
        let runner_name = Lib.best_name runner in
        List.filter libs ~f:(fun lib ->
          Lib.best_name lib <> runner_name)
        @ [runner]
      )
    in
    add_rule
      (libs
       >>>
       Build.dyn_paths (Build.arr (Lib.archive_files ~mode ~ext_lib:ctx.ext_lib))
       >>>
       Build.run (Dep compiler)
         [ A "-o"; Target target
         ; Dyn (Lib.link_flags ~mode)
         ]);
    libs

  let get_ppx_driver pps ~dep_kind =
    let names =
      Pp_set.elements pps
      |> List.map ~f:Pp.to_string
    in
    let key = String.concat names ~sep:"+" in
    match Hashtbl.find ppx_drivers key with
    | Some x -> x
    | None ->
      let ppx_dir = Path.relative ctx.build_dir (sprintf ".ppx/%s" key) in
      let exe = Path.relative ppx_dir "ppx.exe" in
      let libs =
        build_ppx_driver names ~dir:ppx_dir ~dep_kind ~target:exe
          ~runner:"ppx_driver.runner"
      in
      Hashtbl.add ppx_drivers ~key ~data:(exe, libs);
      (exe, libs)

  let specific_args_for_ppx_rewriters ~dir ~lib_name (libs : Lib.t list) =
    let uses_inline_test = ref false in
    let uses_inline_bench = ref false in
    let uses_here = ref false in
    let uses_libname = ref false in
    List.iter libs ~f:(fun lib ->
      match Lib.best_name lib with
      | "ppx_here" | "ppx_assert" -> uses_here := true
      | "ppx_inline_test.libname" -> uses_libname := true
      | "ppx_expect" -> uses_inline_test := true; uses_here := true
      | "ppx_inline_test" -> uses_inline_test := true
      | "ppx_bench" -> uses_inline_bench := true
      | _ -> ());
    Arg_spec.S
      [ S (if !uses_here
           then [A "-dirname"; Path dir]
           else [])
      ; S (match lib_name with
          | Some name when !uses_libname ->
            [ A "-inline-test-lib"; A name ]
          | _ -> [])
      ; S (if !uses_inline_test(* && drop_test*)
           then [ A "-inline-test-drop-with-deadcode" ]
           else [])
      ; S (if !uses_inline_bench (*&& drop_bench*)
           then [ A "-bench-drop-with-deadcode" ]
           else [])
      ]

  (* Generate rules to build the .pp files and return a new module map where all filenames
     point to the .pp files *)
  let pped_modules ~dir ~dep_kind ~modules ~preprocess ~preprocessor_deps ~lib_name =
    let preprocessor_deps = Dep_conf_interpret.dep_of_list ~dir preprocessor_deps in
    String_map.map modules ~f:(fun (m : Module.t) ->
      match Preprocess_map.find m.name preprocess with
      | No_preprocessing -> m
      | Command cmd ->
        pped_module m ~dir ~f:(fun _kind src dst ->
          let dir = ctx.build_dir in
          add_rule
            (preprocessor_deps
             >>>
             Build.path src
             >>>
             Build.system ~stdout_to:dst ~dir
               ~needed_to:"run preprocessor commands"
               (sprintf "%s %s" (expand_vars ~dir cmd)
                  (Filename.quote (Path.reach src ~from:dir)))))
      | Pps { pps; flags } ->
        let ppx_exe, libs = get_ppx_driver pps ~dep_kind in
        pped_module m ~dir ~f:(fun kind src dst ->
          add_rule
            (preprocessor_deps
             >>>
             libs
             >>>
             Build.run
               (Dep ppx_exe)
               [ Dyn (specific_args_for_ppx_rewriters ~dir ~lib_name)
               ; As flags
               ; A "-dump-ast"
               ; A "-o"; Target dst
               ; Ml_kind.flag kind; Dep src
               ])
        )
    )

  let real_requires ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps =
    let all_pps =
      Preprocess_map.pps preprocess
      |> Pp_set.elements
      |> List.map ~f:Pp.to_string
    in
    let vrequires = Lib_db.vrequires ~dir ~item in
    add_rule
      (Build.record_lib_deps ~dir ~kind:dep_kind (List.map virtual_deps ~f:Lib_dep.direct)
       >>>
       Build.fanout
         (Lib_db.closure ~dir ~dep_kind libraries)
         (Lib_db.closed_ppx_runtime_deps_of ~dir ~dep_kind
            (List.map all_pps ~f:Lib_dep.direct))
       >>>
       Build.arr (fun (libs, rt_deps) ->
         Lib.remove_dups_preserve_order (libs @ rt_deps))
       >>>
       Build.store_vfile vrequires);
    Build.vpath vrequires

  let requires ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps =
    let real_requires =
      real_requires ~dir ~dep_kind ~item ~libraries ~preprocess ~virtual_deps
    in
    let requires =
      if ctx.merlin then
        match Path.extract_build_context dir with
        | Some (_, remaindir) ->
          Build.path (Path.relative remaindir ".merlin") >>> real_requires
        | _ -> real_requires
      else
        real_requires
    in
    (requires, real_requires)

  let dot_merlin ~dir ~requires ~alias_modules =
    if ctx.merlin then
      match Path.extract_build_context dir with
      | Some (_, remaindir) ->
        let path = Path.relative remaindir ".merlin" in
        add_rule (
          requires
          >>^ (fun libs ->
              let internals, externals =
                List.partition_map libs ~f:(function
                  | Lib.Internal (path, _) ->
                    let path = Path.reach path ~from:remaindir in
                    Inl ("B " ^ path)
                  | Lib.External pkg ->
                    Inr ("PKG " ^ pkg.name))
              in
              let dot_merlin =
                [ "S ." ; "B " ^ (Path.reach dir ~from:remaindir) ]
                @ internals
                @ externals
                @ List.map alias_modules ~f:(fun (m : Module.t) ->
                    "FLG -open " ^ m.name)
              in
              dot_merlin
              |> String_set.of_list
              |> String_set.elements
              |> List.map ~f:(Printf.sprintf "%s\n")
              |> String.concat ~sep:"")
          >>>
          Build.echo path
        )
      | _ ->
        ()

  let merge_dot_merlin merlin_deps ~dir =
    if ctx.merlin && merlin_deps <> [] then
      let requires, alias_modules = List.split merlin_deps in
      let alias_modules = List.filter_map alias_modules ~f:(fun x -> x) in
      let requires =
        Build.all requires
        >>^ fun requires ->
        Lib.remove_dups_preserve_order (List.concat requires)
      in
      dot_merlin ~dir ~requires ~alias_modules

  let setup_runtime_deps ~dir ~dep_kind ~item ~libraries ~ppx_runtime_libraries =
    let vruntime_deps = Lib_db.vruntime_deps ~dir ~item in
    add_rule
      (Build.fanout
         (Lib_db.closure ~dir ~dep_kind (List.map ppx_runtime_libraries ~f:Lib_dep.direct))
         (Lib_db.closed_ppx_runtime_deps_of ~dir ~dep_kind libraries)
       >>>
       Build.arr (fun (rt_deps, rt_deps_of_deps) ->
         Lib.remove_dups_preserve_order (rt_deps @ rt_deps_of_deps))
       >>>
       Build.store_vfile vruntime_deps)

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
            (fn, Sexp_load.single (Path.to_string path)))
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

  let build_cm ~flags ~cm_kind ~dep_graph ~requires
        ~(modules : Module.t String_map.t) ~dir ~alias_module (m : Module.t) =
    Option.iter (Cm_kind.compiler cm_kind) ~f:(fun compiler ->
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
        let extra_targets =
          match cm_kind with
          | Cmx -> Path.relative dir (m.obj_name ^ ctx.ext_obj) :: extra_targets
          | Cmi | Cmo -> extra_targets
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
        add_rule
          (Build.paths extra_deps >>>
           other_cm_files >>>
           requires >>>
           Build.dyn_paths (Build.arr (lib_dependencies ~cm_kind)) >>>
           Build.run (Dep compiler)
             ~extra_targets
             [ Ocaml_flags.get_for_cm flags ~cm_kind
             ; cmt_args
             ; Dyn Lib.include_flags
             ; As extra_args
             ; A "-no-alias-deps"
             ; A "-I"; Path dir
             ; (match alias_module with
                | None -> S []
                | Some (m : Module.t) -> As ["-open"; m.name])
             ; A "-o"; Target dst
             ; A "-c"; Ml_kind.flag ml_kind; Dep src
             ])))

  let build_module ~flags m ~dir ~dep_graph ~modules ~requires ~alias_module =
    List.iter Cm_kind.all ~f:(fun cm_kind ->
      build_cm ~flags ~dir ~dep_graph ~modules m ~cm_kind ~requires ~alias_module)

  let build_modules ~flags ~dir ~dep_graph ~modules ~requires ~alias_module =
    String_map.iter
      (match alias_module with
       | None -> modules
       | Some (m : Module.t) -> String_map.remove m.name modules)
      ~f:(fun ~key:_ ~data:m ->
        build_module m ~flags ~dir ~dep_graph ~modules ~requires ~alias_module)

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

  let stubs_archive lib ~dir =
    Library.stubs_archive lib ~dir ~ext_lib:ctx.ext_lib

  let dll (lib : Library.t) ~dir =
    Path.relative dir (sprintf "dll%s_stubs%s" lib.name ctx.ext_dll)

  let build_lib (lib : Library.t) ~flags ~dir ~mode ~modules ~dep_graph =
    Option.iter (Mode.compiler mode) ~f:(fun compiler ->
      let target = lib_archive lib ~dir ~ext:(Mode.compiled_lib_ext mode) in
      let dep_graph = Ml_kind.Dict.get dep_graph Impl in
      let stubs_flags =
        if not (Library.has_stubs lib) then
          []
        else
          let stubs_name = lib.name ^ "_stubs" in
          match mode with
          | Byte -> ["-dllib"; "-l" ^ stubs_name; "-cclib"; "-l" ^ stubs_name]
          | Native -> ["-cclib"; "-l" ^ stubs_name]
      in
      add_rule
        (Build.fanout
           (dep_graph >>>
            Build.arr (fun dep_graph ->
              names_to_top_closed_cm_files
                ~dir
                ~dep_graph
                ~modules
                ~mode
                (String_map.keys modules)))
           (expand_and_eval_set ~dir lib.c_library_flags ~standard:[])
         >>>
         Build.run (Dep compiler)
           ~extra_targets:(
             match mode with
             | Byte -> []
             | Native -> [lib_archive lib ~dir ~ext:ctx.ext_lib])
           [ Ocaml_flags.get flags mode
           ; A "-a"; A "-o"; Target target
           ; As stubs_flags
           ; Dyn (fun (_, cclibs) ->
               S (List.map cclibs ~f:(fun flag ->
                 Arg_spec.S [A "-cclib"; A flag])))
           ; As (List.map lib.library_flags ~f:(expand_vars ~dir))
           ; As (match lib.kind with
               | Normal -> []
               | Ppx_type_conv_plugin | Ppx_rewriter -> ["-linkall"])
           ; Dyn (fun (cm_files, _) -> Deps cm_files)
           ]))

  let mk_lib_cm_all (lib : Library.t) ~dir ~modules cm_kind =
    let deps = cm_files ~dir (String_map.values modules) ~cm_kind in
    add_rule (Build.paths deps >>>
            Build.return "" >>>
            Build.echo (lib_cm_all lib ~dir cm_kind))

  let expand_includes ~dir includes =
    Arg_spec.As (List.concat_map includes ~f:(fun s ->
      ["-I"; expand_vars ~dir s]))

  let build_c_file (lib : Library.t) ~dir ~requires ~h_files c_name =
    let src = Path.relative dir (c_name ^ ".c") in
    let dst = Path.relative dir (c_name ^ ctx.ext_obj) in
    add_rule
      (Build.paths h_files
       >>>
       Build.fanout
         (expand_and_eval_set ~dir lib.c_flags ~standard:default_c_flags)
         (requires
          >>>
          Build.dyn_paths (Build.arr Lib.header_files))
       >>>
       Build.run
         (* We have to execute the rule in the library directory as the .o is produced in
            the current directory *)
         ~dir
         (Dep ctx.ocamlc)
         [ As (g ())
         ; expand_includes ~dir lib.includes
         ; Dyn (fun (c_flags, libs) ->
             S [ Lib.c_include_flags libs
               ; As (List.concat_map c_flags ~f:(fun f -> ["-ccopt"; f]))
               ])
         ; A "-o"; Target dst
         ; Dep src
         ]);
    dst

  let build_cxx_file (lib : Library.t) ~dir ~requires ~h_files c_name =
    let src = Path.relative dir (c_name ^ ".cpp") in
    let dst = Path.relative dir (c_name ^ ctx.ext_obj) in
    add_rule
      (Build.paths h_files
       >>>
       Build.fanout
         (expand_and_eval_set ~dir lib.cxx_flags ~standard:default_cxx_flags)
         requires
       >>>
       Build.run
         (* We have to execute the rule in the library directory as the .o is produced in
            the current directory *)
         ~dir
         (Dep cxx_compiler)
         [ S [A "-I"; Path ctx.stdlib_dir]
         ; expand_includes ~dir lib.includes
         ; As cxx_flags
         ; Dyn (fun (cxx_flags, libs) ->
             S [ Lib.c_include_flags libs
               ; As cxx_flags
               ])
         ; A "-o"; Target dst
         ; A "-c"; Dep src
         ]);
    dst

  (* Hack for the install file *)
  let modules_by_lib : (string, Module.t list) Hashtbl.t = Hashtbl.create 32

  let library_rules (lib : Library.t) ~dir ~all_modules ~files =
    let dep_kind = if lib.optional then Build.Optional else Required in
    let flags = Ocaml_flags.make lib.buildable in
    let modules =
      parse_modules ~dir ~all_modules ~modules_written_by_user:lib.buildable.modules
    in
    let main_module_name = String.capitalize_ascii lib.name in
    let modules =
      String_map.map modules ~f:(fun (m : Module.t) ->
        if not lib.wrapped || m.name = main_module_name then
          { m with obj_name = obj_name_of_basename m.ml_fname }
        else
          { m with obj_name = sprintf "%s__%s" lib.name m.name })
    in
    let alias_module =
      if not lib.wrapped ||
         (String_map.cardinal modules = 1 &&
          String_map.mem main_module_name modules) then
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
      pped_modules ~dir ~dep_kind ~modules ~preprocess:lib.buildable.preprocess
        ~preprocessor_deps:lib.buildable.preprocessor_deps
        ~lib_name:(Some lib.name)
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
      add_rule
        (Build.return
           (String_map.values (String_map.remove m.name modules)
            |> List.map ~f:(fun (m : Module.t) ->
              sprintf "module %s = %s\n" m.name (Module.real_unit_name m))
            |> String.concat ~sep:"")
         >>> Build.echo (Path.relative dir m.ml_fname)));

    let requires, real_requires =
      requires ~dir ~dep_kind ~item:lib.name
        ~libraries:lib.buildable.libraries
        ~preprocess:lib.buildable.preprocess
        ~virtual_deps:lib.virtual_deps
    in

    setup_runtime_deps ~dir ~dep_kind ~item:lib.name
      ~libraries:lib.buildable.libraries
      ~ppx_runtime_libraries:lib.ppx_runtime_libraries;
    List.iter (Lib_db.select_rules ~dir lib.buildable.libraries) ~f:add_rule;

    build_modules ~flags ~dir ~dep_graph ~modules ~requires ~alias_module;
    Option.iter alias_module ~f:(fun m ->
      let flags = Ocaml_flags.default () in
      build_module m
        ~flags:{ flags with common = flags.common @ ["-w"; "-49"] }
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

    if Library.has_stubs lib then begin
      let h_files =
        String_set.elements files
        |> List.filter_map ~f:(fun fn ->
          if String.is_suffix fn ~suffix:".h" then
            Some (Path.relative dir fn)
          else
            None)
      in
      let o_files =
        List.map lib.c_names   ~f:(build_c_file   lib ~dir ~requires ~h_files) @
        List.map lib.cxx_names ~f:(build_cxx_file lib ~dir ~requires ~h_files)
      in
      match lib.self_build_stubs_archive with
      | Some _ -> ()
      | None ->
        let targets = [ stubs_archive lib ~dir; dll lib ~dir ] in
        add_rule
          (expand_and_eval_set ~dir lib.c_library_flags ~standard:[]
           >>>
           Build.run
             ~extra_targets:targets
             (Dep ctx.ocamlmklib)
             [ As (g ())
             ; A "-o"
             ; Path (Path.relative dir (sprintf "%s_stubs" lib.name))
             ; Deps o_files
             ; Dyn (fun cclibs -> As cclibs)
             ]);
    end;

    List.iter Cm_kind.all ~f:(mk_lib_cm_all lib ~dir ~modules);

    List.iter Mode.all ~f:(fun mode ->
      build_lib lib ~flags ~dir ~mode ~modules ~dep_graph);

    Option.iter ctx.ocamlopt ~f:(fun ocamlopt ->
      let src = lib_archive lib ~dir ~ext:(Mode.compiled_lib_ext Native) in
      let dst = lib_archive lib ~dir ~ext:".cmxs" in
      let build =
        Build.run
          (Dep ocamlopt)
          [ Ocaml_flags.get flags Native
          ; A "-shared"; A "-linkall"
          ; A "-I"; Path dir
          ; A "-o"; Target dst
          ; Dep src
          ]
      in
      let build =
        if Library.has_stubs lib then
          Build.path (stubs_archive ~dir lib)
          >>>
          build
        else
          build
      in
      add_rule build
    );

    begin match lib.kind with
    | Normal | Ppx_type_conv_plugin -> ()
    | Ppx_rewriter ->
      ignore
        (build_ppx_driver [lib.name]
           ~dir
           ~dep_kind
           ~target:(Path.relative dir "as-ppx.exe")
           ~runner:"ppx_driver.runner_as_ppx"
         : (unit, Lib.t list) Build.t)
    end;

    (real_requires, alias_module)

  (* +-----------------------------------------------------------------+
     | Executables stuff                                               |
     +-----------------------------------------------------------------+ *)

  let build_exe ~flags ~dir ~requires ~name ~mode ~modules ~dep_graph ~link_flags =
    let exe_ext = Mode.exe_ext mode in
    let mode, link_flags, compiler =
      match Mode.compiler mode with
      | Some compiler -> (mode, link_flags, compiler)
      | None          -> (Byte, "-custom" :: link_flags, ctx.ocamlc)
    in
    let dep_graph = Ml_kind.Dict.get dep_graph Impl in
    let exe = Path.relative dir (name ^ exe_ext) in
    add_rule
      (Build.fanout
         (requires
          >>> Build.dyn_paths (Build.arr (Lib.archive_files ~mode ~ext_lib:ctx.ext_lib)))
         (dep_graph
          >>> Build.arr (fun dep_graph ->
            names_to_top_closed_cm_files
              ~dir
              ~dep_graph
              ~modules
              ~mode
              [String.capitalize_ascii name]))
       >>>
       Build.run
         (Dep compiler)
         [ Ocaml_flags.get flags mode
         ; A "-o"; Target exe
         ; As link_flags
         ; Dyn (fun (libs, _) -> Lib.link_flags libs ~mode)
         ; Dyn (fun (_, cm_files) -> Deps cm_files)
         ])

  let executables_rules (exes : Executables.t) ~dir ~all_modules =
    let dep_kind = Build.Required in
    let flags = Ocaml_flags.make exes.buildable in
    let modules =
      parse_modules ~dir ~all_modules ~modules_written_by_user:exes.buildable.modules
    in
    let modules =
      String_map.map modules ~f:(fun (m : Module.t) ->
        { m with obj_name = obj_name_of_basename m.ml_fname })
    in
    List.iter exes.names ~f:(fun name ->
      if not (String_map.mem (String.capitalize name) modules) then
        die "executable %s in %s doesn't have a corresponding .ml file"
          name (Path.to_string dir));
    let modules =
      pped_modules ~dir ~dep_kind ~modules
        ~preprocess:exes.buildable.preprocess
        ~preprocessor_deps:exes.buildable.preprocessor_deps
        ~lib_name:None
    in
    let item = List.hd exes.names in
    let dep_graph = ocamldep_rules ~dir ~item ~modules ~alias_module:None in

    let requires, real_requires =
      requires ~dir ~dep_kind ~item
        ~libraries:exes.buildable.libraries
        ~preprocess:exes.buildable.preprocess
        ~virtual_deps:[]
    in

    List.iter (Lib_db.select_rules ~dir exes.buildable.libraries) ~f:add_rule;

    build_modules ~flags ~dir ~dep_graph ~modules ~requires ~alias_module:None;

    List.iter exes.names ~f:(fun name ->
      List.iter Mode.all ~f:(fun mode ->
        build_exe ~flags ~dir ~requires ~name ~mode ~modules ~dep_graph
          ~link_flags:exes.link_flags));

    (real_requires, None)

  (* +-----------------------------------------------------------------+
     | User actions                                                    |
     +-----------------------------------------------------------------+ *)

  module Action_interpret : sig
    val run
      :  Action.Unexpanded.t
      -> dir:Path.t
      -> dep_kind:Build.lib_dep_kind
      -> targets:Path.t list
      -> deps:Dep_conf.t list
      -> (unit, unit) Build.t
  end = struct
    module U = Action.Unexpanded

    type resolved_forms =
      { (* Mapping from ${...} forms to their resolutions *)
        artifacts : Path.t String_map.t
      ; (* Failed resolutions *)
        failures  : fail list
      ; (* All "name" for ${lib:name:...} forms *)
        lib_deps  : String_set.t
      }

    let add_artifact ?lib_dep acc ~var result =
      let lib_deps =
        match lib_dep with
        | None -> acc.lib_deps
        | Some lib -> String_set.add lib acc.lib_deps
      in
      match result with
      | Ok path ->
        { acc with
          artifacts = String_map.add acc.artifacts ~key:var ~data:path
        ; lib_deps
        }
      | Error fail ->
        { acc with
          failures = fail :: acc.failures
        ; lib_deps
        }

    let extract_artifacts ~dir t =
      let init =
        { artifacts = String_map.empty
        ; failures  = []
        ; lib_deps  = String_set.empty
        }
      in
      U.fold t ~init ~f:(fun acc var ->
        let module A = Artifacts in
        match String.lsplit2 var ~on:':' with
        | Some ("exe"     , s) -> add_artifact acc ~var (Ok (Path.relative dir s))
        | Some ("path"    , s) -> add_artifact acc ~var (Ok (Path.relative dir s))
        | Some ("bin"     , s) -> add_artifact acc ~var (A.binary s)
        | Some ("lib"     , s)
        | Some ("libexec" , s) ->
          let lib_dep, res = A.file_of_lib ~dir s in
          add_artifact acc ~var ~lib_dep res
        (* CR-someday jdimino: allow this only for (jbuild_version jane_street) *)
        | Some ("findlib" , s) ->
          let lib_dep, res = A.file_of_lib ~dir s ~use_provides:true in
          add_artifact acc ~var ~lib_dep res
        | _ -> acc)

    let expand_string_with_vars ~artifacts ~targets ~deps =
      let dep_exn ~dir name = function
        | Some dep -> Path.reach ~from:dir dep
        | None -> die "cannot use ${%s} with files_recursively_in" name
      in
      let lookup ~dir var_name =
        match String_map.find var_name artifacts with
        | Some path -> Some (Path.reach ~from:dir path)
        | None ->
          match var_name with
          | "@" -> Some (String.concat ~sep:" "
                           (List.map targets ~f:(Path.reach ~from:dir)))
          | "<" -> Some (match deps with [] -> "" | dep1::_ -> dep_exn ~dir var_name dep1)
          | "^" ->
            let deps = List.map deps ~f:(dep_exn ~dir var_name) in
            Some (String.concat ~sep:" " deps)
          | _ -> root_var_lookup ~dir var_name
      in
      fun ~dir str ->
        String_with_vars.expand str ~f:(lookup ~dir)

    let run t ~dir ~dep_kind ~targets ~deps =
      let deps =
        List.map deps ~f:(fun dep ->
          Option.map (Dep_conf_interpret.only_plain_file ~dir dep)
            ~f:(Path.relative dir))
      in
      let forms = extract_artifacts ~dir t in
      let build =
        Build.record_lib_deps ~dir ~kind:dep_kind
          (String_set.elements forms.lib_deps
           |> List.map ~f:(fun s -> Lib_dep.Direct s))
        >>>
        Build.paths (String_map.values forms.artifacts)
        >>>
        Build.action t ~dir ~env:ctx.env ~targets
          ~expand:(expand_string_with_vars ~artifacts:forms.artifacts ~targets ~deps)
      in
      match forms.failures with
      | [] -> build
      | fail :: _ -> Build.fail fail >>> build
  end

  (* +-----------------------------------------------------------------+
     | User rules                                                      |
     +-----------------------------------------------------------------+ *)

  let user_rule (rule : Rule.t) ~dir =
    let targets = List.map rule.targets ~f:(Path.relative dir) in
    add_rule
      (Dep_conf_interpret.dep_of_list ~dir rule.deps
       >>>
       Action_interpret.run
         rule.action
         ~dir
         ~dep_kind:Required
         ~targets
         ~deps:rule.deps)

  let alias_rules (alias_conf : Alias_conf.t) ~dir =
    let digest =
      let deps =
        Sexp.To_sexp.list Dep_conf_interpret.sexp_of_t alias_conf.deps in
      let action =
        match alias_conf.action with
        | None -> Sexp.Atom "none"
        | Some a -> List [Atom "some" ; Action.Unexpanded.sexp_of_t a] in
      Sexp.List [deps ; action]
      |> Sexp.to_string
      |> Digest.string
      |> Digest.to_hex in
    let alias = Alias.make alias_conf.name ~dir in
    let digest_path = Path.extend_basename (Alias.file alias) ~suffix:("-" ^ digest) in
    let dummy = Build.touch digest_path in
    Alias.add_deps alias [digest_path];
    let deps =
      let deps = Dep_conf_interpret.dep_of_list ~dir alias_conf.deps in
      match alias_conf.action with
      | None -> deps
      | Some action ->
        deps
        >>> Action_interpret.run
          action
          ~dir
          ~dep_kind:Required
          ~targets:[]
          ~deps:alias_conf.deps
    in
    add_rule (deps >>> dummy)

  (* +-----------------------------------------------------------------+
     | Modules listing                                                 |
     +-----------------------------------------------------------------+ *)

  let guess_modules ~dir ~files =
    let ml_files, mli_files =
      String_set.elements files
      |> List.filter_map ~f:(fun fn ->
        match String.lsplit2 fn ~on:'.' with
        | Some (_, "ml") ->
          Some (Inl fn)
        | Some (_, "mli") ->
          Some (Inr fn)
        | _ ->
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
      let ml_fname =
        match ml_fname with
        | None ->
          let mli_fname = Option.value_exn mli_fname in
          let ml_fname = String.sub mli_fname ~pos:0 ~len:(String.length mli_fname - 1) in
          Format.eprintf
            "@{<warning>Warning@}: Module %s in %s doesn't have a \
             corresponding .ml file.\n\
             Modules without an implementation are not recommended, \
             see this discussion:\n\
             \n\
            \  https://github.com/janestreet/jbuilder/issues/9\n\
             \n\
             In the meantime I'm setting up a rule for copying %s to %s.\n"
            name (Path.to_string dir)
            mli_fname ml_fname;
          let dir = Path.append ctx.build_dir dir in
          add_rule
            (Build.copy
               ~src:(Path.relative dir mli_fname)
               ~dst:(Path.relative dir ml_fname));
          ml_fname
        | Some ml_fname -> ml_fname
      in
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
    (* Interpret user rules and other simple stanzas first in order to populate the known
       target table, which is needed for guessing the list of modules. *)
    List.iter stanzas ~f:(fun stanza ->
      let dir = ctx_dir in
      match (stanza : Stanza.t) with
      | Rule         rule -> user_rule         rule ~dir
      | Alias        alias -> alias_rules alias ~dir
      | Library _ | Executables _ | Provides _ | Install _ -> ());
    let files = lazy (
      let files = sources_and_targets_known_so_far ~src_path:src_dir in
      (* Manually add files generated by the (select ...) dependencies since we haven't
         interpreted libraries and executables yet. *)
      List.fold_left stanzas ~init:files ~f:(fun acc stanza ->
        match (stanza : Stanza.t) with
        | Library { buildable; _ } | Executables { buildable; _ } ->
          List.fold_left buildable.libraries ~init:acc ~f:(fun acc dep ->
            match (dep : Jbuild_types.Lib_dep.t) with
            | Direct _ -> acc
            | Select s -> String_set.add s.result_fn acc)
        | _ -> acc)
    ) in
    let all_modules = lazy (
      guess_modules ~dir:src_dir
        ~files:(Lazy.force files))
    in
    List.filter_map stanzas ~f:(fun stanza ->
      let dir = ctx_dir in
      match (stanza : Stanza.t) with
      | Library lib  ->
        Some (library_rules lib ~dir
                ~all_modules:(Lazy.force all_modules) ~files:(Lazy.force files))
      | Executables  exes ->
        Some (executables_rules exes ~dir ~all_modules:(Lazy.force all_modules))
      | _ -> None)
    |> merge_dot_merlin ~dir:ctx_dir

  let () = List.iter P.stanzas ~f:rules

  (* +-----------------------------------------------------------------+
     | META                                                            |
     +-----------------------------------------------------------------+ *)

  (* The rules for META files must come after the interpretation of the jbuild stanzas
     since a user rule might generate a META.<package> file *)

  let stanzas_to_consider_for_install =
    if P.filter_out_optional_stanzas_with_missing_deps then
      List.concat_map P.stanzas ~f:(fun { ctx_dir; stanzas; _ } ->
        List.filter_map stanzas ~f:(function
          | Library _ -> None
          | stanza    -> Some (ctx_dir, stanza)))
      @ List.map (Lib_db.internal_libs_without_non_installable_optional_ones)
          ~f:(fun (dir, lib) -> (dir, Stanza.Library lib))
    else
      List.concat_map P.stanzas ~f:(fun { ctx_dir; stanzas; _ } ->
        List.map stanzas ~f:(fun s -> (ctx_dir, s)))

  (* META files that must be installed. Either because there is an explicit or user
     generated one, or because *)
  let packages_with_explicit_or_user_generated_meta =
    String_map.values P.packages
    |> List.filter_map ~f:(fun (pkg : Package.t) ->
      let path = Path.append ctx.build_dir pkg.path in
      let meta_fn = "META." ^ pkg.name in
      let meta_templ_fn = meta_fn ^ ".template" in

      let files = sources_and_targets_known_so_far ~src_path:pkg.path in
      let has_meta, has_meta_tmpl =
        (String_set.mem meta_fn files,
         String_set.mem meta_templ_fn files)
      in

      let meta_fn =
        if has_meta then
          meta_fn ^ ".from-jbuilder"
        else
          meta_fn
      in
      let meta_path = Path.relative path meta_fn in

      let version =
        match pkg.version_from_opam_file with
        | Some s -> Gen_meta.This s
        | None ->
          let candicates =
            [ pkg.name ^ ".version"
            ; "version"
            ; "VERSION"
            ]
          in
          match List.find candicates ~f:(fun fn -> String_set.mem fn files) with
          | None -> Na
          | Some fn -> Load (Path.relative path fn)
      in

      let template =
        if has_meta_tmpl then
          let meta_templ_path = Path.relative pkg.path meta_templ_fn in
          Build.path meta_templ_path
          >>^ fun () ->
          lines_of_file (Path.to_string meta_templ_path)
        else
          Build.return ["# JBUILDER_GEN"]
      in
      let meta =
        Gen_meta.gen ~package:pkg.name
          ~version
          ~stanzas:stanzas_to_consider_for_install
          ~lib_deps:(fun ~dir jbuild ->
            match jbuild with
            | Library lib ->
              Build.arr ignore
              >>>
              Lib_db.load_requires ~dir ~item:lib.name
              >>^ List.map ~f:Lib.best_name
            | Executables exes ->
              let item = List.hd exes.names in
              Build.arr ignore
              >>>
              Lib_db.load_requires ~dir ~item
              >>^ List.map ~f:Lib.best_name
            | _ -> Build.arr (fun _ -> []))
          ~ppx_runtime_deps:(fun ~dir jbuild ->
            match jbuild with
            | Library lib ->
              Build.arr ignore
              >>>
              Lib_db.load_runtime_deps ~dir ~item:lib.name
              >>^ List.map ~f:Lib.best_name
            | _ -> Build.arr (fun _ -> []))
      in
      add_rule
        (Build.fanout meta template
         >>>
         Build.create_file ~target:meta_path (fun ((meta : Meta.t), template) ->
           with_file_out (Path.to_string meta_path) ~f:(fun oc ->
             let ppf = Format.formatter_of_out_channel oc in
             Format.pp_open_vbox ppf 0;
             List.iter template ~f:(fun s ->
               if String.is_prefix s ~prefix:"#" then
                 match
                   String.split_words (String.sub s ~pos:1 ~len:(String.length s - 1))
                 with
                 | ["JBUILDER_GEN"] -> Format.fprintf ppf "%a@," Meta.pp meta.entries
                 | _ -> Format.fprintf ppf "%s@," s
               else
                 Format.fprintf ppf "%s@," s);
             Format.pp_close_box ppf ();
             Format.pp_print_flush ppf ())));

      if has_meta || has_meta_tmpl then
        Some pkg.name
      else
        None)
    |> String_set.of_list

  (* +-----------------------------------------------------------------+
     | Installation                                                    |
     +-----------------------------------------------------------------+ *)

  let lib_install_files ~dir (lib : Library.t) =
    let byte   = List.mem Mode.Byte   ~set:lib.modes in
    let native = List.mem Mode.Native ~set:lib.modes in
    let if_ cond l = if cond then l else [] in
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
              [ [ Module.cm_file m ~dir Cmi ]
              ; if_ native [ Module.cm_file m ~dir Cmx ]
              ; List.filter_map Ml_kind.all ~f:(Module.cmt_file m ~dir)
              ; [ match Module.file m ~dir Intf with
                  | Some fn -> fn
                  | None    -> Path.relative dir m.ml_fname ]
              ])
        ; if_ byte [ lib_archive ~dir lib ~ext:".cma" ]
        ; if_ (Library.has_stubs lib) [ stubs_archive ~dir lib ]
        ; if_ native
            (match ctx.ocamlopt with
             | None -> []
             | Some _ ->
               [ lib_archive ~dir lib ~ext:".cmxa"
               ; lib_archive ~dir lib ~ext:ctx.ext_lib
               ; lib_archive ~dir lib ~ext:".cmxs"
               ])
        ; (match lib.js_of_ocaml with
           | None -> []
           | Some { javascript_files = l; _ } ->
             List.map l ~f:(Path.relative dir))
        ; List.map lib.install_c_headers ~f:(fun fn ->
            Path.relative dir (fn ^ ".h"))
        ]
    in
    let dlls  = if_ (byte && Library.has_stubs lib) [dll ~dir lib] in
    let execs =
      match lib.kind with
      | Normal | Ppx_type_conv_plugin -> []
      | Ppx_rewriter -> [Path.relative dir "as-ppx.exe"]
    in
    List.concat
      [ List.map files ~f:(Install.Entry.make Lib     )
      ; List.map execs ~f:(Install.Entry.make Libexec )
      ; List.map dlls  ~f:(Install.Entry.make Stublibs)
      ]

  let obj_install_files ~dir (exes : Executables.t) =
    List.concat_map exes.names ~f:(fun name ->
      [ Path.relative dir (name ^ ".cmo")
      ; Path.relative dir (name ^ ".cmx")
      ; Path.relative dir (name ^ ctx.ext_obj)
      ])
    |> List.map ~f:(Install.Entry.make Lib)

  let is_odig_doc_file fn =
    List.exists [ "README"; "LICENSE"; "CHANGE"; "HISTORY"]
      ~f:(fun prefix -> String.is_prefix fn ~prefix)

  let local_install_rules (entries : Install.Entry.t list) ~package =
    let install_dir = Config.local_install_dir ~context:ctx.name in
    List.iter entries ~f:(fun entry ->
      let dst = Install.Entry.relative_installed_path entry ~package in
      add_rule
        (Build.symlink ~src:entry.src ~dst:(Path.append install_dir dst)))

  let install_file package_path package =
    let entries =
      List.concat_map stanzas_to_consider_for_install ~f:(fun (dir, stanza) ->
        match stanza with
        | Library ({ public_name = Some name; _ } as lib)
          when Findlib.root_package_name name = package ->
          lib_install_files ~dir lib
        | Executables ({ object_public_name = Some name; _ } as exes)
          when Findlib.root_package_name name = package ->
          obj_install_files ~dir exes
        | Install { section; files; package = Some p } when p = package ->
          List.map files ~f:(fun { Install_conf. src; dst } ->
            Install.Entry.make section (Path.relative dir src) ?dst)
        | _ -> [])
    in
    let entries =
      let files = sources_and_targets_known_so_far ~src_path:Path.root in
      String_set.fold files ~init:entries ~f:(fun fn acc ->
        if is_odig_doc_file fn then
          Install.Entry.make Doc (Path.relative ctx.build_dir fn) :: acc
        else
          acc)
    in
    let entries =
      let opam = Path.relative package_path (package ^ ".opam") in
      Install.Entry.make Lib opam ~dst:"opam" :: entries
    in
    let entries =
      (* Install a META file if the user wrote one or setup a rule to generate one, or if
         we have at least another file to install in the lib/ directory *)
      let meta_fn = "META." ^ package in
      if String_set.mem package packages_with_explicit_or_user_generated_meta ||
         List.exists entries ~f:(fun (e : Install.Entry.t) -> e.section = Lib) then
        let meta = Path.append ctx.build_dir (Path.relative package_path meta_fn) in
        Install.Entry.make Lib meta ~dst:"META" :: entries
      else
        entries
    in
    let fn =
      Path.relative (Path.append ctx.build_dir package_path) (package ^ ".install")
    in
    local_install_rules entries ~package;
    add_rule
      (Build.path_set (Install.files entries) >>>
       Build.create_file ~target:fn (fun () ->
         Install.write_install_file fn entries))

  let () = String_map.iter P.packages ~f:(fun ~key:_ ~data:pkg ->
    install_file pkg.Package.path pkg.name)

  let () =
    let is_default = Path.basename ctx.build_dir = "default" in
    String_map.iter P.packages ~f:(fun ~key:pkg ~data:{ Package.path = src_path; _ } ->
      let install_fn = pkg ^ ".install" in

      let ctx_path = Path.append ctx.build_dir src_path in
      let ctx_install_alias = Alias.install ~dir:ctx_path in
      let ctx_install_file = Path.relative ctx_path install_fn in
      Alias.add_deps ctx_install_alias [ctx_install_file];

      if is_default then begin
        let src_install_alias = Alias.install ~dir:src_path in
        let src_install_file = Path.relative src_path install_fn in
        add_rule (Build.copy ~src:ctx_install_file ~dst:src_install_file);
        Alias.add_deps src_install_alias [src_install_file]
      end)
end

let gen ~contexts ?(filter_out_optional_stanzas_with_missing_deps=true) conf =
  let open Future in
  let { Jbuild_load. file_tree; tree; jbuilds; packages } = conf in
  let alias_store = Alias.Store.create () in
  List.map contexts ~f:(fun context ->
    Jbuild_load.Jbuilds.eval ~context jbuilds >>| fun stanzas ->
    let module M =
      Gen(struct
        let context  = context
        let file_tree = file_tree
        let stanzas  = stanzas
        let packages = packages
        let filter_out_optional_stanzas_with_missing_deps =
          filter_out_optional_stanzas_with_missing_deps
        let alias_store = alias_store
      end)
    in
    !M.all_rules)
  |> Future.all
  >>| fun rules ->
  Alias.rules alias_store ~prefixes:(Path.root :: List.map contexts ~f:(fun c -> c.Context.build_dir)) ~tree
  @ List.concat rules

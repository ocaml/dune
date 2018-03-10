open Import
open Fiber.O

module Kind = struct
  module Opam = struct
    type t =
      { root   : string
      ; switch : string
      }
  end
  type t = Default | Opam of Opam.t

  let sexp_of_t : t -> Sexp.t = function
    | Default -> Sexp.unsafe_atom_of_string "default"
    | Opam o  ->
      Sexp.To_sexp.(record [ "root"  , string o.root
                           ; "switch", string o.switch
                           ])
end

type t =
  { name                    : string
  ; kind                    : Kind.t
  ; merlin                  : bool
  ; for_host                : t option
  ; implicit                : bool
  ; build_dir               : Path.t
  ; path                    : Path.t list
  ; toplevel_path           : Path.t option
  ; ocaml_bin               : Path.t
  ; ocaml                   : Path.t
  ; ocamlc                  : Path.t
  ; ocamlopt                : Path.t option
  ; ocamldep                : Path.t
  ; ocamlmklib              : Path.t
  ; env                     : string array
  ; env_extra               : string Env.Map.t
  ; findlib                 : Findlib.t
  ; findlib_toolchain       : string option
  ; arch_sixtyfour          : bool
  ; opam_var_cache          : (string, string) Hashtbl.t
  ; natdynlink_supported    : bool
  ; ocaml_config            : Ocaml_config.t
  ; version_string          : string
  ; version                 : int * int * int
  ; stdlib_dir              : Path.t
  ; ccomp_type              : string
  ; c_compiler              : string
  ; ocamlc_cflags           : string list
  ; ocamlopt_cflags         : string list
  ; bytecomp_c_libraries    : string list
  ; native_c_libraries      : string list
  ; cc_profile              : string list
  ; architecture            : string
  ; system                  : string
  ; ext_obj                 : string
  ; ext_asm                 : string
  ; ext_lib                 : string
  ; ext_dll                 : string
  ; ext_exe                 : string
  ; os_type                 : string
  ; default_executable_name : string
  ; host                    : string
  ; target                  : string
  ; flambda                 : bool
  ; exec_magic_number       : string
  ; cmi_magic_number        : string
  ; cmo_magic_number        : string
  ; cma_magic_number        : string
  ; cmx_magic_number        : string
  ; cmxa_magic_number       : string
  ; ast_impl_magic_number   : string
  ; ast_intf_magic_number   : string
  ; cmxs_magic_number       : string
  ; cmt_magic_number        : string
  ; which_cache             : (string, Path.t option) Hashtbl.t
  }

let sexp_of_t t =
  let open Sexp.To_sexp in
  let path = Path.sexp_of_t in
  record
    [ "name", string t.name
    ; "kind", Kind.sexp_of_t t.kind
    ; "merlin", bool t.merlin
    ; "for_host", option string (Option.map t.for_host ~f:(fun t -> t.name))
    ; "build_dir", path t.build_dir
    ; "toplevel_path", option path t.toplevel_path
    ; "ocaml_bin", path t.ocaml_bin
    ; "ocaml", path t.ocaml
    ; "ocamlc", path t.ocamlc
    ; "ocamlopt", option path t.ocamlopt
    ; "ocamldep", path t.ocamldep
    ; "ocamlmklib", path t.ocamlmklib
    ; "env", list (pair string string) (Env.Map.to_list t.env_extra)
    ; "findlib_path", list path (Findlib.path t.findlib)
    ; "arch_sixtyfour", bool t.arch_sixtyfour
    ; "natdynlink_supported", bool t.natdynlink_supported
    ; "opam_vars", string_hashtbl string t.opam_var_cache
    ; "ocaml_config", Ocaml_config.sexp_of_t t.ocaml_config
    ; "which", string_hashtbl (option path) t.which_cache
    ]

let compare a b = compare a.name b.name

let opam_config_var ~env ~cache var =
  match Hashtbl.find cache var with
  | Some _ as x -> Fiber.return x
  | None ->
    match Bin.opam with
    | None -> Fiber.return None
    | Some fn ->
      Process.run_capture (Accept All) (Path.to_string fn) ~env
        ["config"; "var"; var]
      >>| function
      | Ok s ->
        let s = String.trim s in
        Hashtbl.add cache var s;
        Some s
      | Error _ -> None

let which ~cache ~path x =
  Hashtbl.find_or_add cache x ~f:(Bin.which ~path)

let create ~(kind : Kind.t) ~path ~base_env ~env_extra ~name ~merlin
      ~targets () =
  let env = Env.extend_env ~env:base_env ~vars:env_extra in
  let opam_var_cache = Hashtbl.create 128 in
  (match kind with
   | Opam { root; _ } ->
     Hashtbl.add opam_var_cache "root" root
   | Default -> ());
  let prog_not_found_in_path prog =
    Utils.program_not_found prog ~context:name
  in
  let which_cache = Hashtbl.create 128 in
  let which x = which ~cache:which_cache ~path x in
  let findlib_config_path = lazy (
    match which "ocamlfind" with
    | None -> prog_not_found_in_path "ocamlfind"
    | Some fn ->
      (* When OCAMLFIND_CONF is set, "ocamlfind printconf" does print the contents of the
         variable, but "ocamlfind printconf conf" still prints the configuration file set
         at the configuration time of ocamlfind, sigh... *)
      match Sys.getenv "OCAMLFIND_CONF" with
      | s -> Fiber.return (Path.absolute s)
      | exception Not_found ->
        Process.run_capture_line ~env Strict
          (Path.to_string fn) ["printconf"; "conf"]
        >>| Path.absolute)
  in

  let create_one ~name ~implicit ?findlib_toolchain ?host ~merlin () =
    (match findlib_toolchain with
     | None -> Fiber.return None
     | Some toolchain ->
       Lazy.force findlib_config_path >>| fun path ->
       Some (Findlib.Config.load path ~toolchain ~context:name))
    >>= fun findlib_config ->

    let get_tool_using_findlib_config prog =
      Option.bind findlib_config ~f:(fun conf ->
        match Findlib.Config.get conf prog with
        | None -> None
        | Some s ->
          match Filename.analyze_program_name s with
          | In_path | Relative_to_current_dir -> which s
          | Absolute -> Some (Path.absolute s))
    in

    let ocamlc =
      match get_tool_using_findlib_config "ocamlc" with
      | Some x -> x
      | None ->
        match which "ocamlc" with
        | Some x -> x
        | None -> prog_not_found_in_path "ocamlc"
    in
    let dir = Path.parent ocamlc in
    let ocaml_tool_not_found prog =
      die "ocamlc found in %s, but %s/%s doesn't exist (context: %s)"
        (Path.to_string dir) (Path.to_string dir) prog name
    in
    let get_ocaml_tool prog =
      match get_tool_using_findlib_config prog with
      | None -> Bin.best_prog dir prog
      | Some _ as x -> x
    in
    let get_ocaml_tool_exn prog =
      match get_ocaml_tool prog with
      | None -> ocaml_tool_not_found prog
      | Some fn -> fn
    in

    let build_dir = Path.of_string (sprintf "_build/%s" name) in
    let findlib_path () =
      match kind, findlib_toolchain, Setup.library_path with
      | Default, None, Some l ->
        Fiber.return (List.map l ~f:Path.absolute)
      | _ ->
        (* If ocamlfind is present, it has precedence over everything else. *)
        match which "ocamlfind" with
        | Some fn ->
          let args =
            let args = ["printconf"; "path"] in
            match findlib_toolchain with
            | None -> args
            | Some s -> "-toolchain" :: s :: args
          in
          Process.run_capture_lines ~env Strict (Path.to_string fn) args
          >>| List.map ~f:Path.absolute
        | None ->
          (* If there no ocamlfind in the PATH, check if we have opam
             and assume a standard opam setup *)
          opam_config_var ~env ~cache:opam_var_cache "lib"
          >>| function
          | Some s -> [Path.absolute s]
          | None ->
            (* If neither opam neither ocamlfind are present, assume
               that libraries are [dir ^ "/../lib"] *)
            [Path.relative (Path.parent dir) "lib"]
    in
    let ocaml_config_ok_exn = function
      | Ok x -> x
      | Error msg ->
        die "Failed to parse the output of '%s -config':@\n\
             %s"
          (Path.to_string ocamlc) msg
    in
    Fiber.fork_and_join
      findlib_path
      (fun () ->
         Process.run_capture_lines ~env Strict
           (Path.to_string ocamlc) ["-config"]
         >>| fun lines ->
         let open Result.O in
         ocaml_config_ok_exn
           (Ocaml_config.Vars.of_lines lines >>= Ocaml_config.make))
    >>= fun (findlib_path, ocfg) ->
    let version = Ocaml_config.version ocfg in
    let env, env_extra =
      (* See comment in ansi_color.ml for setup_env_for_colors. For
         OCaml < 4.05, OCAML_COLOR is not supported so we use
         OCAMLPARAM. OCaml 4.02 doesn't support 'color' in OCAMLPARAM,
         so we just don't force colors with 4.02. *)
      if !Clflags.capture_outputs
      && Lazy.force Colors.stderr_supports_colors
      && version >= (4, 03, 0)
      && version <  (4, 05, 0) then
        let value =
          match Env.get_env env "OCAMLPARAM" with
          | None -> "color=always,_"
          | Some s -> "color=always," ^ s
        in
        Env.extend_env ~env ~vars:((Env.Map.singleton "OCAMLPARAM" value)),
        (Env.Map.add env_extra "OCAMLPARAM" value)
      else
        env,env_extra
    in
    let stdlib_dir = Path.of_string (Ocaml_config.standard_library ocfg) in
    let natdynlink_supported = Ocaml_config.natdynlink_supported ocfg in
    let version        = Ocaml_config.version ocfg        in
    let version_string = Ocaml_config.version_string ocfg in
    let arch_sixtyfour = Ocaml_config.word_size ocfg = 64 in
    Fiber.return
      { name
      ; implicit
      ; kind
      ; merlin
      ; for_host = host
      ; build_dir
      ; path
      ; toplevel_path = Option.map (Env.get_env env "OCAML_TOPLEVEL_PATH") ~f:Path.absolute

      ; ocaml_bin  = dir
      ; ocaml      = (match which "ocaml" with Some p -> p | None -> prog_not_found_in_path "ocaml")
      ; ocamlc
      ; ocamlopt   = get_ocaml_tool     "ocamlopt"
      ; ocamldep   = get_ocaml_tool_exn "ocamldep"
      ; ocamlmklib = get_ocaml_tool_exn "ocamlmklib"

      ; env
      ; env_extra
      ; findlib = Findlib.create ~stdlib_dir ~path:findlib_path
      ; findlib_toolchain
      ; arch_sixtyfour

      ; opam_var_cache

      ; natdynlink_supported

      ; stdlib_dir
      ; ocaml_config = ocfg
      ; version_string
      ; version
      ; ccomp_type              = Ocaml_config.ccomp_type              ocfg
      ; c_compiler              = Ocaml_config.c_compiler              ocfg
      ; ocamlc_cflags           = Ocaml_config.ocamlc_cflags           ocfg
      ; ocamlopt_cflags         = Ocaml_config.ocamlopt_cflags         ocfg
      ; bytecomp_c_libraries    = Ocaml_config.bytecomp_c_libraries    ocfg
      ; native_c_libraries      = Ocaml_config.native_c_libraries      ocfg
      ; cc_profile              = Ocaml_config.cc_profile              ocfg
      ; architecture            = Ocaml_config.architecture            ocfg
      ; system                  = Ocaml_config.system                  ocfg
      ; ext_obj                 = Ocaml_config.ext_obj                 ocfg
      ; ext_asm                 = Ocaml_config.ext_asm                 ocfg
      ; ext_lib                 = Ocaml_config.ext_lib                 ocfg
      ; ext_dll                 = Ocaml_config.ext_dll                 ocfg
      ; ext_exe                 = Ocaml_config.ext_exe                 ocfg
      ; os_type                 = Ocaml_config.os_type                 ocfg
      ; default_executable_name = Ocaml_config.default_executable_name ocfg
      ; host                    = Ocaml_config.host                    ocfg
      ; target                  = Ocaml_config.target                  ocfg
      ; flambda                 = Ocaml_config.flambda                 ocfg
      ; exec_magic_number       = Ocaml_config.exec_magic_number       ocfg
      ; cmi_magic_number        = Ocaml_config.cmi_magic_number        ocfg
      ; cmo_magic_number        = Ocaml_config.cmo_magic_number        ocfg
      ; cma_magic_number        = Ocaml_config.cma_magic_number        ocfg
      ; cmx_magic_number        = Ocaml_config.cmx_magic_number        ocfg
      ; cmxa_magic_number       = Ocaml_config.cmxa_magic_number       ocfg
      ; ast_impl_magic_number   = Ocaml_config.ast_impl_magic_number   ocfg
      ; ast_intf_magic_number   = Ocaml_config.ast_intf_magic_number   ocfg
      ; cmxs_magic_number       = Ocaml_config.cmxs_magic_number       ocfg
      ; cmt_magic_number        = Ocaml_config.cmt_magic_number        ocfg

      ; which_cache
      }
  in

  let implicit = not (List.mem ~set:targets Workspace.Context.Target.Native) in
  create_one () ~implicit ~name ~merlin >>= fun native ->
  Fiber.parallel_map targets ~f:(function
    | Native -> Fiber.return None
    | Named findlib_toolchain ->
      let name = sprintf "%s.%s" name findlib_toolchain in
      create_one () ~implicit:false ~name ~findlib_toolchain ~host:native
        ~merlin:false
      >>| fun x -> Some x)
  >>| fun others ->
  native :: List.filter_map others ~f:(fun x -> x)

let opam_config_var t var = opam_config_var ~env:t.env ~cache:t.opam_var_cache var

let default ?(merlin=true) ~targets () =
  let env = Lazy.force Env.initial_env in
  let path =
    match Env.get_env env "PATH" with
    | Some s -> Bin.parse_path s
    | None -> []
  in
  create ~kind:Default ~path ~base_env:env ~env_extra:Env.Map.empty
    ~name:"default" ~merlin ~targets ()

let create_for_opam ?root ~targets ~switch ~name ?(merlin=false) () =
  match Bin.opam with
  | None -> Utils.program_not_found "opam"
  | Some fn ->
    (match root with
     | Some root -> Fiber.return root
     | None ->
       Process.run_capture_line Strict (Path.to_string fn) ["config"; "var"; "root"])
    >>= fun root ->
    Process.run_capture Strict (Path.to_string fn)
      ["config"; "env"; "--root"; root; "--switch"; switch; "--sexp"]
    >>= fun s ->
    let vars =
      Usexp.parse_string ~fname:"<opam output>" ~mode:Single s
      |> Sexp.Of_sexp.(list (pair string string))
      |> Env.Map.of_list_multi
      |> Env.Map.mapi ~f:(fun var values ->
        match List.rev values with
        | [] -> assert false
        | [x] -> x
        | x :: _ ->
          Format.eprintf
            "@{<warning>Warning@}: variable %S present multiple times in the output of:\n\
             @{<details>%s@}@."
            var
            (String.concat ~sep:" "
               (List.map ~f:quote_for_shell
                  [Path.to_string fn; "config"; "env"; "--root"; root;
                   "--switch"; switch; "--sexp"]));
          x)
    in
    let path =
      match Env.Map.find vars "PATH" with
      | None -> Bin.path
      | Some s -> Bin.parse_path s
    in
    let env = Lazy.force Env.initial_env in
    create ~kind:(Opam { root; switch }) ~targets
      ~path ~base_env:env ~env_extra:vars ~name ~merlin ()

let create ?merlin def =
  match (def : Workspace.Context.t) with
  | Default targets -> default ~targets ?merlin ()
  | Opam { name; switch; root; targets; _ } ->
    create_for_opam ?root ~switch ~name ?merlin ~targets ()

let which t s = which ~cache:t.which_cache ~path:t.path s

let install_prefix t =
  opam_config_var t "prefix" >>| function
  | Some x -> Path.absolute x
  | None   -> Path.parent t.ocaml_bin

let install_ocaml_libdir t =
  match t.kind, t.findlib_toolchain, Setup.library_destdir with
  | Default, None, Some d ->
    Fiber.return (Some (Path.absolute d))
  | _ ->
    (* If ocamlfind is present, it has precedence over everything else. *)
    match which t "ocamlfind" with
    | Some fn ->
      (Process.run_capture_line ~env:t.env Strict
         (Path.to_string fn) ["printconf"; "destdir"]
       >>| fun s ->
       Some (Path.absolute s))
    | None ->
      Fiber.return None

(* CR-someday jdimino: maybe we should just do this for [t.env] directly? *)
let env_for_exec t =
  let sep = if Sys.win32 then ';' else ':' in
  let cwd = Sys.getcwd () in
  let extend_var var v =
    let v = Filename.concat cwd (Path.to_string v) in
    match Env.get_env t.env var with
    | None -> (var, v)
    | Some prev -> (var, sprintf "%s%c%s" v sep prev)
  in
  let vars =
    [ extend_var "CAML_LD_LIBRARY_PATH"
        (Path.relative
           (Config.local_install_dir ~context:t.name)
           "lib/stublibs")
    ; extend_var "OCAMLPATH"
        (Path.relative
           (Config.local_install_dir ~context:t.name)
           "lib")
    ; extend_var "PATH"
        (Config.local_install_bin_dir ~context:t.name)
    ; extend_var "MANPATH"
        (Config.local_install_man_dir ~context:t.name)
    ]
  in
  Env.extend_env ~env:t.env ~vars:(Env.Map.of_list_exn vars)

let compiler t (mode : Mode.t) =
  match mode with
  | Byte   -> Some t.ocamlc
  | Native -> t.ocamlopt

let best_mode t : Mode.t =
  match t.ocamlopt with
  | Some _ -> Native
  | None   -> Byte

let cc_g (ctx : t) =
  if !Clflags.g && ctx.ccomp_type <> "msvc" then
    ["-g"]
  else
    []

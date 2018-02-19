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
    | Default -> Atom "default"
    | Opam o  ->
      Sexp.To_sexp.(record [ "root"  , atom o.root
                           ; "switch", atom o.switch
                           ])
end

module Env_var = struct
  type t = string
  let compare a b =
    if Sys.win32 then
      String.compare (String.lowercase_ascii a) (String.lowercase_ascii b)
    else
      String.compare a b
end

module Env_var_map = Map.Make(Env_var)

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
  ; env_extra               : string Env_var_map.t
  ; findlib                 : Findlib.t
  ; findlib_toolchain       : string option
  ; arch_sixtyfour          : bool
  ; opam_var_cache          : (string, string) Hashtbl.t
  ; natdynlink_supported    : bool
  ; ocamlc_config           : Ocamlc_config.t
  ; version_string          : string
  ; version                 : int * int * int
  ; stdlib_dir              : Path.t
  ; ccomp_type              : string
  ; c_compiler              : string
  ; ocamlc_cflags           : string
  ; ocamlopt_cflags         : string
  ; bytecomp_c_libraries    : string
  ; native_c_libraries      : string
  ; native_pack_linker      : string
  ; ranlib                  : string
  ; cc_profile              : string
  ; architecture            : string
  ; system                  : string
  ; ext_obj                 : string
  ; ext_asm                 : string
  ; ext_lib                 : string
  ; ext_dll                 : string
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
    [ "name", atom t.name
    ; "kind", Kind.sexp_of_t t.kind
    ; "merlin", bool t.merlin
    ; "for_host", option atom (Option.map t.for_host ~f:(fun t -> t.name))
    ; "build_dir", path t.build_dir
    ; "toplevel_path", option path t.toplevel_path
    ; "ocaml_bin", path t.ocaml_bin
    ; "ocaml", path t.ocaml
    ; "ocamlc", path t.ocamlc
    ; "ocamlopt", option path t.ocamlopt
    ; "ocamldep", path t.ocamldep
    ; "ocamlmklib", path t.ocamlmklib
    ; "env", list (pair atom atom) (Env_var_map.bindings t.env_extra)
    ; "findlib_path", list path (Findlib.path t.findlib)
    ; "arch_sixtyfour", bool t.arch_sixtyfour
    ; "natdynlink_supported", bool t.natdynlink_supported
    ; "opam_vars", atom_hashtbl atom t.opam_var_cache
    ; "ocamlc_config", Ocamlc_config.sexp_of_t t.ocamlc_config
    ; "which", atom_hashtbl (option path) t.which_cache
    ]

let compare a b = compare a.name b.name

let get_arch_sixtyfour stdlib_dir =
  let files = ["caml/config.h"; "caml/m.h"] in
  let get_arch_sixtyfour_from file =
    let file_path = Path.to_string (Path.relative stdlib_dir file) in
    if Sys.file_exists file_path then begin
      List.exists (Io.lines_of_file file_path) ~f:(fun line ->
        match String.extract_blank_separated_words line with
        | ["#define"; "ARCH_SIXTYFOUR"] -> true
        | _ -> false)
    end else
      false
  in
  List.exists ~f:get_arch_sixtyfour_from files

let opam_config_var ~env ~cache var =
  match Hashtbl.find cache var with
  | Some _ as x -> Fiber.return x
  | None ->
    match Bin.opam with
    | None -> Fiber.return None
    | Some fn ->
      Process.run_capture (Accept All) (Path.to_string fn) ~env ["config"; "var"; var]
      >>| function
      | Ok s ->
        let s = String.trim s in
        Hashtbl.add cache ~key:var ~data:s;
        Some s
      | Error _ -> None

let get_env env var =
  let rec loop i =
    if i = Array.length env then
      None
    else
      let entry = env.(i) in
      match String.lsplit2 entry ~on:'=' with
      | Some (key, value) when Env_var.compare key var = 0 ->
        Some value
      | _ -> loop (i + 1)
  in
  loop 0

let which ~cache ~path x =
  Hashtbl.find_or_add cache x ~f:(Bin.which ~path)

let extend_env ~vars ~env =
  if Env_var_map.is_empty vars then
    env
  else
    let imported =
      Array.to_list env
      |> List.filter ~f:(fun s ->
        match String.index s '=' with
        | None -> true
        | Some i ->
          let key = String.sub s ~pos:0 ~len:i in
          not (Env_var_map.mem key vars))
    in
    List.rev_append
      (List.map (Env_var_map.bindings vars) ~f:(fun (k, v) -> sprintf "%s=%s" k v))
      imported
    |> Array.of_list

let create ~(kind : Kind.t) ~path ~base_env ~env_extra ~name ~merlin
      ~use_findlib ~targets () =
  let env = extend_env ~env:base_env ~vars:env_extra in
  let opam_var_cache = Hashtbl.create 128 in
  (match kind with
   | Opam { root; _ } ->
     Hashtbl.add opam_var_cache ~key:"root" ~data:root
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
        | "" -> None
        | s ->
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
      if use_findlib then
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
            (* If neither opam neither ocamlfind are present, assume that libraries are
               [dir ^ "/../lib"] *)
            [Path.relative (Path.parent dir) "lib"]
      else
        Fiber.return []
    in
    Fiber.fork_and_join
      findlib_path
      (fun () -> Ocamlc_config.read ~ocamlc ~env)
    >>= fun (findlib_path, ocamlc_config) ->
    let version = Ocamlc_config.version ocamlc_config in
    let env, env_extra =
      (* See comment in ansi_color.ml for setup_env_for_colors. For OCaml < 4.05,
         OCAML_COLOR is not supported so we use OCAMLPARAM. OCaml 4.02 doesn't support
         'color' in OCAMLPARAM, so we just don't force colors with 4.02. *)
      if !Clflags.capture_outputs
      && Lazy.force Ansi_color.stderr_supports_colors
      && version >= (4, 03, 0)
      && version <  (4, 05, 0) then
        let value =
          match get_env env "OCAMLPARAM" with
          | None -> "color=always,_"
          | Some s -> "color=always," ^ s
        in
        extend_env ~env ~vars:((Env_var_map.singleton "OCAMLPARAM" value)),
        (Env_var_map.add ~key:"OCAMLPARAM" ~data:value env_extra)
      else
        env,env_extra
    in
    let stdlib_dir = Ocamlc_config.stdlib_dir ocamlc_config in
    let natdynlink_supported = Ocamlc_config.natdynlink_supported ocamlc_config in
    let version = Ocamlc_config.version ocamlc_config in
    let version_string = Ocamlc_config.version_string ocamlc_config in
    let get = Ocamlc_config.get ocamlc_config in
    let c_compiler, ocamlc_cflags, ocamlopt_cflags =
      Ocamlc_config.c_compiler_settings ocamlc_config in
    let arch_sixtyfour =
      match Ocamlc_config.word_size ocamlc_config with
      | Some ws -> ws = "64"
      | None -> get_arch_sixtyfour stdlib_dir
    in
    Fiber.return
      { name
      ; implicit
      ; kind
      ; merlin
      ; for_host = host
      ; build_dir
      ; path
      ; toplevel_path = Option.map (get_env env "OCAML_TOPLEVEL_PATH") ~f:Path.absolute

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
      ; ocamlc_config
      ; version_string
      ; version
      ; ccomp_type              = get       "ccomp_type"
      ; c_compiler
      ; ocamlc_cflags
      ; ocamlopt_cflags
      ; bytecomp_c_libraries    = get       "bytecomp_c_libraries"
      ; native_c_libraries      = get       "native_c_libraries"
      ; native_pack_linker      = get       "native_pack_linker"
      ; ranlib                  = get       "ranlib"
      ; cc_profile              = get       "cc_profile"
      ; architecture            = get       "architecture"
      ; system                  = get       "system"
      ; ext_obj                 = get       "ext_obj"
      ; ext_asm                 = get       "ext_asm"
      ; ext_lib                 = get       "ext_lib"
      ; ext_dll                 = get       "ext_dll"
      ; os_type                 = get       "os_type"
      ; default_executable_name = get       "default_executable_name"
      ; host                    = get       "host"
      ; target                  = get       "target"
      ; flambda                 = Ocamlc_config.flambda ocamlc_config
      ; exec_magic_number       = get       "exec_magic_number"
      ; cmi_magic_number        = get       "cmi_magic_number"
      ; cmo_magic_number        = get       "cmo_magic_number"
      ; cma_magic_number        = get       "cma_magic_number"
      ; cmx_magic_number        = get       "cmx_magic_number"
      ; cmxa_magic_number       = get       "cmxa_magic_number"
      ; ast_impl_magic_number   = get       "ast_impl_magic_number"
      ; ast_intf_magic_number   = get       "ast_intf_magic_number"
      ; cmxs_magic_number       = get       "cmxs_magic_number"
      ; cmt_magic_number        = get       "cmt_magic_number"

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

let initial_env = lazy (
  Lazy.force Ansi_color.setup_env_for_colors;
  Unix.environment ())

let default ?(merlin=true) ?(use_findlib=true) ~targets () =
  let env = Lazy.force initial_env in
  let path =
    match get_env env "PATH" with
    | Some s -> Bin.parse_path s
    | None -> []
  in
  create ~kind:Default ~path ~base_env:env ~env_extra:Env_var_map.empty
    ~name:"default" ~merlin ~use_findlib ~targets ()

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
      |> Env_var_map.of_alist_multi
      |> Env_var_map.mapi ~f:(fun var values ->
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
      match Env_var_map.find "PATH" vars with
      | None -> Bin.path
      | Some s -> Bin.parse_path s
    in
    let env = Lazy.force initial_env in
    create ~kind:(Opam { root; switch }) ~targets
      ~path ~base_env:env ~env_extra:vars ~name ~merlin ~use_findlib:true ()

let create ?use_findlib ?merlin def =
  match (def : Workspace.Context.t) with
  | Default targets -> default ~targets ?merlin ?use_findlib ()
  | Opam { name; switch; root; targets; _ } ->
    create_for_opam ?root ~switch ~name ?merlin ~targets ()

let which t s = which ~cache:t.which_cache ~path:t.path s

let install_prefix t =
  opam_config_var t "prefix" >>| function
  | Some x -> Path.absolute x
  | None   -> Path.parent t.ocaml_bin

let install_ocaml_libdir t =
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
    match get_env t.env var with
    | None -> (var, v)
    | Some prev -> (var, sprintf "%s%c%s" v sep prev)
  in
  let vars =
    [ extend_var "CAML_LD_LIBRARY_PATH" (Path.relative
                                           (Config.local_install_dir ~context:t.name)
                                           "lib/stublibs")
    ; extend_var "OCAMLPATH"            (Path.relative
                                           (Config.local_install_dir ~context:t.name)
                                           "lib")
    ; extend_var "PATH"                 (Config.local_install_bin_dir ~context:t.name)
    ; extend_var "MANPATH"              (Config.local_install_man_dir ~context:t.name)
    ]
  in
  extend_env ~env:t.env ~vars:(Env_var_map.of_alist_exn vars)

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

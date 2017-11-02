open Import
open Future

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
      Sexp.To_sexp.(record [ "root"  , string o.root
                           ; "switch", string o.switch
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
  ; arch_sixtyfour          : bool
  ; opam_var_cache          : (string, string) Hashtbl.t
  ; natdynlink_supported    : bool
  ; ocamlc_config           : (string * string) list
  ; version                 : string
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
    ; "env", list (pair string string) (Env_var_map.bindings t.env_extra)
    ; "findlib_path", list path (Findlib.path t.findlib)
    ; "arch_sixtyfour", bool t.arch_sixtyfour
    ; "natdynlink_supported", bool t.natdynlink_supported
    ; "opam_vars", string_hashtbl string t.opam_var_cache
    ; "ocamlc_config", list (pair string string) t.ocamlc_config
    ; "which", string_hashtbl (option path) t.which_cache
    ]

let compare a b = compare a.name b.name

let get_arch_sixtyfour stdlib_dir =
  let files = ["caml/config.h"; "caml/m.h"] in
  let get_arch_sixtyfour_from file =
    let config_h = Path.relative stdlib_dir file in
    List.exists (Io.lines_of_file (Path.to_string config_h)) ~f:(fun line ->
      match String.extract_blank_separated_words line with
      | ["#define"; "ARCH_SIXTYFOUR"] -> true
      | _ -> false)
  in
  List.exists ~f:get_arch_sixtyfour_from files

let opam_config_var ~env ~cache var =
  match Hashtbl.find cache var with
  | Some _ as x -> return x
  | None ->
    match Bin.opam with
    | None -> return None
    | Some fn ->
      Future.run_capture (Accept All) (Path.to_string fn) ~env ["config"; "var"; var]
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

let create ~(kind : Kind.t) ~path ~base_env ~env_extra ~name ~merlin ~use_findlib =
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
  let ocamlc =
    match which "ocamlc" with
    | None -> prog_not_found_in_path "ocamlc"
    | Some x -> x
  in
  let dir = Path.parent ocamlc in
  let prog_not_found prog =
    die "ocamlc found in %s, but %s/%s doesn't exist (context: %s)"
      (Path.to_string dir) (Path.to_string dir) prog name
  in
  let best_prog prog = Bin.best_prog dir prog in
  let get_prog prog =
    match best_prog prog with
    | None -> prog_not_found prog
    | Some fn -> fn
  in
  let build_dir =
    Path.of_string (sprintf "_build/%s" name)
  in
  let ocamlc_config_cmd = sprintf "%s -config" (Path.to_string ocamlc) in
  let findlib_path =
    if use_findlib then
      (* If ocamlfind is present, it has precedence over everything else. *)
      match which "ocamlfind" with
      | Some fn ->
        (Future.run_capture_lines ~env Strict
           (Path.to_string fn) ["printconf"; "path"]
         >>| List.map ~f:Path.absolute)
      | None ->
        (* If there no ocamlfind in the PATH, check if we have opam and assume a stan opam
           setup *)
        opam_config_var ~env ~cache:opam_var_cache "lib"
        >>| function
        | Some s -> [Path.absolute s]
        | None ->
          (* If neither opam neither ocamlfind are present, assume that libraries are
             [dir ^ "/../lib"] *)
          [Path.relative (Path.parent dir) "lib"]
    else
      return []
  in
  both
    findlib_path
    (Future.run_capture_lines ~env Strict (Path.to_string ocamlc) ["-config"])
  >>= fun (findlib_path, ocamlc_config) ->
  let ocamlc_config =
    List.map ocamlc_config ~f:(fun line ->
      match String.index line ':' with
      | Some i ->
        (String.sub line ~pos:0 ~len:i,
         String.sub line ~pos:(i + 2) ~len:(String.length line - i - 2))
      | None ->
        die "unrecognized line in the output of `%s`: %s" ocamlc_config_cmd
          line)
    |> String_map.of_alist
    |> function
    | Ok x -> x
    | Error (key, _, _) ->
      die "variable %S present twice in the output of `%s`" key ocamlc_config_cmd
  in
  let get_opt var = String_map.find var ocamlc_config in
  let get ?default var =
    match get_opt var with
    | Some s -> s
    | None ->
      match default with
      | Some x -> x
      | None ->
        die "variable %S not found in the output of `%s`" var ocamlc_config_cmd
  in
  let get_bool ?default var =
    match get ?default:(Option.map default ~f:string_of_bool) var with
    | "true" -> true
    | "false" -> false
    | _ -> die "variable %S is neither 'true' neither 'false' in the output of `%s`"
             var ocamlc_config_cmd
  in
  let get_path var = Path.absolute (get var) in
  let stdlib_dir = get_path "standard_library" in
  let natdynlink_supported = Path.exists (Path.relative stdlib_dir "dynlink.cmxa") in
  let version = get "version" in
  let env,env_extra =
    (* See comment in ansi_color.ml for setup_env_for_colors. For OCaml < 4.05,
       OCAML_COLOR is not supported so we use OCAMLPARAM. OCaml 4.02 doesn't support
       'color' in OCAMLPARAM, so we just don't force colors with 4.02. *)
    let ocaml_version = Scanf.sscanf version "%u.%u" (fun a b -> a, b) in
    if !Clflags.capture_outputs
    && Lazy.force Ansi_color.stderr_supports_colors
    && ocaml_version > (4, 02)
    && ocaml_version < (4, 05) then
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
  let c_compiler, ocamlc_cflags, ocamlopt_cflags =
    match get_opt "c_compiler" with
    | Some c_compiler -> (* >= 4.06 *)
      (c_compiler, get "ocamlc_cflags", get "ocamlopt_cflags")
    | None ->
      let split_prog s =
        let len = String.length s in
        let rec loop i =
          if i = len then
            (s, "")
          else
            match s.[i] with
            | ' ' | '\t' ->
              (String.sub s ~pos:0 ~len:i,
               String.sub s ~pos:i ~len:(len - i))
            | _ -> loop (i + 1)
        in
        loop 0
      in
      let c_compiler, ocamlc_cflags = split_prog (get "bytecomp_c_compiler") in
      let _, ocamlopt_cflags = split_prog (get "native_c_compiler") in
      (c_compiler, ocamlc_cflags, ocamlopt_cflags)
  in
  return
    { name
    ; kind
    ; merlin
    ; for_host = None
    ; build_dir
    ; path
    ; toplevel_path = Option.map (get_env env "OCAML_TOPLEVEL_PATH") ~f:Path.absolute

    ; ocaml_bin  = dir
    ; ocaml      = Path.relative dir ("ocaml" ^ Bin.exe)
    ; ocamlc
    ; ocamlopt   = best_prog "ocamlopt"
    ; ocamldep   = get_prog  "ocamldep"
    ; ocamlmklib = get_prog  "ocamlmklib"

    ; env
    ; env_extra
    ; findlib = Findlib.create ~stdlib_dir ~path:findlib_path
    ; arch_sixtyfour = get_arch_sixtyfour stdlib_dir

    ; opam_var_cache

    ; natdynlink_supported

    ; stdlib_dir
    ; ocamlc_config = String_map.bindings ocamlc_config
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
    ; flambda                 = get_bool  "flambda" ~default:false
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

let opam_config_var t var = opam_config_var ~env:t.env ~cache:t.opam_var_cache var

let initial_env = lazy (
  Lazy.force Ansi_color.setup_env_for_colors;
  Unix.environment ())

let default ?(merlin=true) ?(use_findlib=true) () =
  let env = Lazy.force initial_env in
  let path =
    match get_env env "PATH" with
    | Some s -> Bin.parse_path s
    | None -> []
  in
  create ~kind:Default ~path ~base_env:env ~env_extra:Env_var_map.empty
    ~name:"default" ~merlin ~use_findlib

let create_for_opam ?root ~switch ~name ?(merlin=false) () =
  match Bin.opam with
  | None -> Utils.program_not_found "opam"
  | Some fn ->
    (match root with
     | Some root -> return root
     | None ->
       Future.run_capture_line Strict (Path.to_string fn) ["config"; "var"; "root"])
    >>= fun root ->
    Future.run_capture Strict (Path.to_string fn)
      ["config"; "env"; "--root"; root; "--switch"; switch; "--sexp"]
    >>= fun s ->
    let vars =
      Sexp_lexer.single (Lexing.from_string s)
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
    create ~kind:(Opam { root; switch }) ~path ~base_env:env ~env_extra:vars
      ~name ~merlin ~use_findlib:true

let which t s = which ~cache:t.which_cache ~path:t.path s

let install_prefix t =
  opam_config_var t "prefix" >>| function
  | Some x -> Path.absolute x
  | None   -> Path.parent t.ocaml_bin

let install_ocaml_libdir t =
  (* If ocamlfind is present, it has precedence over everything else. *)
  match which t "ocamlfind" with
  | Some fn ->
    (Future.run_capture_line ~env:t.env Strict
       (Path.to_string fn) ["printconf"; "destdir"]
     >>| fun s ->
     Some (Path.absolute s))
  | None ->
    return None

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

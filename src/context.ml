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
  ; ocamllex                : Path.t
  ; ocamlyacc               : Path.t
  ; ocamlmklib              : Path.t
  ; env                     : string array
  ; env_extra               : string String_map.t
  ; findlib                 : Findlib.t
  ; arch_sixtyfour          : bool
  ; opam_var_cache          : (string, string) Hashtbl.t
  ; natdynlink_supported    : bool
  ; ocamlc_config           : (string * string) list
  ; version                 : string
  ; stdlib_dir              : Path.t
  ; ccomp_type              : string
  ; bytecomp_c_compiler     : string
  ; bytecomp_c_libraries    : string
  ; native_c_compiler       : string
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
    ; "ocamllex", path t.ocamllex
    ; "ocamlyacc", path t.ocamlyacc
    ; "ocamlmklib", path t.ocamlmklib
    ; "env", string_map string t.env_extra
    ; "findlib_path", list path (Findlib.path t.findlib)
    ; "arch_sixtyfour", bool t.arch_sixtyfour
    ; "natdynlink_supported", bool t.natdynlink_supported
    ; "opam_vars", string_hashtbl string t.opam_var_cache
    ; "ocamlc_config", list (pair string string) t.ocamlc_config
    ; "which", string_hashtbl (option path) t.which_cache
    ]

let compare a b = compare a.name b.name

let get_arch_sixtyfour stdlib_dir =
  let config_h = Path.relative stdlib_dir "caml/config.h" in
  List.exists (lines_of_file (Path.to_string config_h)) ~f:(fun line ->
    match String.split_words line with
    | ["#define"; "ARCH_SIXTYFOUR"] -> true
    | _ -> false)

let opam_config_var ~env ~cache var =
  match Hashtbl.find cache var with
  | Some _ as x -> return x
  | None ->
    match Bin.opam with
    | None -> return None
    | Some fn ->
      Future.run_capture Strict (Path.to_string fn) ~env ["config"; "var"; var]
      >>| fun s ->
      let s = String.trim s in
      Hashtbl.add cache ~key:var ~data:s;
      Some s

let get_env env var =
  let prefix = var ^ "=" in
  let rec loop i =
    if i = Array.length env then
      None
    else
      let entry = env.(i) in
      if String.is_prefix entry ~prefix then
        let len_p = String.length prefix in
        Some (String.sub entry ~pos:len_p ~len:(String.length entry - len_p))
      else
        loop (i + 1)
  in
  loop 0

let which ~cache ~path x =
  Hashtbl.find_or_add cache x ~f:(Bin.which ~path)

let extend_env ~vars ~env =
  if String_map.is_empty vars then
    env
  else
    let imported =
      Array.to_list env
      |> List.filter ~f:(fun s ->
        match String.index s '=' with
        | None -> true
        | Some i ->
          let key = String.sub s ~pos:0 ~len:i in
          not (String_map.mem key vars))
    in
    List.rev_append
      (List.map (String_map.bindings vars) ~f:(fun (k, v) -> sprintf "%s=%s" k v))
      imported
    |> Array.of_list

let create ~(kind : Kind.t) ~path ~base_env ~env_extra ~name ~merlin =
  let env = extend_env ~env:base_env ~vars:env_extra in
  let opam_var_cache = Hashtbl.create 128 in
  (match kind with
   | Opam { root; _ } ->
     Hashtbl.add opam_var_cache ~key:"root" ~data:root
   | Default -> ());
  let prog_not_found_in_path prog =
    die "Program %s not found in PATH (context: %s)" prog name
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
  both
    (both
       (opam_config_var ~env ~cache:opam_var_cache "lib"
        >>| function
        | None -> []
        | Some s -> [Path.absolute s])
       (match which "ocamlfind" with
        | None ->
          return []
        | Some fn ->
          Future.run_capture_lines ~env (Accept [127])
            (Path.to_string fn) ["printconf"; "path"]
          >>| function
          | Ok lines -> List.map lines ~f:Path.absolute
          | Error _  -> [])
     >>| fun (a, b) ->
     match a @ b with
     | [] -> [Path.relative (Path.parent dir) "lib"]
     | l  ->
       List.fold_left l ~init:l ~f:(fun acc x ->
         if List.mem x ~set:acc then
           acc
         else
           x :: acc)
       |> List.rev)
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
  let get ?default var =
    match String_map.find var ocamlc_config with
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
  return
    { name
    ; kind
    ; merlin
    ; for_host = None
    ; build_dir
    ; path
    ; toplevel_path = Option.map (get_env env "OCAML_TOPLEVEL_PATH") ~f:Path.absolute

    ; ocaml_bin  = dir
    ; ocaml      = Path.relative dir "ocaml"
    ; ocamlc
    ; ocamlopt   = best_prog "ocamlopt"
    ; ocamllex   = get_prog  "ocamllex"
    ; ocamlyacc  = get_prog  "ocamlyacc"
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
    ; version                 = get       "version"
    ; ccomp_type              = get       "ccomp_type"
    ; bytecomp_c_compiler     = get       "bytecomp_c_compiler"
    ; bytecomp_c_libraries    = get       "bytecomp_c_libraries"
    ; native_c_compiler       = get       "native_c_compiler"
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
  Lazy.force Ansi_color.setup_env_for_ocaml_colors;
  Unix.environment ())

let default ?(merlin=true) () =
  let env = Lazy.force initial_env in
  let rec find_path i =
    if i = Array.length env then
      []
    else
      match String.lsplit2 env.(i) ~on:'=' with
      | Some ("PATH", s) ->
        Bin.parse_path s
      | _ -> find_path (i + 1)
  in
  let path = find_path 0 in
  create ~kind:Default ~path ~base_env:env ~env_extra:String_map.empty
    ~name:"default" ~merlin

let create_for_opam ?root ~switch ~name ?(merlin=false) () =
  match Bin.opam with
  | None -> die "Program opam not found in PATH"
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
      |> Sexp.Of_sexp.(string_map string)
    in
    let path =
      match String_map.find "PATH" vars with
      | None -> Bin.path
      | Some s -> Bin.parse_path s
    in
    let env = Lazy.force initial_env in
    create ~kind:(Opam { root; switch }) ~path ~base_env:env ~env_extra:vars
      ~name ~merlin

let which t s = which ~cache:t.which_cache ~path:t.path s

let install_prefix t =
  opam_config_var t "prefix" >>| function
  | Some x -> Path.absolute x
  | None   -> Path.parent t.ocaml_bin


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
  extend_env ~env:t.env ~vars:(String_map.of_alist_exn vars)

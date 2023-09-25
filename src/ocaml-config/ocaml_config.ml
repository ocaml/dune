open! Stdune
open Result.O

module Prog_and_args = struct
  type t =
    { prog : string
    ; args : string list
    }
end

open Prog_and_args

module Value = struct
  type t =
    | Bool of bool
    | Int of int
    | String of string
    | Words of string list
    | Prog_and_args of Prog_and_args.t

  let to_dyn : t -> Dyn.t =
    let open Dyn in
    function
    | Bool x -> Bool x
    | Int x -> Int x
    | String x -> String x
    | Words x -> (list string) x
    | Prog_and_args { prog; args } -> (list string) (prog :: args)
  ;;

  let to_string = function
    | Bool x -> string_of_bool x
    | Int x -> string_of_int x
    | String x -> x
    | Words x -> String.concat x ~sep:" "
    | Prog_and_args x -> String.concat ~sep:" " (x.prog :: x.args)
  ;;
end

module Os_type = struct
  type t =
    | Win32
    | Unix
    | Other of string

  let of_string = function
    | "Win32" -> Win32
    | "Unix" -> Unix
    | s -> Other s
  ;;

  let to_string = function
    | Win32 -> "Win32"
    | Unix -> "Unix"
    | Other s -> s
  ;;
end

module Ccomp_type = struct
  type t =
    | Msvc
    | Other of string

  let to_dyn =
    let open Dyn in
    function
    | Msvc -> variant "Msvc" []
    | Other s -> variant "Other" [ string s ]
  ;;

  let of_string = function
    | "msvc" -> Msvc
    | s -> Other s
  ;;

  let to_string = function
    | Msvc -> "msvc"
    | Other s -> s
  ;;
end

type t =
  { version : int * int * int
  ; version_string : string
  ; standard_library_default : string
  ; standard_library : string
  ; standard_runtime : string
  ; ccomp_type : Ccomp_type.t
  ; c_compiler : string
  ; ocamlc_cflags : string list
  ; ocamlc_cppflags : string list
  ; ocamlopt_cflags : string list
  ; ocamlopt_cppflags : string list
  ; bytecomp_c_compiler : Prog_and_args.t
  ; bytecomp_c_libraries : string list
  ; native_c_compiler : Prog_and_args.t
  ; native_c_libraries : string list
  ; native_pack_linker : Prog_and_args.t
  ; cc_profile : string list
  ; architecture : string
  ; model : string
  ; int_size : int
  ; word_size : int
  ; system : string
  ; asm : Prog_and_args.t
  ; asm_cfi_supported : bool
  ; with_frame_pointers : bool
  ; ext_exe : string
  ; ext_obj : string
  ; ext_asm : string
  ; ext_lib : string
  ; ext_dll : string
  ; os_type : Os_type.t
  ; default_executable_name : string
  ; systhread_supported : bool
  ; host : string
  ; target : string
  ; profiling : bool
  ; flambda : bool
  ; spacetime : bool
  ; safe_string : bool
  ; exec_magic_number : string
  ; cmi_magic_number : string
  ; cmo_magic_number : string
  ; cma_magic_number : string
  ; cmx_magic_number : string
  ; cmxa_magic_number : string
  ; ast_impl_magic_number : string
  ; ast_intf_magic_number : string
  ; cmxs_magic_number : string
  ; cmt_magic_number : string
  ; natdynlink_supported : bool
  ; supports_shared_libraries : bool
  ; windows_unicode : bool
  }

let version t = t.version
let version_string t = t.version_string
let standard_library_default t = t.standard_library_default
let standard_library t = t.standard_library
let standard_runtime t = t.standard_runtime
let ccomp_type t = t.ccomp_type
let c_compiler t = t.c_compiler
let ocamlc_cflags t = t.ocamlc_cflags
let ocamlc_cppflags t = t.ocamlc_cppflags
let ocamlopt_cflags t = t.ocamlopt_cflags
let ocamlopt_cppflags t = t.ocamlopt_cppflags
let bytecomp_c_compiler t = t.bytecomp_c_compiler
let bytecomp_c_libraries t = t.bytecomp_c_libraries
let native_c_compiler t = t.native_c_compiler
let native_c_libraries t = t.native_c_libraries
let native_pack_linker t = t.native_pack_linker
let cc_profile t = t.cc_profile
let architecture t = t.architecture
let model t = t.model
let int_size t = t.int_size
let word_size t = t.word_size
let system t = t.system
let asm t = t.asm
let asm_cfi_supported t = t.asm_cfi_supported
let with_frame_pointers t = t.with_frame_pointers
let ext_exe t = t.ext_exe
let ext_obj t = t.ext_obj
let ext_asm t = t.ext_asm
let ext_lib t = t.ext_lib
let ext_dll t = t.ext_dll
let os_type t = t.os_type
let default_executable_name t = t.default_executable_name
let systhread_supported t = t.systhread_supported
let host t = t.host
let target t = t.target
let profiling t = t.profiling
let flambda t = t.flambda
let spacetime t = t.spacetime
let safe_string t = t.safe_string
let exec_magic_number t = t.exec_magic_number
let cmi_magic_number t = t.cmi_magic_number
let cmo_magic_number t = t.cmo_magic_number
let cma_magic_number t = t.cma_magic_number
let cmx_magic_number t = t.cmx_magic_number
let cmxa_magic_number t = t.cmxa_magic_number
let ast_impl_magic_number t = t.ast_impl_magic_number
let ast_intf_magic_number t = t.ast_intf_magic_number
let cmxs_magic_number t = t.cmxs_magic_number
let cmt_magic_number t = t.cmt_magic_number
let natdynlink_supported t = t.natdynlink_supported
let supports_shared_libraries t = t.supports_shared_libraries
let windows_unicode t = t.windows_unicode

let to_list
  { version = _
  ; version_string
  ; standard_library_default
  ; standard_library
  ; standard_runtime
  ; ccomp_type
  ; c_compiler
  ; ocamlc_cflags
  ; ocamlc_cppflags
  ; ocamlopt_cflags
  ; ocamlopt_cppflags
  ; bytecomp_c_compiler
  ; bytecomp_c_libraries
  ; native_c_compiler
  ; native_c_libraries
  ; native_pack_linker
  ; cc_profile
  ; architecture
  ; model
  ; int_size
  ; word_size
  ; system
  ; asm
  ; asm_cfi_supported
  ; with_frame_pointers
  ; ext_exe
  ; ext_obj
  ; ext_asm
  ; ext_lib
  ; ext_dll
  ; os_type
  ; default_executable_name
  ; systhread_supported
  ; host
  ; target
  ; profiling
  ; flambda
  ; spacetime
  ; safe_string
  ; exec_magic_number
  ; cmi_magic_number
  ; cmo_magic_number
  ; cma_magic_number
  ; cmx_magic_number
  ; cmxa_magic_number
  ; ast_impl_magic_number
  ; ast_intf_magic_number
  ; cmxs_magic_number
  ; cmt_magic_number
  ; natdynlink_supported
  ; supports_shared_libraries
  ; windows_unicode
  }
  : (string * Value.t) list
  =
  [ "version", String version_string
  ; "standard_library_default", String standard_library_default
  ; "standard_library", String standard_library
  ; "standard_runtime", String standard_runtime
  ; "ccomp_type", String (Ccomp_type.to_string ccomp_type)
  ; "c_compiler", String c_compiler
  ; "ocamlc_cflags", Words ocamlc_cflags
  ; "ocamlc_cppflags", Words ocamlc_cppflags
  ; "ocamlopt_cflags", Words ocamlopt_cflags
  ; "ocamlopt_cppflags", Words ocamlopt_cppflags
  ; "bytecomp_c_compiler", Prog_and_args bytecomp_c_compiler
  ; "bytecomp_c_libraries", Words bytecomp_c_libraries
  ; "native_c_compiler", Prog_and_args native_c_compiler
  ; "native_c_libraries", Words native_c_libraries
  ; "native_pack_linker", Prog_and_args native_pack_linker
  ; "cc_profile", Words cc_profile
  ; "architecture", String architecture
  ; "model", String model
  ; "int_size", Int int_size
  ; "word_size", Int word_size
  ; "system", String system
  ; "asm", Prog_and_args asm
  ; "asm_cfi_supported", Bool asm_cfi_supported
  ; "with_frame_pointers", Bool with_frame_pointers
  ; "ext_exe", String ext_exe
  ; "ext_obj", String ext_obj
  ; "ext_asm", String ext_asm
  ; "ext_lib", String ext_lib
  ; "ext_dll", String ext_dll
  ; "os_type", String (Os_type.to_string os_type)
  ; "default_executable_name", String default_executable_name
  ; "systhread_supported", Bool systhread_supported
  ; "host", String host
  ; "target", String target
  ; "profiling", Bool profiling
  ; "flambda", Bool flambda
  ; "spacetime", Bool spacetime
  ; "safe_string", Bool safe_string
  ; "exec_magic_number", String exec_magic_number
  ; "cmi_magic_number", String cmi_magic_number
  ; "cmo_magic_number", String cmo_magic_number
  ; "cma_magic_number", String cma_magic_number
  ; "cmx_magic_number", String cmx_magic_number
  ; "cmxa_magic_number", String cmxa_magic_number
  ; "ast_impl_magic_number", String ast_impl_magic_number
  ; "ast_intf_magic_number", String ast_intf_magic_number
  ; "cmxs_magic_number", String cmxs_magic_number
  ; "cmt_magic_number", String cmt_magic_number
  ; "natdynlink_supported", Bool natdynlink_supported
  ; "supports_shared_libraries", Bool supports_shared_libraries
  ; "windows_unicode", Bool windows_unicode
  ]
;;

(* There is a test in the test suite to check that the names used in the above
   functions are the same as the ones used in the below function. *)

let by_name
  { version = _
  ; version_string
  ; standard_library_default
  ; standard_library
  ; standard_runtime
  ; ccomp_type
  ; c_compiler
  ; ocamlc_cflags
  ; ocamlc_cppflags
  ; ocamlopt_cflags
  ; ocamlopt_cppflags
  ; bytecomp_c_compiler
  ; bytecomp_c_libraries
  ; native_c_compiler
  ; native_c_libraries
  ; native_pack_linker
  ; cc_profile
  ; architecture
  ; model
  ; int_size
  ; word_size
  ; system
  ; asm
  ; asm_cfi_supported
  ; with_frame_pointers
  ; ext_exe
  ; ext_obj
  ; ext_asm
  ; ext_lib
  ; ext_dll
  ; os_type
  ; default_executable_name
  ; systhread_supported
  ; host
  ; target
  ; profiling
  ; flambda
  ; spacetime
  ; safe_string
  ; exec_magic_number
  ; cmi_magic_number
  ; cmo_magic_number
  ; cma_magic_number
  ; cmx_magic_number
  ; cmxa_magic_number
  ; ast_impl_magic_number
  ; ast_intf_magic_number
  ; cmxs_magic_number
  ; cmt_magic_number
  ; natdynlink_supported
  ; supports_shared_libraries
  ; windows_unicode
  }
  name
  : Value.t option
  =
  match name with
  | "version" -> Some (String version_string)
  | "standard_library_default" -> Some (String standard_library_default)
  | "standard_library" -> Some (String standard_library)
  | "standard_runtime" -> Some (String standard_runtime)
  | "ccomp_type" -> Some (String (Ccomp_type.to_string ccomp_type))
  | "c_compiler" -> Some (String c_compiler)
  | "ocamlc_cflags" -> Some (Words ocamlc_cflags)
  | "ocamlc_cppflags" -> Some (Words ocamlc_cppflags)
  | "ocamlopt_cflags" -> Some (Words ocamlopt_cflags)
  | "ocamlopt_cppflags" -> Some (Words ocamlopt_cppflags)
  | "bytecomp_c_compiler" -> Some (Prog_and_args bytecomp_c_compiler)
  | "bytecomp_c_libraries" -> Some (Words bytecomp_c_libraries)
  | "native_c_compiler" -> Some (Prog_and_args native_c_compiler)
  | "native_c_libraries" -> Some (Words native_c_libraries)
  | "native_pack_linker" -> Some (Prog_and_args native_pack_linker)
  | "cc_profile" -> Some (Words cc_profile)
  | "architecture" -> Some (String architecture)
  | "model" -> Some (String model)
  | "int_size" -> Some (Int int_size)
  | "word_size" -> Some (Int word_size)
  | "system" -> Some (String system)
  | "asm" -> Some (Prog_and_args asm)
  | "asm_cfi_supported" -> Some (Bool asm_cfi_supported)
  | "with_frame_pointers" -> Some (Bool with_frame_pointers)
  | "ext_exe" -> Some (String ext_exe)
  | "ext_obj" -> Some (String ext_obj)
  | "ext_asm" -> Some (String ext_asm)
  | "ext_lib" -> Some (String ext_lib)
  | "ext_dll" -> Some (String ext_dll)
  | "os_type" -> Some (String (Os_type.to_string os_type))
  | "default_executable_name" -> Some (String default_executable_name)
  | "systhread_supported" -> Some (Bool systhread_supported)
  | "host" -> Some (String host)
  | "target" -> Some (String target)
  | "profiling" -> Some (Bool profiling)
  | "flambda" -> Some (Bool flambda)
  | "spacetime" -> Some (Bool spacetime)
  | "safe_string" -> Some (Bool safe_string)
  | "exec_magic_number" -> Some (String exec_magic_number)
  | "cmi_magic_number" -> Some (String cmi_magic_number)
  | "cmo_magic_number" -> Some (String cmo_magic_number)
  | "cma_magic_number" -> Some (String cma_magic_number)
  | "cmx_magic_number" -> Some (String cmx_magic_number)
  | "cmxa_magic_number" -> Some (String cmxa_magic_number)
  | "ast_impl_magic_number" -> Some (String ast_impl_magic_number)
  | "ast_intf_magic_number" -> Some (String ast_intf_magic_number)
  | "cmxs_magic_number" -> Some (String cmxs_magic_number)
  | "cmt_magic_number" -> Some (String cmt_magic_number)
  | "natdynlink_supported" -> Some (Bool natdynlink_supported)
  | "supports_shared_libraries" -> Some (Bool supports_shared_libraries)
  | "windows_unicode" -> Some (Bool windows_unicode)
  | _ -> None
;;

let to_dyn t =
  let open Dyn in
  Record (to_list t |> List.map ~f:(fun (k, v) -> k, Value.to_dyn v))
;;

module Origin = struct
  type t =
    | Ocamlc_config
    | Makefile_config of Path.t
end

let split_prog s =
  match String.extract_blank_separated_words s with
  | [] -> None
  | prog :: args -> Some { prog; args }
;;

module Vars = struct
  type t = string String.Map.t

  let to_list = String.Map.to_list
  let of_list_exn = String.Map.of_list_exn
  let find = String.Map.find

  let of_lines lines =
    let rec loop acc = function
      | [] -> Ok acc
      | line :: lines ->
        (match String.index line ':' with
         | Some i ->
           let x =
             String.take line i, String.drop line (i + 2)
             (* skipping the space *)
           in
           loop (x :: acc) lines
         | None -> Error (Printf.sprintf "Unrecognized line: %S" line))
    in
    let* vars = loop [] lines in
    Result.map_error (String.Map.of_list vars) ~f:(fun (var, _, _) ->
      Printf.sprintf "Variable %S present twice." var)
  ;;

  let load_makefile_config file =
    let lines = Io.lines_of_file file in
    List.filter_map lines ~f:(fun line ->
      let line = String.trim line in
      if line = "" || line.[0] = '#' then None else String.lsplit2 line ~on:'=')
    |> String.Map.of_list_reduce ~f:(fun _ x -> x)
  ;;

  exception E of Origin.t * string

  module Getters (Origin : sig
      val origin : Origin.t
    end) =
  struct
    let fail fmt = Printf.ksprintf (fun msg -> raise (E (Origin.origin, msg))) fmt
    let get_opt t var = String.Map.find t var

    let get t var =
      match get_opt t var with
      | Some s -> s
      | None -> fail "Variable %S not found." var
    ;;

    let get_bool t ?(default = false) var =
      match get_opt t var with
      | None -> default
      | Some s ->
        (match s with
         | "true" -> true
         | "false" -> false
         | s -> fail "Value of %S is neither 'true' neither 'false': %s." var s)
    ;;

    let get_int_opt t var =
      Option.bind (get_opt t var) ~f:(fun s ->
        match Int.of_string s with
        | Some x -> Some x
        | None -> fail "Value of %S is not an integer: %s." var s)
    ;;

    let get_words t var =
      match get_opt t var with
      | None -> []
      | Some s -> String.extract_blank_separated_words s
    ;;

    let get_prog_or_dummy t var =
      Option.map (get_opt t var) ~f:(fun v ->
        match split_prog v with
        | None -> { prog = Printf.sprintf "%s-not-found-in-ocaml-config" var; args = [] }
        | Some s -> s)
    ;;

    let get_prog_or_dummy_exn t var =
      match get_prog_or_dummy t var with
      | None -> fail "Variable %S not found." var
      | Some s -> s
    ;;
  end

  module Ocamlc_config_getters = Getters (struct
      let origin = Origin.Ocamlc_config
    end)
end

let get_arch_sixtyfour stdlib_dir =
  let files = [ "caml/config.h"; "caml/m.h" ] in
  let get_arch_sixtyfour_from file =
    let file = Filename.concat stdlib_dir file in
    if Sys.file_exists file
    then (
      let rec loop ic =
        match input_line ic with
        | exception End_of_file -> false
        | line ->
          (match String.extract_blank_separated_words line with
           | [ "#define"; "ARCH_SIXTYFOUR" ] -> true
           | _ -> loop ic)
      in
      Exn.protectx (open_in file) ~finally:close_in ~f:loop)
    else false
  in
  List.exists ~f:get_arch_sixtyfour_from files
;;

let make vars =
  match
    let open Vars.Ocamlc_config_getters in
    let bytecomp_c_compiler = get_prog_or_dummy_exn vars "bytecomp_c_compiler" in
    let native_c_compiler = get_prog_or_dummy_exn vars "native_c_compiler" in
    let native_pack_linker = get_prog_or_dummy_exn vars "native_pack_linker" in
    let c_compiler, ocamlc_cflags, ocamlc_cppflags, ocamlopt_cflags, ocamlopt_cppflags =
      match get_prog_or_dummy vars "c_compiler" with
      | Some { prog; args } ->
        (* >= 4.06 GPR#1114, GPR#1393, GPR#1429: refine the (ocamlc -config)
           information on C compilers: the variables
           {bytecode,native}_c_compiler are deprecated (the distinction is now
           mostly meaningless) in favor of a single c_compiler variable combined
           with ocaml{c,opt}_cflags and ocaml{c,opt}_cppflags. *)
        let get_flags var = args @ get_words vars var in
        ( prog
        , get_flags "ocamlc_cflags"
        , get_flags "ocamlc_cppflags"
        , get_flags "ocamlopt_cflags"
        , get_flags "ocamlopt_cppflags" )
      | None ->
        bytecomp_c_compiler.prog, bytecomp_c_compiler.args, [], native_c_compiler.args, []
    in
    let version_string = get vars "version" in
    let version =
      match Scanf.sscanf version_string "%u.%u.%u" (fun a b c -> a, b, c) with
      | Ok t -> t
      | Error () ->
        User_error.raise
          [ Pp.textf "Unable to parse ocamlc -config version: %s" version_string ]
    in
    let os_type = Os_type.of_string (get vars "os_type") in
    let standard_library_default = get vars "standard_library_default" in
    let standard_library = get vars "standard_library" in
    let standard_runtime =
      Option.value
        (get_opt vars "standard_runtime")
        ~default:"the_standard_runtime_variable_was_deleted"
    in
    let ccomp_type = Ccomp_type.of_string (get vars "ccomp_type") in
    let bytecomp_c_libraries = get_words vars "bytecomp_c_libraries" in
    let native_c_libraries = get_words vars "native_c_libraries" in
    let cc_profile = get_words vars "cc_profile" in
    let architecture = get vars "architecture" in
    let model = get vars "model" in
    let system = get vars "system" in
    let asm_cfi_supported = get_bool vars "asm_cfi_supported" in
    let with_frame_pointers = get_bool vars "with_frame_pointers" in
    let asm = get_prog_or_dummy_exn vars "asm" in
    let word_size =
      match get_int_opt vars "word_size" with
      | Some n -> n
      | None -> if get_arch_sixtyfour standard_library then 64 else 32
    in
    let int_size =
      match get_int_opt vars "int_size" with
      | Some n -> n
      | None -> word_size - 1
    in
    let ext_obj = get vars "ext_obj" in
    let ext_asm = get vars "ext_asm" in
    let ext_lib = get vars "ext_lib" in
    let ext_dll = get vars "ext_dll" in
    let ext_exe =
      match get_opt vars "exe_ext" with
      | Some s -> s
      | None -> if os_type = Os_type.Win32 then ".exe" else ""
    in
    let default_executable_name = get vars "default_executable_name" in
    let systhread_supported = get_bool vars "systhread_supported" in
    let host = get vars "host" in
    let target = get vars "target" in
    let profiling = get_bool vars "profiling" in
    let flambda = get_bool vars "flambda" in
    let spacetime = get_bool vars "spacetime" in
    let safe_string = get_bool vars "safe_string" in
    let exec_magic_number = get vars "exec_magic_number" in
    let cmi_magic_number = get vars "cmi_magic_number" in
    let cmo_magic_number = get vars "cmo_magic_number" in
    let cma_magic_number = get vars "cma_magic_number" in
    let cmx_magic_number = get vars "cmx_magic_number" in
    let cmxa_magic_number = get vars "cmxa_magic_number" in
    let ast_impl_magic_number = get vars "ast_impl_magic_number" in
    let ast_intf_magic_number = get vars "ast_intf_magic_number" in
    let cmxs_magic_number = get vars "cmxs_magic_number" in
    let cmt_magic_number = get vars "cmt_magic_number" in
    let windows_unicode = get_bool vars "windows_unicode" in
    let natdynlink_supported =
      let lib = "dynlink.cmxa" in
      let lib = if version >= (5, 0, 0) then Filename.concat "dynlink" lib else lib in
      Sys.file_exists (Filename.concat standard_library lib)
    in
    let file =
      (* TODO This can give a code error if not an external path *)
      let stdlib = Path.external_ (Path.External.of_string standard_library) in
      Path.relative stdlib "Makefile.config"
    in
    let vars = Vars.load_makefile_config file in
    let module Getters =
      Vars.Getters (struct
        let origin = Origin.Makefile_config file
      end)
    in
    let supports_shared_libraries = get_bool vars "SUPPORTS_SHARED_LIBRARIES" in
    { version
    ; version_string
    ; standard_library_default
    ; standard_library
    ; standard_runtime
    ; ccomp_type
    ; c_compiler
    ; ocamlc_cflags
    ; ocamlc_cppflags
    ; ocamlopt_cflags
    ; ocamlopt_cppflags
    ; bytecomp_c_compiler
    ; bytecomp_c_libraries
    ; native_c_compiler
    ; native_c_libraries
    ; native_pack_linker
    ; cc_profile
    ; architecture
    ; model
    ; int_size
    ; word_size
    ; system
    ; asm
    ; asm_cfi_supported
    ; with_frame_pointers
    ; ext_exe
    ; ext_obj
    ; ext_asm
    ; ext_lib
    ; ext_dll
    ; os_type
    ; default_executable_name
    ; systhread_supported
    ; host
    ; target
    ; profiling
    ; flambda
    ; spacetime
    ; safe_string
    ; exec_magic_number
    ; cmi_magic_number
    ; cmo_magic_number
    ; cma_magic_number
    ; cmx_magic_number
    ; cmxa_magic_number
    ; ast_impl_magic_number
    ; ast_intf_magic_number
    ; cmxs_magic_number
    ; cmt_magic_number
    ; natdynlink_supported
    ; supports_shared_libraries
    ; windows_unicode
    }
  with
  | t -> Ok t
  | exception Vars.E (origin, msg) -> Error (origin, msg)
;;

let is_dev_version t =
  Scanf.sscanf t.version_string "%u.%u.%u+dev" (fun _ _ _ -> ()) |> Result.is_ok
;;

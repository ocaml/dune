open! Stdune
open Import

module Var = struct
  type t =
    | Values of Value.t list
    | Project_root
    | First_dep
    | Deps
    | Targets
    | Target
    | Named_local
    | Cc
    | Cxx

  let compare x y =
    match (x, y) with
    | Values v1, Values v2 -> List.compare ~compare:Value.compare v1 v2
    | Values _, _ -> Lt
    | _, Values _ -> Gt
    | Project_root, Project_root -> Eq
    | Project_root, _ -> Lt
    | _, Project_root -> Gt
    | First_dep, First_dep -> Eq
    | First_dep, _ -> Lt
    | _, First_dep -> Gt
    | Deps, Deps -> Eq
    | Deps, _ -> Lt
    | _, Deps -> Gt
    | Named_local, Named_local -> Eq
    | Named_local, _ -> Lt
    | _, Named_local -> Gt
    | Targets, Targets -> Eq
    | Targets, _ -> Lt
    | _, Targets -> Gt
    | Target, Target -> Eq
    | Target, _ -> Lt
    | _, Target -> Gt
    | Cc, Cc -> Eq
    | Cc, _ -> Lt
    | _, Cc -> Gt
    | Cxx, Cxx -> Eq

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Values values -> constr "Values" [ list Value.to_dyn values ]
    | Project_root -> string "Project_root"
    | First_dep -> string "First_dep"
    | Deps -> string "Deps"
    | Targets -> string "Targets"
    | Target -> string "Target"
    | Named_local -> string "Named_local"
    | Cc -> string "cc"
    | Cxx -> string "cxx"
end

module Artifact = struct
  type t =
    | Mod of Cm_kind.t
    | Lib of Mode.t

  let compare x y =
    match (x, y) with
    | Mod x, Mod y -> Cm_kind.compare x y
    | Mod _, _ -> Lt
    | _, Mod _ -> Gt
    | Lib x, Lib y -> Mode.compare x y

  let ext = function
    | Mod cm_kind -> Cm_kind.ext cm_kind
    | Lib mode -> Mode.compiled_lib_ext mode

  let all =
    List.map ~f:(fun kind -> Mod kind) Cm_kind.all
    @ List.map ~f:(fun mode -> Lib mode) Mode.all

  let to_dyn a =
    let open Dyn.Encoder in
    match a with
    | Mod cm_kind -> constr "Mod" [ Cm_kind.to_dyn cm_kind ]
    | Lib mode -> constr "Lib" [ Mode.to_dyn mode ]
end

module Macro = struct
  type t =
    | Exe
    | Dep
    | Bin
    | Lib of
        { lib_exec : bool
        ; lib_private : bool
        }
    | Lib_available
    | Version
    | Read
    | Read_strings
    | Read_lines
    | Path_no_dep
    | Ocaml_config
    | Env
    | Artifact of Artifact.t

  let compare x y =
    match (x, y) with
    | Exe, Exe -> Eq
    | Exe, _ -> Lt
    | _, Exe -> Gt
    | Dep, Dep -> Eq
    | Dep, _ -> Lt
    | _, Dep -> Gt
    | Bin, Bin -> Eq
    | Bin, _ -> Lt
    | _, Bin -> Gt
    | Lib { lib_exec; lib_private }, Lib y ->
      Tuple.T2.compare Bool.compare Bool.compare (lib_exec, lib_private)
        (y.lib_exec, y.lib_private)
    | Lib _, _ -> Lt
    | _, Lib _ -> Gt
    | Lib_available, Lib_available -> Eq
    | Lib_available, _ -> Lt
    | _, Lib_available -> Gt
    | Version, Version -> Eq
    | Version, _ -> Lt
    | _, Version -> Gt
    | Read, Read -> Eq
    | Read, _ -> Lt
    | _, Read -> Gt
    | Read_strings, Read_strings -> Eq
    | Read_strings, _ -> Lt
    | _, Read_strings -> Gt
    | Read_lines, Read_lines -> Eq
    | Read_lines, _ -> Lt
    | _, Read_lines -> Gt
    | Path_no_dep, Path_no_dep -> Eq
    | Path_no_dep, _ -> Lt
    | _, Path_no_dep -> Gt
    | Ocaml_config, Ocaml_config -> Eq
    | Ocaml_config, _ -> Lt
    | _, Ocaml_config -> Gt
    | Env, Env -> Eq
    | Env, _ -> Lt
    | _, Env -> Gt
    | Artifact x, Artifact y -> Artifact.compare x y

  let to_dyn =
    let open Dyn.Encoder in
    function
    | Exe -> string "Exe"
    | Dep -> string "Dep"
    | Bin -> string "Bin"
    | Lib { lib_private; lib_exec } ->
      constr "Lib"
        [ record
            [ ("lib_exec", bool lib_exec); ("lib_private", bool lib_private) ]
        ]
    | Lib_available -> string "Lib_available"
    | Version -> string "Version"
    | Read -> string "Read"
    | Read_strings -> string "Read_strings"
    | Read_lines -> string "Read_lines"
    | Path_no_dep -> string "Path_no_dep"
    | Ocaml_config -> string "Ocaml_config"
    | Env -> string "Env"
    | Artifact ext -> constr "Artifact" [ Artifact.to_dyn ext ]
end

module Expansion = struct
  module T = struct
    type t =
      | Var of Var.t
      | Macro of Macro.t * string

    let to_dyn e =
      let open Dyn.Encoder in
      match e with
      | Var v -> pair string Var.to_dyn ("Var", v)
      | Macro (m, s) -> triple string Macro.to_dyn string ("Macro", m, s)

    let compare x y =
      match (x, y) with
      | Var x, Var y -> Var.compare x y
      | Var _, _ -> Lt
      | _, Var _ -> Gt
      | Macro (m1, s1), Macro (m2, s2) ->
        Tuple.T2.compare Macro.compare String.compare (m1, s1) (m2, s2)
  end

  include T
  module Map = Map.Make (T)
end

type 'a t =
  | No_info of 'a
  | Since of 'a * Dune_lang.Syntax.Version.t
  | Deleted_in of
      'a * Dune_lang.Syntax.Version.t * User_message.Style.t Pp.t list
  | Renamed_in of Dune_lang.Syntax.Version.t * string

let values v = No_info (Var.Values v)

let renamed_in ~new_name ~version = Renamed_in (version, new_name)

let deleted_in ~version ?(repl = []) kind = Deleted_in (kind, version, repl)

let since ~version v = Since (v, version)

type 'a pform = 'a t

let to_dyn f =
  let open Dyn.Encoder in
  function
  | No_info x -> constr "No_info" [ f x ]
  | Since (x, v) -> constr "Since" [ f x; Dune_lang.Syntax.Version.to_dyn v ]
  | Deleted_in (x, v, repl) ->
    constr "Deleted_in"
      [ f x
      ; Dune_lang.Syntax.Version.to_dyn v
      ; List
          (List.map repl ~f:(fun pp ->
               Dyn.String (Format.asprintf "%a" Pp.render_ignore_tags pp)))
      ]
  | Renamed_in (v, s) ->
    constr "Renamed_in" [ Dune_lang.Syntax.Version.to_dyn v; string s ]

module Map = struct
  type 'a map = 'a t String.Map.t

  type t =
    { vars : Var.t map
    ; macros : Macro.t map
    }

  let static_vars =
    String.Map.of_list_exn
      [ ("targets", since ~version:(1, 0) Var.Targets)
      ; ("target", since ~version:(1, 11) Var.Target)
      ; ("deps", since ~version:(1, 0) Var.Deps)
      ; ("project_root", since ~version:(1, 0) Var.Project_root)
      ; ( "<"
        , deleted_in Var.First_dep ~version:(1, 0)
            ~repl:
              [ Pp.text
                  "Use a named dependency instead:\n\n\
                  \  (deps (:x <dep>) ...)\n\
                  \   ... %{x} ..."
              ] )
      ; ("@", renamed_in ~version:(1, 0) ~new_name:"targets")
      ; ("^", renamed_in ~version:(1, 0) ~new_name:"deps")
      ; ("SCOPE_ROOT", renamed_in ~version:(1, 0) ~new_name:"project_root")
      ]

  let macros =
    let macro (x : Macro.t) = No_info x in
    let artifact x =
      (String.drop (Artifact.ext x) 1, since ~version:(2, 0) (Macro.Artifact x))
    in
    String.Map.of_list_exn
      ( [ ("exe", macro Exe)
        ; ("bin", macro Bin)
        ; ("lib", macro (Lib { lib_exec = false; lib_private = false }))
        ; ("libexec", macro (Lib { lib_exec = true; lib_private = false }))
        ; ( "lib-private"
          , since ~version:(2, 1)
              (Macro.Lib { lib_exec = false; lib_private = true }) )
        ; ( "libexec-private"
          , since ~version:(2, 1)
              (Macro.Lib { lib_exec = true; lib_private = true }) )
        ; ("lib-available", macro Lib_available)
        ; ("version", macro Version)
        ; ("read", macro Read)
        ; ("read-lines", macro Read_lines)
        ; ("read-strings", macro Read_strings)
        ; ("dep", since ~version:(1, 0) Macro.Dep)
        ; ("path", renamed_in ~version:(1, 0) ~new_name:"dep")
        ; ("findlib", renamed_in ~version:(1, 0) ~new_name:"lib")
        ; ("path-no-dep", deleted_in ~version:(1, 0) Macro.Path_no_dep)
        ; ("ocaml-config", macro Ocaml_config)
        ; ("env", since ~version:(1, 4) Macro.Env)
        ]
      @ List.map ~f:artifact Artifact.all )

  let create ~(context : Context.t) =
    let ocamlopt =
      match context.ocamlopt with
      | Error _ -> Path.relative context.ocaml_bin "ocamlopt"
      | Ok p -> p
    in
    let ocaml =
      match context.ocaml with
      | Error _ -> Path.relative context.ocaml_bin "ocaml"
      | Ok p -> p
    in
    let string s = values [ Value.String s ] in
    let path p = values [ Value.Path p ] in
    let make =
      match Bin.make ~path:(Env.path context.env) with
      | None -> string "make"
      | Some p -> path p
    in
    let cflags = Ocaml_config.ocamlc_cflags context.ocaml_config in
    let strings s = values (Value.L.strings s) in
    let lowercased =
      let c_compiler = Ocaml_config.c_compiler context.ocaml_config in
      [ ("cpp", strings ((c_compiler :: cflags) @ [ "-E" ]))
      ; ( "pa_cpp"
        , strings
            ( (c_compiler :: cflags)
            @ [ "-undef"; "-traditional"; "-x"; "c"; "-E" ] ) )
      ; ("ocaml", path ocaml)
      ; ("ocamlc", path context.ocamlc)
      ; ("ocamlopt", path ocamlopt)
      ; ("arch_sixtyfour", string (string_of_bool context.arch_sixtyfour))
      ; ("make", make)
      ; ("cc", No_info Var.Cc)
      ; ("cxx", No_info Var.Cxx)
      ]
    in
    let uppercased =
      List.map lowercased ~f:(fun (k, _) ->
          (String.uppercase k, renamed_in ~new_name:k ~version:(1, 0)))
    in
    let other =
      let ext_asm = Ocaml_config.ext_asm context.ocaml_config in
      let ext_exe = Ocaml_config.ext_exe context.ocaml_config in
      let os_type = Ocaml_config.os_type context.ocaml_config in
      let architecture = Ocaml_config.architecture context.ocaml_config in
      let model = Ocaml_config.model context.ocaml_config in
      let system = Ocaml_config.system context.ocaml_config in
      let version_string = Ocaml_config.version_string context.ocaml_config in
      let ext_plugin =
        Mode.plugin_ext
          ( if Ocaml_config.natdynlink_supported context.ocaml_config then
            Mode.Native
          else
            Mode.Byte )
      in
      [ ("-verbose", values [])
      ; ("ocaml_bin", values [ Dir context.ocaml_bin ])
      ; ("ocaml_version", string version_string)
      ; ("ocaml_where", string (Path.to_string context.stdlib_dir))
      ; ("null", string (Path.to_string Config.dev_null))
      ; ("ext_obj", string context.lib_config.ext_obj)
      ; ("ext_asm", string ext_asm)
      ; ("ext_lib", string context.lib_config.ext_lib)
      ; ("ext_dll", string context.lib_config.ext_dll)
      ; ("ext_exe", string ext_exe)
      ; ("ext_plugin", since ~version:(2, 4) (Var.Values [ String ext_plugin ]))
      ; ("profile", string (Profile.to_string context.profile))
      ; ("workspace_root", values [ Value.Dir (Path.build context.build_dir) ])
      ; ("context_name", string (Context_name.to_string context.name))
      ; ("ROOT", renamed_in ~version:(1, 0) ~new_name:"workspace_root")
      ; ( "os_type"
        , since ~version:(1, 10)
            (Var.Values [ String (Ocaml_config.Os_type.to_string os_type) ]) )
      ; ( "architecture"
        , since ~version:(1, 10) (Var.Values [ String architecture ]) )
      ; ("system", since ~version:(1, 10) (Var.Values [ String system ]))
      ; ("model", since ~version:(1, 10) (Var.Values [ String model ]))
      ; ( "ignoring_promoted_rules"
        , since ~version:(1, 10)
            (Var.Values
               [ String (string_of_bool !Clflags.ignore_promoted_rules) ]) )
      ]
    in
    { vars =
        String.Map.superpose static_vars
          (String.Map.of_list_exn
             (List.concat [ lowercased; uppercased; other ]))
    ; macros
    }

  let superpose a b =
    { vars = String.Map.superpose a.vars b.vars
    ; macros = String.Map.superpose a.macros b.macros
    }

  let rec expand map ~syntax_version ~pform =
    let open Option.O in
    let open Dune_lang.Syntax.Version.Infix in
    let name = String_with_vars.Var.name pform in
    let* v = String.Map.find map name in
    let describe = String_with_vars.Var.describe in
    match v with
    | No_info v -> Some v
    | Since (v, min_version) ->
      if syntax_version >= min_version then
        Some v
      else
        Dune_lang.Syntax.Error.since
          (String_with_vars.Var.loc pform)
          Stanza.syntax min_version ~what:(describe pform)
    | Renamed_in (in_version, new_name) ->
      if syntax_version >= in_version then
        Dune_lang.Syntax.Error.renamed_in
          (String_with_vars.Var.loc pform)
          Stanza.syntax syntax_version ~what:(describe pform)
          ~to_:(describe (String_with_vars.Var.with_name pform ~name:new_name))
      else
        expand map ~syntax_version:in_version
          ~pform:(String_with_vars.Var.with_name pform ~name:new_name)
    | Deleted_in (v, in_version, repl) ->
      if syntax_version < in_version then
        Some v
      else
        Dune_lang.Syntax.Error.deleted_in
          (String_with_vars.Var.loc pform)
          Stanza.syntax in_version ~what:(describe pform) ~repl

  let expand t pform syntax_version =
    match String_with_vars.Var.payload pform with
    | None ->
      Option.map (expand t.vars ~syntax_version ~pform) ~f:(fun x ->
          Expansion.Var x)
    | Some payload ->
      Option.map (expand t.macros ~syntax_version ~pform) ~f:(fun x ->
          Expansion.Macro (x, payload))

  let to_dyn { vars; macros } =
    let open Dyn.Encoder in
    record
      [ ("vars", String.Map.to_dyn (to_dyn Var.to_dyn) vars)
      ; ("macros", String.Map.to_dyn (to_dyn Macro.to_dyn) macros)
      ]

  let expand_exn t pform syntax_version =
    match expand t pform syntax_version with
    | Some v -> v
    | None ->
      Code_error.raise "Pform.Map.expand_exn"
        [ ("t", to_dyn t)
        ; ("pform", String_with_vars.Var.to_dyn pform)
        ; ("syntax_version", Dune_lang.Syntax.Version.to_dyn syntax_version)
        ]

  let empty = { vars = String.Map.empty; macros = String.Map.empty }

  let singleton k v =
    { vars = String.Map.singleton k (No_info v); macros = String.Map.empty }

  let of_list_exn pforms =
    { vars = String.Map.of_list_map_exn ~f:(fun (k, x) -> (k, No_info x)) pforms
    ; macros = String.Map.empty
    }

  let of_bindings bindings =
    { vars =
        Bindings.fold bindings ~init:String.Map.empty ~f:(fun x acc ->
            match x with
            | Unnamed _ -> acc
            | Named (s, _) -> String.Map.set acc s (No_info Var.Named_local))
    ; macros = String.Map.empty
    }

  let input_file path =
    let value = Var.Values (Value.L.paths [ path ]) in
    { vars =
        String.Map.of_list_exn
          [ ("input-file", since ~version:(1, 0) value)
          ; ("<", renamed_in ~new_name:"input-file" ~version:(1, 0))
          ]
    ; macros = String.Map.empty
    }

  type stamp = (string * Var.t pform) list * (string * Macro.t pform) list

  let to_stamp { vars; macros } : stamp =
    (String.Map.to_list vars, String.Map.to_list macros)
end

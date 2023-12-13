open Stdune
open Dune_sexp
module Payload = Template.Pform.Payload

module Var = struct
  module Pkg = struct
    module Section = struct
      type t =
        | Lib
        | Libexec
        | Bin
        | Sbin
        | Toplevel
        | Share
        | Etc
        | Doc
        | Stublibs
        | Man

      let all =
        [ Lib, "lib"
        ; Libexec, "libexec"
        ; Bin, "bin"
        ; Sbin, "sbin"
        ; Toplevel, "toplevel"
        ; Share, "share"
        ; Etc, "etc"
        ; Doc, "doc"
        ; Stublibs, "stublibs"
        ; Man, "man"
        ]
      ;;

      let to_string t = List.assoc all t |> Option.value_exn

      let rec of_string x = function
        | [] -> None
        | (s, x') :: xs -> if x' = x then Some s else of_string x xs
      ;;

      let of_string x = of_string x all
    end

    type t =
      | Switch
      | Os
      | Os_version
      | Os_distribution
      | Os_family
      | Build
      | Prefix
      | User
      | Group
      | Jobs
      | Arch
      | Sys_ocaml_version
      | Section_dir of Section.t

    let compare = Poly.compare

    let to_dyn t =
      let open Dyn in
      match t with
      | Switch -> variant "Switch" []
      | Os -> variant "Os" []
      | Os_version -> variant "Os_version" []
      | Os_distribution -> variant "Os_distribution" []
      | Os_family -> variant "Os_family" []
      | Build -> variant "Build" []
      | Prefix -> variant "Prefix" []
      | User -> variant "User" []
      | Group -> variant "Group" []
      | Jobs -> variant "Jobs" []
      | Arch -> variant "Arch" []
      | Sys_ocaml_version -> variant "Sys_ocaml_version" []
      | Section_dir section ->
        variant "Section_dir" [ string (Section.to_string section) ]
    ;;

    let encode_to_latest_dune_lang_version = function
      | Switch -> "switch"
      | Os -> "os"
      | Os_version -> "os_version"
      | Os_distribution -> "os_distribution"
      | Os_family -> "os_family"
      | Build -> "build"
      | Prefix -> "prefix"
      | User -> "user"
      | Group -> "group"
      | Jobs -> "jobs"
      | Arch -> "arch"
      | Sys_ocaml_version -> "sys_ocaml_version"
      | Section_dir section -> Section.to_string section
    ;;
  end

  type t =
    | User_var of string
    | Nothing
    | Project_root
    | Workspace_root
    | First_dep
    | Deps
    | Targets
    | Target
    | Cc
    | Cxx
    | Ccomp_type
    | Cpp
    | Pa_cpp
    | Make
    | Ocaml_version
    | Ocaml
    | Ocamlc
    | Ocamlopt
    | Ocaml_bin_dir
    | Ocaml_stdlib_dir
    | Dev_null
    | Ext_obj
    | Ext_asm
    | Ext_lib
    | Ext_dll
    | Ext_exe
    | Ext_plugin
    | Profile
    | Context_name
    | Os_type
    | Architecture
    | Arch_sixtyfour
    | System
    | Model
    | Ignoring_promoted_rules
    | Input_file
    | Library_name
    | Partition
    | Impl_files
    | Intf_files
    | Test
    | Corrected_suffix
    | Inline_tests
    | Toolchain
    | Pkg of Pkg.t

  let compare : t -> t -> Ordering.t = Poly.compare

  let to_dyn = function
    | User_var v -> Dyn.Variant ("User_var", [ String v ])
    | t ->
      let open Dyn in
      (match t with
       | User_var _ -> assert false
       | Nothing -> variant "Nothing" []
       | Project_root -> variant "Project_root" []
       | Workspace_root -> variant "Workspace_root" []
       | First_dep -> variant "First_dep" []
       | Deps -> variant "Deps" []
       | Targets -> variant "Targets" []
       | Target -> variant "Target" []
       | Cc -> variant "Cc" []
       | Cxx -> variant "Cxx" []
       | Ccomp_type -> variant "Ccomp_type" []
       | Cpp -> variant "Cpp" []
       | Pa_cpp -> variant "Pa_cpp" []
       | Make -> variant "Make" []
       | Ocaml_version -> variant "Ocaml_version" []
       | Ocaml -> variant "Ocaml" []
       | Ocamlc -> variant "Ocamlc" []
       | Ocamlopt -> variant "Ocamlopt" []
       | Ocaml_bin_dir -> variant "Ocaml_bin_dir" []
       | Ocaml_stdlib_dir -> variant "Ocaml_stdlib_dir" []
       | Dev_null -> variant "Dev_null" []
       | Ext_obj -> variant "Ext_obj" []
       | Ext_asm -> variant "Ext_asm" []
       | Ext_lib -> variant "Ext_lib" []
       | Ext_dll -> variant "Ext_dll" []
       | Ext_exe -> variant "Ext_exe" []
       | Ext_plugin -> variant "Ext_plugin" []
       | Profile -> variant "Profile" []
       | Context_name -> variant "Context_name" []
       | Os_type -> variant "Os_type" []
       | Architecture -> variant "Architecture" []
       | Arch_sixtyfour -> variant "Arch_sixtyfour" []
       | System -> variant "System" []
       | Model -> variant "Model" []
       | Ignoring_promoted_rules -> variant "Ignoring_promoted_rules" []
       | Input_file -> variant "Input_file" []
       | Library_name -> variant "Library_name" []
       | Partition -> variant "Partition" []
       | Impl_files -> variant "Impl_files" []
       | Intf_files -> variant "Intf_files" []
       | Test -> variant "Test" []
       | Corrected_suffix -> variant "Corrected_suffix" []
       | Inline_tests -> variant "Inline_tests" []
       | Toolchain -> variant "Toolchain" []
       | Pkg pkg -> Pkg.to_dyn pkg)
  ;;

  module Map = Map.Make (struct
      type nonrec t = t

      let to_dyn = to_dyn
      let compare = compare
    end)

  let of_opam_global_variable_name name =
    match Pkg.Section.of_string name with
    | Some section_dir -> Some (Pkg (Pkg.Section_dir section_dir))
    | None ->
      (match name with
       | "make" -> Some Make
       | "switch" -> Some (Pkg Switch)
       | "os" -> Some (Pkg Os)
       | "os-version" -> Some (Pkg Os_version)
       | "os-distribution" -> Some (Pkg Os_distribution)
       | "os-family" -> Some (Pkg Os_family)
       | "build" -> Some (Pkg Build)
       | "prefix" -> Some (Pkg Prefix)
       | "user" -> Some (Pkg User)
       | "group" -> Some (Pkg Group)
       | "jobs" -> Some (Pkg Jobs)
       | "arch" -> Some (Pkg Arch)
       | "sys-ocaml-version" -> Some (Pkg Sys_ocaml_version)
       | _ -> None)
  ;;
end

module Artifact = struct
  open Ocaml

  type t =
    | Mod of Cm_kind.t
    | Lib of Mode.t

  let compare x y =
    match x, y with
    | Mod x, Mod y -> Cm_kind.compare x y
    | Mod _, _ -> Lt
    | _, Mod _ -> Gt
    | Lib x, Lib y -> Mode.compare x y
  ;;

  let ext = function
    | Mod cm_kind -> Cm_kind.ext cm_kind
    | Lib mode -> Mode.compiled_lib_ext mode
  ;;

  let all =
    List.map ~f:(fun kind -> Mod kind) Cm_kind.all
    @ List.map ~f:(fun mode -> Lib mode) Mode.all
  ;;

  let to_dyn a =
    let open Dyn in
    match a with
    | Mod cm_kind -> variant "Mod" [ Cm_kind.to_dyn cm_kind ]
    | Lib mode -> variant "Lib" [ Mode.to_dyn mode ]
  ;;
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
    | Bin_available
    | Version
    | Read
    | Read_strings
    | Read_lines
    | Path_no_dep
    | Ocaml_config
    | Coq_config
    | Env
    | Artifact of Artifact.t
    | Pkg
    | Pkg_self

  let compare x y =
    match x, y with
    | Exe, Exe -> Eq
    | Exe, _ -> Lt
    | _, Exe -> Gt
    | Dep, Dep -> Eq
    | Dep, _ -> Lt
    | _, Dep -> Gt
    | Bin, Bin -> Eq
    | Bin, _ -> Lt
    | _, Bin -> Gt
    | Lib { lib_exec; lib_private }, Lib t ->
      let open Ordering.O in
      let= () = Bool.compare lib_exec t.lib_exec in
      Bool.compare lib_private t.lib_private
    | Lib _, _ -> Lt
    | _, Lib _ -> Gt
    | Lib_available, Lib_available -> Eq
    | Lib_available, _ -> Lt
    | _, Lib_available -> Gt
    | Bin_available, Bin_available -> Eq
    | Bin_available, _ -> Lt
    | _, Bin_available -> Gt
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
    | Coq_config, Coq_config -> Eq
    | Coq_config, _ -> Lt
    | _, Coq_config -> Gt
    | Env, Env -> Eq
    | Env, _ -> Lt
    | _, Env -> Gt
    | Pkg, Pkg -> Eq
    | Pkg, _ -> Lt
    | _, Pkg -> Gt
    | Pkg_self, Pkg_self -> Eq
    | Pkg_self, _ -> Lt
    | _, Pkg_self -> Gt
    | Artifact x, Artifact y -> Artifact.compare x y
  ;;

  let to_dyn =
    let open Dyn in
    function
    | Exe -> string "Exe"
    | Dep -> string "Dep"
    | Bin -> string "Bin"
    | Lib { lib_private; lib_exec } ->
      variant
        "Lib"
        [ record [ "lib_exec", bool lib_exec; "lib_private", bool lib_private ] ]
    | Lib_available -> string "Lib_available"
    | Bin_available -> string "Bin_available"
    | Version -> string "Version"
    | Read -> string "Read"
    | Read_strings -> string "Read_strings"
    | Read_lines -> string "Read_lines"
    | Path_no_dep -> string "Path_no_dep"
    | Ocaml_config -> string "Ocaml_config"
    | Coq_config -> string "Coq_config"
    | Env -> string "Env"
    | Artifact ext -> variant "Artifact" [ Artifact.to_dyn ext ]
    | Pkg -> variant "Pkg" []
    | Pkg_self -> variant "Pkg_self" []
  ;;

  module Map = Map.Make (struct
      type nonrec t = t

      let to_dyn = to_dyn
      let compare = compare
    end)

  let encode = function
    | Exe -> Ok "exe"
    | Dep -> Ok "dep"
    | Bin -> Ok "bin"
    | Lib { lib_exec = false; lib_private = false } -> Ok "lib"
    | Lib { lib_exec = true; lib_private = false } -> Ok "libexec"
    | Lib { lib_exec = false; lib_private = true } -> Ok "lib-private"
    | Lib { lib_exec = true; lib_private = true } -> Ok "libexec-private"
    | Lib_available -> Ok "lib-available"
    | Bin_available -> Ok "bin-available"
    | Version -> Ok "version"
    | Read -> Ok "read"
    | Read_strings -> Ok "read-strings"
    | Read_lines -> Ok "read-lines"
    | Path_no_dep -> Error `Pform_was_deleted
    | Ocaml_config -> Ok "ocaml-config"
    | Coq_config -> Ok "coq"
    | Env -> Ok "env"
    | Pkg -> Ok "pkg"
    | Pkg_self -> Ok "pkg-self"
    | Artifact a -> Ok (String.drop (Artifact.ext a) 1)
  ;;
end

module Macro_invocation = struct
  type t =
    { macro : Macro.t
    ; payload : Payload.t
    }

  let to_dyn { macro; payload } =
    Dyn.record [ "macro", Macro.to_dyn macro; "payload", Payload.to_dyn payload ]
  ;;

  let compare { macro; payload } t =
    let open Ordering.O in
    let= () = Macro.compare macro t.macro in
    Payload.compare payload t.payload
  ;;

  module Args = struct
    let whole { payload; _ } = Payload.Args.whole payload

    let lsplit2 { payload; macro } loc =
      Payload.Args.lsplit2 payload loc
      |> Result.map_error ~f:(fun (user_message : User_message.t) ->
        let paragraphs =
          match Macro.encode macro with
          | Ok name ->
            let header = Pp.textf "Incorrect arguments for macro %s." name in
            header :: user_message.paragraphs
          | Error `Pform_was_deleted -> user_message.paragraphs
        in
        { user_message with paragraphs })
    ;;

    let lsplit2_exn t loc = lsplit2 t loc |> User_error.ok_exn
    let split { payload; _ } = Payload.Args.split payload
  end
end

module T = struct
  type t =
    | Var of Var.t
    | Macro of Macro_invocation.t

  let to_dyn e =
    let open Dyn in
    match e with
    | Var v -> variant "Var" [ Var.to_dyn v ]
    | Macro macro_invocation ->
      variant "Macro" [ Macro_invocation.to_dyn macro_invocation ]
  ;;

  let compare x y =
    match x, y with
    | Var x, Var y -> Var.compare x y
    | Var _, _ -> Lt
    | _, Var _ -> Gt
    | Macro a, Macro b -> Macro_invocation.compare a b
  ;;
end

include T
module Map = Map.Make (T)

type encode_result =
  | Success of
      { name : string
      ; payload : Payload.t option
      }
  | Pform_was_deleted

let encode_to_latest_dune_lang_version t =
  match t with
  | Var x ->
    (match
       match x with
       | User_var x -> Some x
       | Nothing -> None
       | Project_root -> Some "project_root"
       | Workspace_root -> Some "workspace_root"
       | First_dep -> None
       | Deps -> Some "deps"
       | Targets -> Some "targets"
       | Target -> Some "target"
       | Cc -> Some "cc"
       | Cxx -> Some "cxx"
       | Ccomp_type -> Some "ccomp_type"
       | Cpp -> Some "cpp"
       | Pa_cpp -> Some "pa_cpp"
       | Make -> Some "make"
       | Ocaml_version -> Some "ocaml_version"
       | Ocaml -> Some "ocaml"
       | Ocamlc -> Some "ocamlc"
       | Ocamlopt -> Some "ocamlopt"
       | Ocaml_bin_dir -> Some "ocaml_bin"
       | Ocaml_stdlib_dir -> Some "ocaml_where"
       | Dev_null -> Some "null"
       | Ext_obj -> Some "ext_obj"
       | Ext_asm -> Some "ext_asm"
       | Ext_lib -> Some "ext_lib"
       | Ext_dll -> Some "ext_dll"
       | Ext_exe -> Some "ext_exe"
       | Ext_plugin -> Some "ext_plugin"
       | Profile -> Some "profile"
       | Context_name -> Some "context_name"
       | Os_type -> Some "os_type"
       | Architecture -> Some "architecture"
       | Arch_sixtyfour -> Some "arch_sixtyfour"
       | System -> Some "system"
       | Model -> Some "model"
       | Ignoring_promoted_rules -> Some "ignoring_promoted_rules"
       | Input_file -> Some "input_file"
       | Library_name -> Some "library-name"
       | Partition -> Some "partition"
       | Impl_files -> Some "impl-files"
       | Intf_files -> Some "intf-files"
       | Test -> Some "test"
       | Corrected_suffix -> Some "corrected-suffix"
       | Inline_tests -> Some "inline_tests"
       | Toolchain -> Some "toolchain"
       | Pkg pkg -> Some (Var.Pkg.encode_to_latest_dune_lang_version pkg)
     with
     | None -> Pform_was_deleted
     | Some name -> Success { name; payload = None })
  | Macro { macro; payload } ->
    (match Macro.encode macro with
     | Error `Pform_was_deleted -> Pform_was_deleted
     | Ok name -> Success { name; payload = Some payload })
;;

let describe_kind = function
  | Var _ -> "variable"
  | Macro _ -> "macro"
;;

module With_versioning_info = struct
  type 'a t =
    | No_info of 'a
    | Since of 'a * Syntax.Version.t
    | Deleted_in of 'a * Syntax.Version.t * User_message.Style.t Pp.t list
    | Renamed_in of 'a * Syntax.Version.t * string

  let get_data = function
    | No_info x | Since (x, _) | Deleted_in (x, _, _) | Renamed_in (x, _, _) -> x
  ;;

  let renamed_in x ~new_name ~version = Renamed_in (x, version, new_name)
  let deleted_in ~version ?(repl = []) kind = Deleted_in (kind, version, repl)
  let since ~version v = Since (v, version)

  let to_dyn f =
    let open Dyn in
    function
    | No_info x -> variant "No_info" [ f x ]
    | Since (x, v) -> variant "Since" [ f x; Syntax.Version.to_dyn v ]
    | Deleted_in (x, v, repl) ->
      variant
        "Deleted_in"
        [ f x
        ; Syntax.Version.to_dyn v
        ; List
            (List.map repl ~f:(fun pp ->
               Dyn.String (Stdlib.Format.asprintf "%a" Pp.to_fmt pp)))
        ]
    | Renamed_in (x, v, s) ->
      variant "Renamed_in" [ f x; Syntax.Version.to_dyn v; string s ]
  ;;
end

open With_versioning_info

module Env = struct
  type 'a map = 'a With_versioning_info.t String.Map.t

  type t =
    { syntax_version : Syntax.Version.t
    ; vars : Var.t map
    ; macros : Macro.t map
    }

  let syntax_version t = t.syntax_version

  let pkg =
    let macros =
      let macro (x : Macro.t) = No_info x in
      String.Map.of_list_exn [ "pkg", macro Pkg; "pkg-self", macro Pkg_self ]
    in
    let vars =
      let pkg =
        [ "switch", Var.Pkg.Switch
        ; "os", Os
        ; "os_version", Os_version
        ; "os_distribution", Os_distribution
        ; "os_family", Os_family
        ; "build", Build
        ; "prefix", Prefix
        ; "user", User
        ; "group", Group
        ; "arch", Arch
        ; "jobs", Jobs
        ; "sys_ocaml_version", Sys_ocaml_version
        ]
        |> List.rev_append
             (List.rev_map Var.Pkg.Section.all ~f:(fun (section, name) ->
                name, Var.Pkg.Section_dir section))
        |> List.rev_map ~f:(fun (x, y) -> x, No_info (Var.Pkg y))
      in
      let vars =
        ("make", No_info Var.Make) :: ("context_name", No_info Var.Context_name) :: pkg
      in
      String.Map.of_list_exn vars
    in
    fun syntax_version -> { vars; macros; syntax_version }
  ;;

  let initial =
    let macros =
      let macro (x : Macro.t) = No_info x in
      let artifact x =
        String.drop (Artifact.ext x) 1, since ~version:(2, 0) (Macro.Artifact x)
      in
      String.Map.of_list_exn
        ([ "exe", macro Exe
         ; "bin", macro Bin
         ; "lib", macro (Lib { lib_exec = false; lib_private = false })
         ; "libexec", macro (Lib { lib_exec = true; lib_private = false })
         ; ( "lib-private"
           , since ~version:(2, 1) (Macro.Lib { lib_exec = false; lib_private = true }) )
         ; ( "libexec-private"
           , since ~version:(2, 1) (Macro.Lib { lib_exec = true; lib_private = true }) )
         ; "lib-available", macro Lib_available
         ; "bin-available", since ~version:(3, 0) Macro.Bin_available
         ; "version", macro Version
         ; "read", macro Read
         ; "read-lines", macro Read_lines
         ; "read-strings", macro Read_strings
         ; "dep", since ~version:(1, 0) Macro.Dep
         ; "path", renamed_in ~version:(1, 0) ~new_name:"dep" Macro.Dep
         ; ( "findlib"
           , renamed_in
               ~version:(1, 0)
               ~new_name:"lib"
               (Macro.Lib { lib_exec = false; lib_private = false }) )
         ; "path-no-dep", deleted_in ~version:(1, 0) Macro.Path_no_dep
         ; "ocaml-config", macro Ocaml_config
         ; "env", since ~version:(1, 4) Macro.Env
         ; ( "coq"
           , macro Coq_config
             (* CR-rgrinberg: this macro is lacking the information
                for when it was introduced. Also, it should require the Coq
                syntax to be enabled to even parse *) )
         ]
         @ List.map ~f:artifact Artifact.all)
    in
    let vars =
      let lowercased : (string * Var.t With_versioning_info.t) list =
        [ "cpp", No_info Cpp
        ; "pa_cpp", No_info Pa_cpp
        ; "ocaml", No_info Ocaml
        ; "ocamlc", No_info Ocamlc
        ; "ocamlopt", No_info Ocamlopt
        ; "arch_sixtyfour", No_info Arch_sixtyfour
        ; "make", No_info Make
        ; "cc", No_info Cc
        ; "cxx", No_info Cxx
        ]
      in
      let uppercased =
        List.map lowercased ~f:(fun (k, v) ->
          match v with
          | No_info v -> String.uppercase k, renamed_in v ~new_name:k ~version:(1, 0)
          | _ -> assert false)
      in
      let other : (string * Var.t With_versioning_info.t) list =
        [ "targets", since ~version:(1, 0) Var.Targets
        ; "target", since ~version:(1, 11) Var.Target
        ; "deps", since ~version:(1, 0) Var.Deps
        ; "project_root", since ~version:(1, 0) Var.Project_root
        ; ( "<"
          , deleted_in
              Var.First_dep
              ~version:(1, 0)
              ~repl:
                [ Pp.text
                    "Use a named dependency instead:\n\n\
                    \  (deps (:x <dep>) ...)\n\
                    \   ... %{x} ..."
                ] )
        ; "@", renamed_in ~version:(1, 0) ~new_name:"targets" Var.Deps
        ; "^", renamed_in ~version:(1, 0) ~new_name:"deps" Var.Targets
        ; ( "SCOPE_ROOT"
          , renamed_in ~version:(1, 0) ~new_name:"project_root" Var.Project_root )
        ; "-verbose", deleted_in ~version:(3, 0) Var.Nothing
        ; "ocaml_bin", No_info Ocaml_bin_dir
        ; "ocaml_version", No_info Ocaml_version
        ; "ocaml_where", No_info Ocaml_stdlib_dir
        ; "ccomp_type", since ~version:(3, 0) Var.Ccomp_type
        ; "null", No_info Dev_null
        ; "ext_obj", No_info Ext_obj
        ; "ext_asm", No_info Ext_asm
        ; "ext_lib", No_info Ext_lib
        ; "ext_dll", No_info Ext_dll
        ; "ext_exe", No_info Ext_exe
        ; "ext_plugin", since ~version:(2, 4) Var.Ext_plugin
        ; "profile", No_info Profile
        ; "workspace_root", No_info Workspace_root
        ; "context_name", No_info Context_name
        ; "ROOT", renamed_in ~version:(1, 0) ~new_name:"workspace_root" Var.Workspace_root
        ; "os_type", since ~version:(1, 10) Var.Os_type
        ; "architecture", since ~version:(1, 10) Var.Architecture
        ; "system", since ~version:(1, 10) Var.System
        ; "model", since ~version:(1, 10) Var.Model
        ; "ignoring_promoted_rules", since ~version:(1, 10) Var.Ignoring_promoted_rules
        ; "library-name", No_info Library_name
        ; "partition", since ~version:(3, 8) Var.Partition
        ; "impl-files", No_info Impl_files
        ; "intf-files", No_info Intf_files
        ; "test", No_info Test
        ; "input-file", since ~version:(1, 0) Var.Input_file
        ; "corrected-suffix", No_info Corrected_suffix
        ; "inline_tests", No_info Inline_tests
        ; "toolchains", since ~version:(3, 0) Var.Toolchain
        ]
      in
      String.Map.of_list_exn (List.concat [ lowercased; uppercased; other ])
    in
    fun syntax_version -> { syntax_version; vars; macros }
  ;;

  let lt_renamed_input_file t =
    { t with
      vars =
        String.Map.set
          t.vars
          "<"
          (renamed_in ~new_name:"input-file" ~version:(1, 0) Var.Input_file)
    }
  ;;

  let parse map syntax_version (pform : Template.Pform.t) =
    let module P = Template.Pform in
    match String.Map.find map pform.name with
    | None ->
      User_error.raise
        ~loc:pform.loc
        [ Pp.textf "Unknown %s %s" (P.describe_kind pform) (P.describe pform) ]
    | Some v ->
      (match v with
       | No_info v -> v
       | Since (v, min_version) ->
         if syntax_version >= min_version
         then v
         else
           Syntax.Error.since
             (P.loc pform)
             Stanza.syntax
             min_version
             ~what:(P.describe pform)
       | Renamed_in (v, in_version, new_name) ->
         if syntax_version < in_version
         then v
         else
           Syntax.Error.renamed_in
             (P.loc pform)
             Stanza.syntax
             in_version
             ~what:(P.describe pform)
             ~to_:(P.describe { pform with name = new_name })
       | Deleted_in (v, in_version, repl) ->
         if syntax_version < in_version
         then v
         else
           Syntax.Error.deleted_in
             (P.loc pform)
             Stanza.syntax
             in_version
             ~what:(P.describe pform)
             ~repl)
  ;;

  let parse t (pform : Template.Pform.t) =
    match pform.payload with
    | None -> Var (parse t.vars t.syntax_version pform)
    | Some payload -> Macro { macro = parse t.macros t.syntax_version pform; payload }
  ;;

  let unsafe_parse_without_checking_version map (pform : Template.Pform.t) =
    let module P = Template.Pform in
    match String.Map.find map pform.name with
    | None ->
      User_error.raise
        ~loc:pform.loc
        [ Pp.textf "Unknown %s %s" (P.describe_kind pform) (P.describe pform) ]
    | Some v -> With_versioning_info.get_data v
  ;;

  let unsafe_parse_without_checking_version t (pform : Template.Pform.t) =
    match pform.payload with
    | None -> Var (unsafe_parse_without_checking_version t.vars pform)
    | Some payload ->
      Macro { macro = unsafe_parse_without_checking_version t.macros pform; payload }
  ;;

  let to_dyn { syntax_version; vars; macros } =
    let open Dyn in
    record
      [ "syntax_version", Syntax.Version.to_dyn syntax_version
      ; "vars", String.Map.to_dyn (to_dyn Var.to_dyn) vars
      ; "macros", String.Map.to_dyn (to_dyn Macro.to_dyn) macros
      ]
  ;;

  let add_user_vars t vars =
    { t with
      vars =
        List.fold_left vars ~init:t.vars ~f:(fun vars var ->
          String.Map.set vars var (No_info (Var.User_var var)))
    }
  ;;

  type stamp =
    Syntax.Version.t
    * (string * Var.t With_versioning_info.t) list
    * (string * Macro.t With_versioning_info.t) list

  let to_stamp { syntax_version; vars; macros } : stamp =
    syntax_version, String.Map.to_list vars, String.Map.to_list macros
  ;;

  let all_known { syntax_version = _; vars; macros } =
    String.Map.union
      (String.Map.map vars ~f:(fun x -> Var (With_versioning_info.get_data x)))
      (String.Map.map macros ~f:(fun x ->
         Macro { macro = With_versioning_info.get_data x; payload = Payload.of_string "" }))
      ~f:(fun _ _ _ -> assert false)
  ;;
end

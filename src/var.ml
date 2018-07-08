open Import

module Kind = struct
  type t =
    | Values of Value.t list
    | Project_root
    | First_dep
    | Deps
    | Targets

  let to_value_no_deps_or_targets t ~scope =
    match t with
    | Values v -> Some v
    | Project_root -> Some [Value.Dir (Scope.root scope)]
    | First_dep
    | Deps
    | Targets -> None
end

module Form = struct
  type t =
    | Exe
    | Dep
    | Bin
    | Lib
    | Libexec
    | Lib_available
    | Version
    | Read
    | Read_strings
    | Read_lines
    | Path_no_dep
end

type 'a t =
  | No_info    of 'a
  | Since      of 'a * Syntax.Version.t
  | Deleted_in of 'a * Syntax.Version.t
  | Renamed_in of Syntax.Version.t * string

module Map = struct
  type nonrec 'a t = 'a t String.Map.t

  let values v                      = No_info (Kind.Values v)
  let renamed_in ~new_name ~version = Renamed_in (version, new_name)
  let deleted_in ~version kind      = Deleted_in (kind, version)
  let since ~version v              = Since (v, version)

  let static_vars =
    [ "first-dep", since ~version:(1, 0) Kind.First_dep
    ; "targets", since ~version:(1, 0) Kind.Targets
    ; "deps", since ~version:(1, 0) Kind.Deps
    ; "project_root", since ~version:(1, 0) Kind.Project_root

    ; "<", renamed_in ~version:(1, 0) ~new_name:"first-dep"
    ; "@", renamed_in ~version:(1, 0) ~new_name:"targets"
    ; "^", renamed_in ~version:(1, 0) ~new_name:"deps"
    ; "SCOPE_ROOT", renamed_in ~version:(1, 0) ~new_name:"project_root"
    ]

  let forms =
    let form kind = No_info kind in
    let open Form in
    [ "exe", form Exe
    ; "bin", form Bin
    ; "lib", form Lib
    ; "libexec", form Libexec
    ; "lib-available", form Lib_available
    ; "version", form Version
    ; "read", form Read
    ; "read-lines", form Read_lines
    ; "read-strings", form Read_strings

    ; "dep", since ~version:(1, 0) Dep

    ; "path", renamed_in ~version:(1, 0) ~new_name:"dep"
    ; "findlib", renamed_in ~version:(1, 0) ~new_name:"lib"

    ; "path-no-dep", deleted_in ~version:(1, 0) Path_no_dep
    ]
    |> String.Map.of_list_exn

  let create_vars ~(context : Context.t) ~cxx_flags =
    let ocamlopt =
      match context.ocamlopt with
      | None -> Path.relative context.ocaml_bin "ocamlopt"
      | Some p -> p
    in
    let string s = values [Value.String s] in
    let path p = values [Value.Path p] in
    let make =
      match Bin.make with
      | None   -> string "make"
      | Some p -> path p
    in
    let cflags = context.ocamlc_cflags in
    let strings s = values (Value.L.strings s) in
    let lowercased =
      [ "cpp"            , strings (context.c_compiler :: cflags @ ["-E"])
      ; "cc"             , strings (context.c_compiler :: cflags)
      ; "cxx"            , strings (context.c_compiler :: cxx_flags)
      ; "ocaml"          , path context.ocaml
      ; "ocamlc"         , path context.ocamlc
      ; "ocamlopt"       , path ocamlopt
      ; "arch_sixtyfour" , string (string_of_bool context.arch_sixtyfour)
      ; "make"           , make
      ; "root"           , values [Value.Dir context.build_dir]
      ] in
    let uppercased =
      List.map lowercased ~f:(fun (k, _) ->
        (String.uppercase k, renamed_in ~new_name:k ~version:(1, 0))) in
    let vars =
      [ "-verbose"       , values []
      ; "pa_cpp"         , strings (context.c_compiler :: cflags
                                    @ ["-undef"; "-traditional";
                                       "-x"; "c"; "-E"])
      ; "ocaml_bin"      , path context.ocaml_bin
      ; "ocaml_version"  , string context.version_string
      ; "ocaml_where"    , string (Path.to_string context.stdlib_dir)
      ; "null"           , string (Path.to_string Config.dev_null)
      ; "ext_obj"        , string context.ext_obj
      ; "ext_asm"        , string context.ext_asm
      ; "ext_lib"        , string context.ext_lib
      ; "ext_dll"        , string context.ext_dll
      ; "ext_exe"        , string context.ext_exe
      ; "profile"        , string context.profile
      ]
    in
    let ocaml_config =
      List.map (Ocaml_config.to_list context.ocaml_config) ~f:(fun (k, v) ->
        ("ocaml-config:" ^ k,
         match (v : Ocaml_config.Value.t) with
         | Bool   x -> string (string_of_bool x)
         | Int    x -> string (string_of_int x)
         | String x -> string x
         | Words  x -> strings x
         | Prog_and_args x -> strings (x.prog :: x.args)))
    in
    [ ocaml_config
    ; static_vars
    ; lowercased
    ; uppercased
    ; vars
    ]
    |> List.concat
    |> String.Map.of_list_exn

  let static_vars = String.Map.of_list_exn static_vars

  let rec expand t ~syntax_version ~var =
    let name = String_with_vars.Var.name var in
    Option.bind (String.Map.find t name) ~f:(fun v ->
      let what var =
        sprintf "Variable %s" (String_with_vars.Var.to_string var) in
      match v with
      | No_info v -> Some v
      | Since (v, min_version) ->
        if syntax_version >= min_version then
          Some v
        else
          Syntax.Error.since (String_with_vars.Var.loc var)
            Stanza.syntax syntax_version
            ~what:(what var)
      | Renamed_in (in_version, new_name) -> begin
          if syntax_version >= in_version then
            let var =
              if String_with_vars.Var.is_form var then
                String_with_vars.Var.with_payload var ~payload:(Some "..")
              else
                var
            in
            Syntax.Error.renamed_in (String_with_vars.Var.loc var)
              Stanza.syntax syntax_version
              ~what:(what var)
              ~to_:(let open String_with_vars.Var in
                    to_string (with_name var ~name:new_name))
          else
            expand t ~syntax_version:in_version
              ~var:(String_with_vars.Var.with_name var ~name:new_name)
        end
      | Deleted_in (v, in_version) ->
        if syntax_version < in_version then
          Some v
        else
          Syntax.Error.deleted_in (String_with_vars.Var.loc var)
            Stanza.syntax syntax_version ~what:(what var))
end

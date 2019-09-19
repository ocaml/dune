open! Stdune
open Import
open Dune_lang.Decoder

(* This file defines the jbuild types as well as the S-expression syntax for
   the various supported version of the specification. *)

(* Deprecated *)
module Jbuild_version = struct
  type t = V1

  let decode = enum [ ("1", V1) ]
end

let invalid_module_name ~loc name =
  User_error.raise ~loc [ Pp.textf "invalid module name: %S" name ]

let module_name =
  plain_string (fun ~loc name ->
      match name with
      | "" -> invalid_module_name ~loc name
      | s -> (
        try
          ( match s.[0] with
          | 'A' .. 'Z'
           |'a' .. 'z' ->
            ()
          | _ -> raise_notrace Exit );
          String.iter s ~f:(function
            | 'A' .. 'Z'
             |'a' .. 'z'
             |'0' .. '9'
             |'\''
             |'_' ->
              ()
            | _ -> raise_notrace Exit);
          Module_name.of_string s
        with Exit -> invalid_module_name ~loc name ))

let relative_file =
  plain_string (fun ~loc fn ->
      if Filename.is_relative fn then
        fn
      else
        User_error.raise ~loc [ Pp.textf "relative filename expected" ])

let library_variants =
  let syntax =
    Dune_lang.Syntax.create ~name:"library_variants"
      ~desc:"the experimental library variants feature." [ (0, 2) ]
  in
  Dune_project.Extension.register_simple ~experimental:true syntax
    (Dune_lang.Decoder.return []);
  syntax

let variants_field =
  field_o "variants"
    (let* () = Dune_lang.Syntax.since library_variants (0, 1) in
     located (repeat Variant.decode >>| Variant.Set.of_list))

(* Parse and resolve "package" fields *)
module Pkg = struct
  let listing packages =
    let longest_pkg =
      String.longest_map packages ~f:(fun p ->
          Package.Name.to_string p.Package.name)
    in
    Pp.enumerate packages ~f:(fun pkg ->
        Printf.ksprintf Pp.verbatim "%-*s (because of %s)" longest_pkg
          (Package.Name.to_string pkg.Package.name)
          (Path.Source.to_string (Package.opam_file pkg)))

  let default (project : Dune_project.t) stanza =
    match Package.Name.Map.values (Dune_project.packages project) with
    | [ pkg ] -> Ok pkg
    | [] ->
      Error
        (User_error.make
           [ Pp.text
               "The current project defines some public elements, but no opam \
                packages are defined."
           ; Pp.text
               "Please add a <package>.opam file at the project root so that \
                these elements are installed into it."
           ])
    | _ :: _ :: _ ->
      Error
        (User_error.make
           [ Pp.text
               "I can't determine automatically which package this stanza is \
                for."
           ; Pp.text "I have the choice between these ones:"
           ; listing (Package.Name.Map.values (Dune_project.packages project))
           ; Pp.textf
               "You need to add a (package ...) field to this (%s) stanza."
               stanza
           ])

  let default_exn ~loc project stanza =
    match default project stanza with
    | Ok p -> p
    | Error msg -> raise (User_error.E { msg with loc = Some loc })

  let resolve (project : Dune_project.t) name =
    let packages = Dune_project.packages project in
    match Package.Name.Map.find packages name with
    | Some pkg -> Ok pkg
    | None ->
      let name_s = Package.Name.to_string name in
      if Package.Name.Map.is_empty packages then
        Error
          (User_error.make
             [ Pp.text
                 "You cannot declare items to be installed without adding a \
                  <package>.opam file at the root of your project."
             ; Pp.textf
                 "To declare elements to be installed as part of package %S, \
                  add a %S file at the root of your project."
                 name_s
                 (Package.Name.opam_fn name)
             ; Pp.textf "Root of the project as discovered by dune: %s"
                 (Path.Source.to_string_maybe_quoted
                    (Dune_project.root project))
             ])
      else
        Error
          (User_error.make
             [ Pp.textf "The current scope doesn't define package %S." name_s
             ; Pp.text
                 "The only packages for which you can declare elements to be \
                  installed in this directory are:"
             ; listing (Package.Name.Map.values packages)
             ]
             ~hints:
               (User_message.did_you_mean name_s
                  ~candidates:
                    ( Package.Name.Map.keys packages
                    |> List.map ~f:Package.Name.to_string )))

  let decode =
    let+ p = Dune_project.get_exn ()
    and+ loc, name = located Package.Name.decode in
    match resolve p name with
    | Ok x -> x
    | Error e -> raise (User_error.E { e with loc = Some loc })

  let field stanza =
    map_validate
      (let+ p = Dune_project.get_exn ()
       and+ pkg = field_o "package" string in
       (p, pkg))
      ~f:(fun (p, pkg) ->
        match pkg with
        | None -> default p stanza
        | Some name -> resolve p (Package.Name.of_string name))
end

module Pps_and_flags = struct
  let decode =
    let+ l, flags =
      until_keyword "--" ~before:String_with_vars.decode
        ~after:(repeat String_with_vars.decode)
    and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
    let pps, more_flags =
      List.partition_map l ~f:(fun s ->
          match String_with_vars.is_prefix ~prefix:"-" s with
          | Yes -> Right s
          | No
           |Unknown _ -> (
            let loc = String_with_vars.loc s in
            match String_with_vars.text_only s with
            | None ->
              User_error.raise ~loc
                [ Pp.text "No variables allowed in ppx library names" ]
            | Some txt -> Left (loc, Lib_name.of_string_exn ~loc:(Some loc) txt)
            ))
    in
    let all_flags = more_flags @ Option.value flags ~default:[] in
    if syntax_version < (1, 10) then
      List.iter
        ~f:(fun flag ->
          if String_with_vars.has_vars flag then
            Dune_lang.Syntax.Error.since
              (String_with_vars.loc flag)
              Stanza.syntax (1, 10) ~what:"Using variables in pps flags")
        all_flags;
    (pps, all_flags)
end

module Dep_conf = struct
  type t =
    | File of String_with_vars.t
    | Alias of String_with_vars.t
    | Alias_rec of String_with_vars.t
    | Glob_files of String_with_vars.t
    | Source_tree of String_with_vars.t
    | Package of String_with_vars.t
    | Universe
    | Env_var of String_with_vars.t
    | Sandbox_config of Sandbox_config.t

  let remove_locs = function
    | File sw -> File (String_with_vars.remove_locs sw)
    | Alias sw -> Alias (String_with_vars.remove_locs sw)
    | Alias_rec sw -> Alias_rec (String_with_vars.remove_locs sw)
    | Glob_files sw -> Glob_files (String_with_vars.remove_locs sw)
    | Source_tree sw -> Source_tree (String_with_vars.remove_locs sw)
    | Package sw -> Package (String_with_vars.remove_locs sw)
    | Universe -> Universe
    | Env_var sw -> Env_var sw
    | Sandbox_config s -> Sandbox_config s

  let decode_sandbox_config =
    let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 12)
    and+ loc, x =
      located
        (repeat
           (sum
              [ ("none", return Sandbox_config.Partial.no_sandboxing)
              ; ("always", return Sandbox_config.Partial.needs_sandboxing)
              ; ( "preserve_file_kind"
                , return (Sandbox_config.Partial.disallow Sandbox_mode.symlink)
                )
              ]))
    in
    Sandbox_config.Partial.merge ~loc x

  let decode =
    let decode =
      let sw = String_with_vars.decode in
      sum
        [ ("file", sw >>| fun x -> File x)
        ; ("alias", sw >>| fun x -> Alias x)
        ; ("alias_rec", sw >>| fun x -> Alias_rec x)
        ; ("glob_files", sw >>| fun x -> Glob_files x)
        ; ("package", sw >>| fun x -> Package x)
        ; ("universe", return Universe)
        ; ( "files_recursively_in"
          , let+ () =
              Dune_lang.Syntax.renamed_in Stanza.syntax (1, 0)
                ~to_:"source_tree"
            and+ x = sw in
            Source_tree x )
        ; ( "source_tree"
          , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
            and+ x = sw in
            Source_tree x )
        ; ("env_var", sw >>| fun x -> Env_var x)
        ; ("sandbox", decode_sandbox_config >>| fun x -> Sandbox_config x)
        ]
    in
    if_list ~then_:decode ~else_:(String_with_vars.decode >>| fun x -> File x)

  open Dune_lang

  let encode = function
    | File t ->
      List [ Dune_lang.unsafe_atom_of_string "file"; String_with_vars.encode t ]
    | Alias t ->
      List
        [ Dune_lang.unsafe_atom_of_string "alias"; String_with_vars.encode t ]
    | Alias_rec t ->
      List
        [ Dune_lang.unsafe_atom_of_string "alias_rec"
        ; String_with_vars.encode t
        ]
    | Glob_files t ->
      List
        [ Dune_lang.unsafe_atom_of_string "glob_files"
        ; String_with_vars.encode t
        ]
    | Source_tree t ->
      List
        [ Dune_lang.unsafe_atom_of_string "source_tree"
        ; String_with_vars.encode t
        ]
    | Package t ->
      List
        [ Dune_lang.unsafe_atom_of_string "package"; String_with_vars.encode t ]
    | Universe -> Dune_lang.unsafe_atom_of_string "universe"
    | Env_var t ->
      List
        [ Dune_lang.unsafe_atom_of_string "env_var"; String_with_vars.encode t ]
    | Sandbox_config config ->
      if Sandbox_config.equal config Sandbox_config.no_special_requirements
      then
        List []
      else
        Code_error.raise "There's no syntax for [Sandbox_config] yet" []

  let to_dyn t = Dune_lang.to_dyn (encode t)
end

module Preprocess = struct
  module Pps = struct
    type t =
      { loc : Loc.t
      ; pps : (Loc.t * Lib_name.t) list
      ; flags : String_with_vars.t list
      ; staged : bool
      }

    let compare_no_locs { loc = _; pps = pps1; flags = flags1; staged = s1 }
        { loc = _; pps = pps2; flags = flags2; staged = s2 } =
      match Bool.compare s1 s2 with
      | (Lt | Gt) as t -> t
      | Eq -> (
        match
          List.compare flags1 flags2 ~compare:String_with_vars.compare_no_loc
        with
        | (Lt | Gt) as t -> t
        | Eq ->
          List.compare pps1 pps2 ~compare:(fun (_, x) (_, y) ->
              Lib_name.compare x y) )
  end

  type t =
    | No_preprocessing
    | Action of Loc.t * Action_dune_lang.t
    | Pps of Pps.t
    | Future_syntax of Loc.t

  let decode =
    sum
      [ ("no_preprocessing", return No_preprocessing)
      ; ( "action"
        , located Action_dune_lang.decode >>| fun (loc, x) -> Action (loc, x)
        )
      ; ( "pps"
        , let+ loc = loc
          and+ pps, flags = Pps_and_flags.decode in
          Pps { loc; pps; flags; staged = false } )
      ; ( "staged_pps"
        , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 1)
          and+ loc = loc
          and+ pps, flags = Pps_and_flags.decode in
          Pps { loc; pps; flags; staged = true } )
      ; ( "future_syntax"
        , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 8)
          and+ loc = loc in
          Future_syntax loc )
      ]

  let loc = function
    | No_preprocessing -> None
    | Action (loc, _)
     |Pps { loc; _ }
     |Future_syntax loc ->
      Some loc

  let pps = function
    | Pps { pps; _ } -> pps
    | _ -> []

  module Without_future_syntax = struct
    type t =
      | No_preprocessing
      | Action of Loc.t * Action_dune_lang.t
      | Pps of Pps.t
  end

  module Pp_flag_consumer = struct
    (* Compiler allows the output of [-pp] to be a binary AST. Merlin requires
       that to be a text file instead. *)
    type t =
      | Compiler
      | Merlin
  end

  let remove_future_syntax t ~(for_ : Pp_flag_consumer.t) v :
      Without_future_syntax.t =
    match t with
    | No_preprocessing -> No_preprocessing
    | Action (loc, action) -> Action (loc, action)
    | Pps pps -> Pps pps
    | Future_syntax loc ->
      if Ocaml_version.supports_let_syntax v then
        No_preprocessing
      else
        Action
          ( loc
          , Run
              ( String_with_vars.make_var loc "bin"
                  ~payload:"ocaml-syntax-shims"
              , ( match for_ with
                | Compiler -> [ String_with_vars.make_text loc "-dump-ast" ]
                | Merlin ->
                  (* We generate a text file instead of AST. That gives you
                     less precise locations, but at least Merlin doesn't fail
                     outright.

                     In general this hack should be applied to all -pp commands
                     that might produce an AST, not just to Future_syntax. But
                     doing so means we need to change dune language so the user
                     can provide two versions of the command.

                     Hopefully this will be fixed in merlin before that becomes
                     a necessity. *)
                  [] )
                @ [ String_with_vars.make_var loc "input-file" ] ) )
end

let enabled_if ~since =
  let decode =
    match since with
    | None -> Blang.decode
    | Some since -> Dune_lang.Syntax.since Stanza.syntax since >>> Blang.decode
  in
  field "enabled_if" ~default:Blang.true_ decode

module Per_module = struct
  include Per_item.Make (Module_name)

  let decode ~default a =
    peek_exn
    >>= function
    | List (loc, Atom (_, A "per_module") :: _) ->
      sum
        [ ( "per_module"
          , let+ x =
              repeat
                (let+ pp, names = pair a (repeat module_name) in
                 (names, pp))
            in
            of_mapping x ~default
            |> function
            | Ok t -> t
            | Error (name, _, _) ->
              User_error.raise ~loc
                [ Pp.textf "module %s present in two different sets"
                    (Module_name.to_string name)
                ] )
        ]
    | _ -> a >>| for_all
end

module Preprocess_map = struct
  type t = Preprocess.t Per_module.t

  let decode =
    Per_module.decode Preprocess.decode ~default:Preprocess.No_preprocessing

  let no_preprocessing = Per_module.for_all Preprocess.No_preprocessing

  let find module_name t = Per_module.get t module_name

  let default = Per_module.for_all Preprocess.No_preprocessing

  let pps t =
    Per_module.fold t ~init:Lib_name.Map.empty ~f:(fun pp acc ->
        List.fold_left (Preprocess.pps pp) ~init:acc ~f:(fun acc (loc, pp) ->
            Lib_name.Map.set acc pp loc))
    |> Lib_name.Map.foldi ~init:[] ~f:(fun pp loc acc -> (loc, pp) :: acc)
end

module Lint = struct
  type t = Preprocess_map.t

  let decode = Preprocess_map.decode

  let default = Preprocess_map.default

  let no_lint = default
end

let field_oslu name = Ordered_set_lang.Unexpanded.field name

module Js_of_ocaml = struct
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; javascript_files : string list
    }

  let decode =
    fields
      (let+ flags = field_oslu "flags"
       and+ javascript_files =
         field "javascript_files" (repeat string) ~default:[]
       in
       { flags; javascript_files })

  let default =
    { flags = Ordered_set_lang.Unexpanded.standard; javascript_files = [] }
end

module Lib_deps = struct
  type t = Lib_dep.t list

  type kind =
    | Required
    | Optional
    | Forbidden

  let decode ~allow_re_export =
    let+ loc = loc
    and+ t = repeat (Lib_dep.decode ~allow_re_export) in
    let add kind name acc =
      match Lib_name.Map.find acc name with
      | None -> Lib_name.Map.set acc name kind
      | Some kind' -> (
        match (kind, kind') with
        | Required, Required ->
          User_error.raise ~loc
            [ Pp.textf "library %S is present twice" (Lib_name.to_string name) ]
        | (Optional | Forbidden), (Optional | Forbidden) -> acc
        | Optional, Required
         |Required, Optional ->
          User_error.raise ~loc
            [ Pp.textf
                "library %S is present both as an optional and required \
                 dependency"
                (Lib_name.to_string name)
            ]
        | Forbidden, Required
         |Required, Forbidden ->
          User_error.raise ~loc
            [ Pp.textf
                "library %S is present both as a forbidden and required \
                 dependency"
                (Lib_name.to_string name)
            ] )
    in
    ignore
      ( List.fold_left t ~init:Lib_name.Map.empty ~f:(fun acc x ->
            match x with
            | Lib_dep.Re_export (_, s)
             |Lib_dep.Direct (_, s) ->
              add Required s acc
            | Select { choices; _ } ->
              List.fold_left choices ~init:acc
                ~f:(fun acc (c : Lib_dep.Select.choice) ->
                  let acc =
                    Lib_name.Set.fold c.required ~init:acc ~f:(add Optional)
                  in
                  Lib_name.Set.fold c.forbidden ~init:acc ~f:(add Forbidden)))
        : kind Lib_name.Map.t );
    t

  let of_pps pps = List.map pps ~f:(fun pp -> Lib_dep.direct (Loc.none, pp))

  let info t ~kind =
    List.concat_map t ~f:(function
      | Lib_dep.Re_export (_, s)
       |Lib_dep.Direct (_, s) ->
        [ (s, kind) ]
      | Select { choices; _ } ->
        List.concat_map choices ~f:(fun (c : Lib_dep.Select.choice) ->
            Lib_name.Set.to_list c.required
            |> List.map ~f:(fun d -> (d, Lib_deps_info.Kind.Optional))))
    |> Lib_name.Map.of_list_reduce ~f:Lib_deps_info.Kind.merge
end

let modules_field name = Ordered_set_lang.field name

module Buildable = struct
  type t =
    { loc : Loc.t
    ; modules : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; c_flags : Ordered_set_lang.Unexpanded.t C.Kind.Dict.t
    ; c_names : Ordered_set_lang.t option
    ; cxx_names : Ordered_set_lang.t option
    ; preprocess : Preprocess_map.t
    ; preprocessor_deps : Loc.t * Dep_conf.t list
    ; lint : Preprocess_map.t
    ; flags : Ocaml_flags.Spec.t
    ; js_of_ocaml : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    }

  let decode ~since_c ~allow_re_export =
    let check_c t =
      match since_c with
      | None -> t
      | Some v -> Dune_lang.Syntax.since Stanza.syntax v >>> t
    in
    let+ loc = loc
    and+ preprocess =
      field "preprocess" Preprocess_map.decode ~default:Preprocess_map.default
    and+ preprocessor_deps =
      located (field "preprocessor_deps" (repeat Dep_conf.decode) ~default:[])
    and+ lint = field "lint" Lint.decode ~default:Lint.default
    and+ c_flags = Dune_env.Stanza.c_flags ~since:since_c
    and+ c_names = field_o "c_names" (check_c Ordered_set_lang.decode)
    and+ cxx_names = field_o "cxx_names" (check_c Ordered_set_lang.decode)
    and+ modules = modules_field "modules"
    and+ modules_without_implementation =
      modules_field "modules_without_implementation"
    and+ libraries =
      field "libraries" (Lib_deps.decode ~allow_re_export) ~default:[]
    and+ flags = Ocaml_flags.Spec.decode
    and+ js_of_ocaml =
      field "js_of_ocaml" Js_of_ocaml.decode ~default:Js_of_ocaml.default
    and+ allow_overlapping_dependencies =
      field_b "allow_overlapping_dependencies"
    in
    { loc
    ; preprocess
    ; preprocessor_deps
    ; lint
    ; modules
    ; modules_without_implementation
    ; c_flags
    ; c_names
    ; cxx_names
    ; libraries
    ; flags
    ; js_of_ocaml
    ; allow_overlapping_dependencies
    }

  let single_preprocess t =
    if Per_module.is_constant t.preprocess then
      Per_module.get t.preprocess (Module_name.of_string "")
    else
      Preprocess.No_preprocessing
end

module Public_lib = struct
  type t =
    { name : Loc.t * Lib_name.t
    ; package : Package.t
    ; sub_dir : string option
    }

  let name t = snd t.name

  let public_name_field =
    map_validate
      (let+ project = Dune_project.get_exn ()
       and+ loc_name = field_o "public_name" (located Lib_name.decode) in
       (project, loc_name))
      ~f:(fun (project, loc_name) ->
        match loc_name with
        | None -> Ok None
        | Some ((_, s) as loc_name) -> (
          let pkg, rest = Lib_name.split s in
          match Pkg.resolve project pkg with
          | Ok pkg ->
            Ok
              (Some
                 { package = pkg
                 ; sub_dir =
                     ( if rest = [] then
                       None
                     else
                       Some (String.concat rest ~sep:"/") )
                 ; name = loc_name
                 })
          | Error _ as e -> e ))
end

module Mode_conf = struct
  module T = struct
    type t =
      | Byte
      | Native
      | Best

    let compare (a : t) b = compare a b
  end

  include T

  let decode = enum [ ("byte", Byte); ("native", Native); ("best", Best) ]

  let to_string = function
    | Byte -> "byte"
    | Native -> "native"
    | Best -> "best"

  let to_dyn t =
    let open Dyn.Encoder in
    constr (to_string t) []

  let encode t = Dune_lang.unsafe_atom_of_string (to_string t)

  module Kind = struct
    type t =
      | Inherited
      | Requested of Loc.t
  end

  module Map = struct
    type nonrec 'a t =
      { byte : 'a
      ; native : 'a
      ; best : 'a
      }

    let find t = function
      | Byte -> t.byte
      | Native -> t.native
      | Best -> t.best

    let update t key ~f =
      match key with
      | Byte -> { t with byte = f t.byte }
      | Native -> { t with native = f t.native }
      | Best -> { t with best = f t.best }

    let make_one x = { byte = x; native = x; best = x }
  end

  module Set = struct
    type nonrec t = Kind.t option Map.t

    let empty : t = Map.make_one None

    let of_list (input : (T.t * Kind.t) list) : t =
      List.fold_left ~init:empty input ~f:(fun acc (key, kind) ->
          Map.update acc key ~f:(function
            | None -> Some kind
            | Some (Kind.Requested loc) ->
              User_error.raise ~loc [ Pp.textf "already configured" ]
            | Some Inherited ->
              (* this doesn't happen as inherited can't be manually specified *)
              assert false))

    let decode =
      let decode =
        let+ loc, t = located decode in
        (t, Kind.Requested loc)
      in
      repeat decode >>| of_list

    let default = of_list [ (Byte, Inherited); (Best, Requested Loc.none) ]

    let eval t ~has_native =
      let exists = function
        | Best
         |Byte ->
          true
        | Native -> has_native
      in
      let get key =
        match Map.find t key with
        | None -> false
        | Some Kind.Inherited -> exists key
        | Some (Kind.Requested loc) ->
          exists key
          || User_error.raise ~loc [ Pp.text "this mode isn't available" ]
      in
      let best_mode =
        if has_native then
          Native
        else
          Byte
      in
      let best = get Best in
      let byte = get Byte || (best && best_mode = Byte) in
      let native = get Native || (best && best_mode = Native) in
      { Mode.Dict.byte; native }
  end
end

module External_variant = struct
  type t =
    { implementation : Loc.t * Lib_name.t
    ; virtual_lib : Loc.t * Lib_name.t
    ; variant : Variant.t
    ; project : Dune_project.t
    ; loc : Loc.t
    }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ loc = loc
       and+ variant = field "variant" Variant.decode
       and+ virtual_lib = field "virtual_library" (located Lib_name.decode)
       and+ implementation = field "implementation" (located Lib_name.decode)
       and+ project = Dune_project.get_exn () in
       { implementation; virtual_lib; variant; project; loc })
end

module Library = struct
  module Inherited = struct
    type 'a t =
      | This of 'a
      | From of (Loc.t * Lib_name.t)
  end

  module Special_builtin_support = struct
    module Build_info = struct
      type api_version = V1

      let supported_api_versions = [ (1, V1) ]

      type t =
        { data_module : string
        ; api_version : api_version
        }

      let decode =
        fields
          (let+ data_module = field "data_module" string
           and+ api_version =
             field "api_version"
               (let+ loc = loc
                and+ ver = int in
                match List.assoc supported_api_versions ver with
                | Some x -> x
                | None ->
                  User_error.raise ~loc
                    [ Pp.textf
                        "API version %d is not supported. Only the following \
                         versions are currently supported:"
                        ver
                    ; Pp.enumerate supported_api_versions ~f:(fun (n, _) ->
                          Pp.textf "%d" n)
                    ])
           in
           { data_module; api_version })

      let encode { data_module; api_version } =
        let open Dune_lang.Encoder in
        record_fields
          [ field "data_module" string data_module
          ; field "api_version" int
              ( match api_version with
              | V1 -> 1 )
          ]
    end

    type t =
      | Findlib_dynload
      | Build_info of Build_info.t

    let decode =
      sum
        [ ("findlib_dynload", return Findlib_dynload)
        ; ( "build_info"
          , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 11)
            and+ info = Build_info.decode in
            Build_info info )
        ]

    let encode t =
      match t with
      | Findlib_dynload -> Dune_lang.atom "findlib_dynload"
      | Build_info x ->
        Dune_lang.List (Dune_lang.atom "build_info" :: Build_info.encode x)
  end

  module Stdlib = struct
    type t =
      { modules_before_stdlib : Module_name.Set.t
      ; exit_module : Module_name.t option
      ; internal_modules : Glob.t
      }

    let syntax =
      let syntax =
        Dune_lang.Syntax.create
          ~name:"experimental_building_ocaml_compiler_with_dune"
          ~desc:"experimental feature for building the compiler with dune"
          [ (0, 1) ]
      in
      Dune_project.Extension.register_simple ~experimental:true syntax
        (Dune_lang.Decoder.return []);
      syntax

    let decode =
      fields
        (let+ modules_before_stdlib =
           field "modules_before_stdlib" (repeat module_name) ~default:[]
         and+ exit_module = field_o "exit_module" module_name
         and+ internal_modules =
           field "internal_modules" Glob.decode ~default:Glob.empty
         in
         { modules_before_stdlib =
             Module_name.Set.of_list modules_before_stdlib
         ; exit_module
         ; internal_modules
         })
  end

  module Wrapped = struct
    include Wrapped

    let default = Simple true

    let make ~wrapped ~implements ~special_builtin_support : t Inherited.t =
      ( match (wrapped, special_builtin_support) with
      | Some (loc, Yes_with_transition _), Some _ ->
        User_error.raise ~loc
          [ Pp.text
              "Cannot have transition modules for libraries with special \
               builtin support"
          ]
      | _, _ -> () );
      match (wrapped, implements) with
      | None, None -> This default
      | None, Some w -> From w
      | Some (_loc, w), None -> This w
      | Some (loc, _), Some _ ->
        User_error.raise ~loc
          [ Pp.text
              "Wrapped cannot be set for implementations. It is inherited \
               from the virtual library."
          ]

    let field = field_o "wrapped" (located decode)
  end

  type t =
    { name : Loc.t * Lib_name.Local.t
    ; public : Public_lib.t option
    ; synopsis : string option
    ; install_c_headers : string list
    ; ppx_runtime_libraries : (Loc.t * Lib_name.t) list
    ; modes : Mode_conf.Set.t
    ; kind : Lib_kind.t
    ; library_flags : Ordered_set_lang.Unexpanded.t
    ; c_library_flags : Ordered_set_lang.Unexpanded.t
    ; self_build_stubs_archive : string option
    ; virtual_deps : (Loc.t * Lib_name.t) list
    ; wrapped : Wrapped.t Inherited.t
    ; optional : bool
    ; buildable : Buildable.t
    ; dynlink : Dynlink_supported.t
    ; project : Dune_project.t
    ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
    ; no_keep_locs : bool
    ; dune_version : Dune_lang.Syntax.Version.t
    ; virtual_modules : Ordered_set_lang.t option
    ; implements : (Loc.t * Lib_name.t) option
    ; variant : Variant.t option
    ; default_implementation : (Loc.t * Lib_name.t) option
    ; private_modules : Ordered_set_lang.t option
    ; stdlib : Stdlib.t option
    ; special_builtin_support : Special_builtin_support.t option
    ; enabled_if : Blang.t
    }

  let decode =
    fields
      (let+ buildable = Buildable.decode ~since_c:None ~allow_re_export:true
       and+ loc = loc
       and+ name = field_o "name" Lib_name.Local.decode_loc
       and+ public = Public_lib.public_name_field
       and+ synopsis = field_o "synopsis" string
       and+ install_c_headers =
         field "install_c_headers" (repeat string) ~default:[]
       and+ ppx_runtime_libraries =
         field "ppx_runtime_libraries"
           (repeat (located Lib_name.decode))
           ~default:[]
       and+ library_flags = field_oslu "library_flags"
       and+ c_library_flags = field_oslu "c_library_flags"
       and+ virtual_deps =
         field "virtual_deps" (repeat (located Lib_name.decode)) ~default:[]
       and+ modes =
         field "modes" Mode_conf.Set.decode ~default:Mode_conf.Set.default
       and+ kind = field "kind" Lib_kind.decode ~default:Lib_kind.Normal
       and+ wrapped = Wrapped.field
       and+ optional = field_b "optional"
       and+ self_build_stubs_archive =
         located
           (field "self_build_stubs_archive" (option string) ~default:None)
       and+ no_dynlink = field_b "no_dynlink"
       and+ no_keep_locs =
         field_b "no_keep_locs"
           ~check:(Dune_lang.Syntax.deprecated_in Stanza.syntax (1, 7))
       and+ sub_systems =
         let* () = return () in
         Sub_system_info.record_parser ()
       and+ project = Dune_project.get_exn ()
       and+ dune_version = Dune_lang.Syntax.get_exn Stanza.syntax
       and+ virtual_modules =
         field_o "virtual_modules"
           ( Dune_lang.Syntax.since Stanza.syntax (1, 7)
           >>> Ordered_set_lang.decode )
       and+ implements =
         field_o "implements"
           ( Dune_lang.Syntax.since Stanza.syntax (1, 7)
           >>> located Lib_name.decode )
       and+ variant =
         field_o "variant"
           ( Dune_lang.Syntax.since library_variants (0, 1)
           >>> located Variant.decode )
       and+ default_implementation =
         field_o "default_implementation"
           ( Dune_lang.Syntax.since library_variants (0, 1)
           >>> located Lib_name.decode )
       and+ private_modules =
         field_o "private_modules"
           (let* () = Dune_lang.Syntax.since Stanza.syntax (1, 2) in
            Ordered_set_lang.decode)
       and+ stdlib =
         field_o "stdlib"
           (Dune_lang.Syntax.since Stdlib.syntax (0, 1) >>> Stdlib.decode)
       and+ special_builtin_support =
         field_o "special_builtin_support"
           ( Dune_lang.Syntax.since Stanza.syntax (1, 10)
           >>> Special_builtin_support.decode )
       and+ enabled_if = enabled_if ~since:(Some (1, 10)) in
       let wrapped =
         Wrapped.make ~wrapped ~implements ~special_builtin_support
       in
       let name =
         let open Dune_lang.Syntax.Version.Infix in
         match (name, public) with
         | Some (loc, res), _ -> (loc, Lib_name.Local.validate (loc, res))
         | None, Some { name = loc, name; _ } ->
           if dune_version >= (1, 1) then
             match Lib_name.to_local name with
             | Ok m -> (loc, m)
             | Error () ->
               User_error.raise ~loc
                 [ Pp.textf "Invalid library name."
                 ; Pp.text
                     "Public library names don't have this restriction. You \
                      can either change this public name to be a valid \
                      library name or add a \"name\" field with a valid \
                      library name."
                 ]
                 ~hints:[ Lib_name.Local.valid_format_doc ]
           else
             User_error.raise ~loc
               [ Pp.text
                   "name field cannot be omitted before version 1.1 of the \
                    dune language"
               ]
         | None, None ->
           User_error.raise ~loc
             [ Pp.text
                 ( if dune_version >= (1, 1) then
                   "supply at least least one of name or public_name fields"
                 else
                   "name field is missing" )
             ]
       in
       Option.both virtual_modules implements
       |> Option.iter ~f:(fun (virtual_modules, (_, impl)) ->
              User_error.raise
                ~loc:(Ordered_set_lang.loc virtual_modules |> Option.value_exn)
                [ Pp.textf "A library cannot be both virtual and implement %s"
                    (Lib_name.to_string impl)
                ]);
       match (virtual_modules, default_implementation) with
       | None, Some (loc, _) ->
         User_error.raise ~loc
           [ Pp.text
               "Only virtual libraries can specify a default implementation."
           ]
       | _ -> (
         ();
         match (implements, variant) with
         | None, Some (loc, _) ->
           User_error.raise ~loc
             [ Pp.text "Only implementations can specify a variant." ]
         | _ ->
           ();
           let variant = Option.map variant ~f:(fun (_, v) -> v) in
           let self_build_stubs_archive =
             let loc, self_build_stubs_archive = self_build_stubs_archive in
             let err =
               match
                 ( buildable.c_names
                 , buildable.cxx_names
                 , self_build_stubs_archive )
               with
               | _, _, None -> None
               | Some _, _, Some _ -> Some "c_names"
               | _, Some _, Some _ -> Some "cxx_names"
               | None, None, _ -> None
             in
             match err with
             | None -> self_build_stubs_archive
             | Some name ->
               User_error.raise ~loc
                 [ Pp.textf
                     "A library cannot use (self_build_stubs_archive ...) and \
                      (%s ...) simultaneously."
                     name
                 ]
           in
           Blang.fold_vars enabled_if ~init:() ~f:(fun var () ->
               match
                 ( String_with_vars.Var.name var
                 , String_with_vars.Var.payload var )
               with
               | var, None
                 when List.mem var ~set:Lib_config.allowed_in_enabled_if ->
                 ()
               | _ ->
                 User_error.raise
                   ~loc:(String_with_vars.Var.loc var)
                   [ Pp.textf
                       "Only %s are allowed in the 'enabled_if' field of \
                        libraries."
                       (String.enumerate_and Lib_config.allowed_in_enabled_if)
                   ]);
           { name
           ; public
           ; synopsis
           ; install_c_headers
           ; ppx_runtime_libraries
           ; modes
           ; kind
           ; library_flags
           ; c_library_flags
           ; self_build_stubs_archive
           ; virtual_deps
           ; wrapped
           ; optional
           ; buildable
           ; dynlink = Dynlink_supported.of_bool (not no_dynlink)
           ; project
           ; sub_systems
           ; no_keep_locs
           ; dune_version
           ; virtual_modules
           ; implements
           ; variant
           ; default_implementation
           ; private_modules
           ; stdlib
           ; special_builtin_support
           ; enabled_if
           } ))

  let has_stubs t =
    match
      (t.buildable.c_names, t.buildable.cxx_names, t.self_build_stubs_archive)
    with
    | None, None, None -> false
    | _ -> true

  let stubs_name t =
    let name =
      match t.self_build_stubs_archive with
      | None -> Lib_name.Local.to_string (snd t.name)
      | Some s -> s
    in
    name ^ "_stubs"

  let stubs t ~dir = Path.Build.relative dir (stubs_name t)

  let stubs_archive t ~dir ~ext_lib =
    Path.Build.relative dir (sprintf "lib%s%s" (stubs_name t) ext_lib)

  let dll t ~dir ~ext_dll =
    Path.Build.relative dir (sprintf "dll%s%s" (stubs_name t) ext_dll)

  let archive t ~dir ~ext =
    Path.Build.relative dir (Lib_name.Local.to_string (snd t.name) ^ ext)

  let best_name t =
    match t.public with
    | None -> Lib_name.of_local t.name
    | Some p -> snd p.name

  let is_virtual t = Option.is_some t.virtual_modules

  let is_impl t = Option.is_some t.implements

  let obj_dir ~dir t =
    Obj_dir.make_lib ~dir
      ~has_private_modules:(t.private_modules <> None)
      (snd t.name)

  module Main_module_name = struct
    type t = Module_name.t option Inherited.t
  end

  let main_module_name t : Main_module_name.t =
    match (t.implements, t.wrapped) with
    | Some x, From _ -> From x
    | Some _, This _ (* cannot specify for wrapped for implements *)
     |None, From _ ->
      assert false (* cannot inherit for normal libs *)
    | None, This (Simple false) -> This None
    | None, This (Simple true | Yes_with_transition _) ->
      This (Some (Module_name.of_local_lib_name (snd t.name)))
end

module Install_conf = struct
  type 'file t =
    { section : Install.Section.t
    ; files : 'file list
    ; package : Package.t
    }

  let decode =
    fields
      (let+ section = field "section" Install.Section.decode
       and+ files = field "files" File_binding.Unexpanded.L.decode
       and+ package = Pkg.field "install" in
       { section; files; package })
end

module Promote = struct
  module Lifetime = struct
    type t =
      | Unlimited
      | Until_clean
  end

  module Into = struct
    type t =
      { loc : Loc.t
      ; dir : string
      }

    let decode =
      let+ loc, dir = located relative_file in
      { loc; dir }
  end

  type t =
    { lifetime : Lifetime.t
    ; into : Into.t option
    ; only : Predicate_lang.t option
    }

  let decode =
    fields
      (let+ until_clean =
         field_b "until-clean"
           ~check:(Dune_lang.Syntax.since Stanza.syntax (1, 10))
       and+ into =
         field_o "into"
           ( Dune_lang.Syntax.since Stanza.syntax (1, 10)
           >>= fun () -> Into.decode )
       and+ only =
         field_o "only"
           ( Dune_lang.Syntax.since Stanza.syntax (1, 10)
           >>= fun () -> Predicate_lang.decode )
       in
       { lifetime =
           ( if until_clean then
             Until_clean
           else
             Unlimited )
       ; into
       ; only
       })
end

module Executables = struct
  module Names : sig
    type t

    val names : t -> (Loc.t * string) list

    val package : t -> Package.t option

    val has_public_name : t -> bool

    val make :
         multi:bool
      -> stanza:string
      -> allow_omit_names_version:Dune_lang.Syntax.Version.t
      -> (t, fields) Dune_lang.Decoder.parser

    val install_conf :
      t -> ext:string -> File_binding.Unexpanded.t Install_conf.t option
  end = struct
    type public =
      { public_names : (Loc.t * string option) list
      ; package : Package.t
      }

    type t =
      { names : (Loc.t * string) list
      ; public : public option
      ; stanza : string
      ; project : Dune_project.t
      ; loc : Loc.t
      ; multi : bool
      }

    let names t = t.names

    let package t = Option.map t.public ~f:(fun p -> p.package)

    let has_public_name t = Option.is_some t.public

    let public_name =
      located string
      >>| fun (loc, s) ->
      ( loc
      , match s with
        | "-" -> None
        | s -> Some s )

    let multi_fields =
      map_validate
        (let+ names = field_o "names" (repeat (located string))
         and+ pub_names = field_o "public_names" (repeat public_name) in
         (names, pub_names))
        ~f:(fun (names, public_names) ->
          match (names, public_names) with
          | Some names, Some public_names ->
            if List.length public_names = List.length names then
              Ok (Some names, Some public_names)
            else
              Error
                (User_error.make
                   [ Pp.text
                       "The list of public names must be of the same length \
                        as the list of names"
                   ])
          | names, public_names -> Ok (names, public_names))

    let single_fields =
      let+ name = field_o "name" (located string)
      and+ public_name = field_o "public_name" (located string) in
      ( Option.map name ~f:List.singleton
      , Option.map public_name ~f:(fun (loc, s) -> [ (loc, Some s) ]) )

    let pluralize s ~multi =
      if multi then
        s ^ "s"
      else
        s

    let make ~multi ~stanza ~allow_omit_names_version =
      let+ names =
        if multi then
          multi_fields
        else
          single_fields
      and+ loc = loc
      and+ dune_syntax = Dune_lang.Syntax.get_exn Stanza.syntax
      and+ package =
        field_o "package"
          (let+ loc = loc
           and+ pkg = Pkg.decode in
           (loc, pkg))
      and+ project = Dune_project.get_exn () in
      let names, public_names = names in
      let stanza = pluralize stanza ~multi in
      let names =
        let open Dune_lang.Syntax.Version.Infix in
        match (names, public_names) with
        | Some names, _ -> names
        | None, Some public_names ->
          if dune_syntax >= allow_omit_names_version then
            List.map public_names ~f:(fun (loc, p) ->
                match p with
                | None ->
                  User_error.raise ~loc
                    [ Pp.text "This executable must have a name field" ]
                | Some s -> (loc, s))
          else
            User_error.raise ~loc
              [ Pp.textf "%s field may not be omitted before dune version %s"
                  (pluralize ~multi "name")
                  (Dune_lang.Syntax.Version.to_string allow_omit_names_version)
              ]
        | None, None ->
          if dune_syntax >= allow_omit_names_version then
            User_error.raise ~loc
              [ Pp.textf "either the %s or the %s field must be present"
                  (pluralize ~multi "name")
                  (pluralize ~multi "public_name")
              ]
          else
            User_error.raise ~loc
              [ Pp.textf "field %s is missing" (pluralize ~multi "name") ]
      in
      let public =
        match (package, public_names) with
        | None, None -> None
        | Some (_loc, package), Some public_names ->
          Some { package; public_names }
        | None, Some public_names ->
          if List.for_all public_names ~f:(fun (_, x) -> Option.is_none x) then
            None
          else
            Some
              { public_names
              ; package =
                  Pkg.default_exn ~loc project (pluralize "executable" ~multi)
              }
        | Some (loc, _), None ->
          User_error.raise ~loc
            [ Pp.textf "This field is useless without a (%s ...) field."
                (pluralize "public_name" ~multi)
            ]
      in
      { names; public; project; stanza; loc; multi }

    let install_conf t ~ext =
      Option.map t.public ~f:(fun { package; public_names } ->
          let files =
            List.map2 t.names public_names ~f:(fun (locn, name) (locp, pub) ->
                Option.map pub ~f:(fun pub ->
                    File_binding.Unexpanded.make
                      ~src:(locn, name ^ ext)
                      ~dst:(locp, pub)))
            |> List.filter_opt
          in
          { Install_conf.section = Bin; files; package })
  end

  module Link_mode = struct
    module T = struct
      type t =
        { mode : Mode_conf.t
        ; kind : Binary_kind.t
        ; loc : Loc.t
        }

      let compare a b =
        match compare a.mode b.mode with
        | Eq -> compare a.kind b.kind
        | ne -> ne

      let to_dyn _ = Dyn.opaque
    end

    include T

    let make mode kind = { mode; kind; loc = Loc.none }

    let exe = make Best Exe

    let object_ = make Best Object

    let shared_object = make Best Shared_object

    let byte_exe = make Byte Exe

    let native_exe = make Native Exe

    let native_object = make Native Object

    let native_shared_object = make Native Shared_object

    let byte = byte_exe

    let native = native_exe

    let js = make Byte Js

    let installable_modes = [ exe; native; byte ]

    let simple_representations =
      [ ("exe", exe)
      ; ("object", object_)
      ; ("shared_object", shared_object)
      ; ("byte", byte)
      ; ("native", native)
      ; ("js", js)
      ]

    let simple = Dune_lang.Decoder.enum simple_representations

    let decode =
      if_list
        ~then_:
          (enter
             (let+ mode = Mode_conf.decode
              and+ kind = Binary_kind.decode
              and+ loc = loc in
              { mode; kind; loc }))
        ~else_:simple

    let simple_encode link_mode =
      let is_ok (_, candidate) = compare candidate link_mode = Eq in
      List.find ~f:is_ok simple_representations
      |> Option.map ~f:(fun (s, _) -> Dune_lang.unsafe_atom_of_string s)

    let encode link_mode =
      match simple_encode link_mode with
      | Some s -> s
      | None ->
        let { mode; kind; loc = _ } = link_mode in
        Dune_lang.Encoder.pair Mode_conf.encode Binary_kind.encode (mode, kind)

    let to_dyn { mode; kind; loc = _ } =
      let open Dyn.Encoder in
      record
        [ ("mode", Mode_conf.to_dyn mode); ("kind", Binary_kind.to_dyn kind) ]

    module O = Comparable.Make (T)

    module Set = struct
      include O.Set

      let decode =
        located (repeat decode)
        >>| fun (loc, l) ->
        match l with
        | [] -> User_error.raise ~loc [ Pp.textf "No linking mode defined" ]
        | l ->
          let t = of_list l in
          if
            (mem t native_exe && mem t exe)
            || (mem t native_object && mem t object_)
            || (mem t native_shared_object && mem t shared_object)
          then
            User_error.raise ~loc
              [ Pp.textf
                  "It is not allowed use both native and best for the same \
                   binary kind."
              ]
          else
            t

      let default = of_list [ byte; exe ]

      let best_install_mode t = List.find ~f:(mem t) installable_modes
    end
  end

  type t =
    { names : (Loc.t * string) list
    ; link_flags : Ordered_set_lang.Unexpanded.t
    ; link_deps : Dep_conf.t list
    ; modes : Link_mode.Set.t
    ; optional : bool
    ; buildable : Buildable.t
    ; variants : (Loc.t * Variant.Set.t) option
    ; package : Package.t option
    ; promote : Promote.t option
    ; install_conf : File_binding.Unexpanded.t Install_conf.t option
    ; forbidden_libraries : (Loc.t * Lib_name.t) list
    }

  let common =
    let+ buildable =
      Buildable.decode ~since_c:(Some (2, 0)) ~allow_re_export:false
    and+ (_ : bool) =
      field "link_executables" ~default:true
        (Dune_lang.Syntax.deleted_in Stanza.syntax (1, 0) >>> bool)
    and+ link_deps = field "link_deps" (repeat Dep_conf.decode) ~default:[]
    and+ link_flags = field_oslu "link_flags"
    and+ modes =
      field "modes" Link_mode.Set.decode ~default:Link_mode.Set.default
    and+ optional =
      field_b "optional" ~check:(Dune_lang.Syntax.since Stanza.syntax (2, 0))
    and+ variants = variants_field
    and+ promote =
      field_o "promote"
        (Dune_lang.Syntax.since Stanza.syntax (1, 11) >>> Promote.decode)
    and+ () =
      map_validate
        (field "inline_tests" (repeat junk >>| fun _ -> true) ~default:false)
        ~f:(function
          | false -> Ok ()
          | true ->
            Error
              (User_error.make
                 [ Pp.text "Inline tests are only allowed in libraries."
                 ; Pp.text
                     "See https://github.com/ocaml/dune/issues/745 for more \
                      details."
                 ]))
    and+ forbidden_libraries =
      field "forbidden_libraries"
        ( Dune_lang.Syntax.since Stanza.syntax (2, 0)
        >>> repeat (located Lib_name.decode) )
        ~default:[]
    in
    fun names ~multi ->
      let has_public_name = Names.has_public_name names in
      let private_names = Names.names names in
      let install_conf =
        match Link_mode.Set.best_install_mode modes with
        | None when has_public_name ->
          User_error.raise ~loc:buildable.loc
            [ Pp.textf "No installable mode found for %s."
                ( if multi then
                  "these executables"
                else
                  "this executable" )
            ; Pp.text "One of the following modes is required:"
            ; Pp.enumerate Link_mode.installable_modes ~f:(fun mode ->
                  Pp.verbatim (Dune_lang.to_string (Link_mode.encode mode)))
            ]
        | None -> None
        | Some mode ->
          let ext =
            match mode.mode with
            | Native
             |Best ->
              ".exe"
            | Byte -> ".bc"
          in
          Names.install_conf names ~ext
      in
      { names = private_names
      ; link_flags
      ; link_deps
      ; modes
      ; optional
      ; buildable
      ; variants
      ; package = Names.package names
      ; promote
      ; install_conf
      ; forbidden_libraries
      }

  let single, multi =
    let stanza = "executable" in
    let make multi =
      fields
        (let+ names = Names.make ~multi ~stanza ~allow_omit_names_version:(1, 1)
         and+ f = common in
         f names ~multi)
    in
    (make false, make true)

  let has_stubs t =
    match (t.buildable.c_names, t.buildable.cxx_names) with
    | None, None -> false
    | _ -> true

  let obj_dir t ~dir = Obj_dir.make_exe ~dir ~name:(snd (List.hd t.names))
end

module Rule = struct
  module Targets = struct
    module Multiplicity = struct
      type t =
        | One
        | Multiple
    end

    type static =
      { targets : String_with_vars.t list
      ; multiplicity : Multiplicity.t
      }

    type t =
      (* List of files in the current directory *)
      | Static of static
      | Infer

    let decode_static =
      let+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax
      and+ targets = repeat String_with_vars.decode in
      if syntax_version < (1, 3) then
        List.iter targets ~f:(fun target ->
            if String_with_vars.has_vars target then
              Dune_lang.Syntax.Error.since
                (String_with_vars.loc target)
                Stanza.syntax (1, 3)
                ~what:"Using variables in the targets field");
      Static { targets; multiplicity = Multiple }

    let decode_one_static =
      let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 11)
      and+ target = String_with_vars.decode in
      Static { targets = [ target ]; multiplicity = One }

    let fields_parser =
      fields_mutually_exclusive ~default:Infer
        [ ("targets", decode_static); ("target", decode_one_static) ]
  end

  module Mode = struct
    type t =
      | Standard
      | Fallback
      | Promote of Promote.t
      | Ignore_source_files

    let decode =
      let promote_into lifetime =
        let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 8)
        and+ into = Promote.Into.decode in
        Promote { lifetime; into = Some into; only = None }
      in
      sum
        [ ("standard", return Standard)
        ; ("fallback", return Fallback)
        ; ( "promote"
          , let+ p = Promote.decode in
            Promote p )
        ; ( "promote-until-clean"
          , return
              (Promote { lifetime = Until_clean; into = None; only = None }) )
        ; ("promote-into", promote_into Unlimited)
        ; ("promote-until-clean-into", promote_into Until_clean)
        ]

    let field = field "mode" decode ~default:Standard
  end

  type t =
    { targets : Targets.t
    ; deps : Dep_conf.t Bindings.t
    ; action : Loc.t * Action_dune_lang.t
    ; mode : Mode.t
    ; locks : String_with_vars.t list
    ; loc : Loc.t
    ; enabled_if : Blang.t
    }

  type action_or_field =
    | Action
    | Field

  let atom_table =
    String.Map.of_list_exn
      [ ("run", Action)
      ; ("chdir", Action)
      ; ("setenv", Action)
      ; ("with-stdout-to", Action)
      ; ("with-stderr-to", Action)
      ; ("with-outputs-to", Action)
      ; ("ignore-stdout", Action)
      ; ("ignore-stderr", Action)
      ; ("ignore-outputs", Action)
      ; ("progn", Action)
      ; ("echo", Action)
      ; ("cat", Action)
      ; ("copy", Action)
      ; ("copy#", Action)
      ; ("copy-and-add-line-directive", Action)
      ; ("system", Action)
      ; ("bash", Action)
      ; ("write-file", Action)
      ; ("diff", Action)
      ; ("diff?", Action)
      ; ("targets", Field)
      ; ("target", Field)
      ; ("deps", Field)
      ; ("action", Field)
      ; ("locks", Field)
      ; ("fallback", Field)
      ; ("mode", Field)
      ]

  let short_form =
    let+ loc, action = located Action_dune_lang.decode in
    { targets = Infer
    ; deps = Bindings.empty
    ; action = (loc, action)
    ; mode = Standard
    ; locks = []
    ; loc
    ; enabled_if = Blang.true_
    }

  let long_form =
    let+ loc = loc
    and+ action = field "action" (located Action_dune_lang.decode)
    and+ targets = Targets.fields_parser
    and+ deps =
      field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty
    and+ locks = field "locks" (repeat String_with_vars.decode) ~default:[]
    and+ mode =
      (* DUNE2: forbid (fallback) *)
      map_validate
        (let+ fallback =
           field_b
             ~check:
               (Dune_lang.Syntax.renamed_in Stanza.syntax (1, 0)
                  ~to_:"(mode fallback)")
             "fallback"
         and+ mode = field_o "mode" Mode.decode in
         (fallback, mode))
        ~f:(function
          | true, Some _ ->
            Error
              (User_error.make
                 [ Pp.text
                     "Cannot use both (fallback) and (mode ...) at the same \
                      time."
                 ; Pp.text
                     "(fallback) is the same as (mode fallback), please use \
                      the latter in new code."
                 ])
          | false, Some mode -> Ok mode
          | true, None -> Ok Fallback
          | false, None -> Ok Standard)
    and+ enabled_if = enabled_if ~since:(Some (1, 4)) in
    { targets; deps; action; mode; locks; loc; enabled_if }

  let dune_syntax =
    peek_exn
    >>= function
    | List (_, Atom (loc, A s) :: _) -> (
      match String.Map.find atom_table s with
      | None ->
        User_error.raise ~loc
          [ Pp.text "Unknown action or rule field." ]
          ~hints:
            (User_message.did_you_mean s
               ~candidates:(String.Map.keys atom_table))
      | Some Field -> fields long_form
      | Some Action -> short_form )
    | sexp ->
      User_error.raise ~loc:(Dune_lang.Ast.loc sexp)
        [ Pp.textf "S-expression of the form (<atom> ...) expected" ]

  let decode = dune_syntax

  type lex_or_yacc =
    { modules : string list
    ; mode : Mode.t
    ; enabled_if : Blang.t
    }

  let ocamllex =
    if_eos
      ~then_:
        (return { modules = []; mode = Standard; enabled_if = Blang.true_ })
      ~else_:
        (if_list
           ~then_:
             (fields
                (let+ modules = field "modules" (repeat string)
                 and+ mode = Mode.field
                 and+ enabled_if = enabled_if ~since:(Some (1, 4)) in
                 { modules; mode; enabled_if }))
           ~else_:
             ( repeat string
             >>| fun modules ->
             { modules; mode = Standard; enabled_if = Blang.true_ } ))

  let ocamlyacc = ocamllex

  let ocamllex_to_rule loc { modules; mode; enabled_if } =
    let module S = String_with_vars in
    List.map modules ~f:(fun name ->
        let src = name ^ ".mll" in
        let dst = name ^ ".ml" in
        { targets =
            (* CR-someday aalekseyev: want to use [multiplicity = One] here,
               but can't because this is might get parsed with old dune syntax
               where [multiplicity = One] is not supported. *)
            Static
              { targets = [ S.make_text loc dst ]; multiplicity = Multiple }
        ; deps = Bindings.singleton (Dep_conf.File (S.virt_text __POS__ src))
        ; action =
            ( loc
            , Chdir
                ( S.virt_var __POS__ "workspace_root"
                , Run
                    ( S.virt_text __POS__ "ocamllex"
                    , [ S.virt_text __POS__ "-q"
                      ; S.virt_text __POS__ "-o"
                      ; S.virt_var __POS__ "targets"
                      ; S.virt_var __POS__ "deps"
                      ] ) ) )
        ; mode
        ; locks = []
        ; loc
        ; enabled_if
        })

  let ocamlyacc_to_rule loc { modules; mode; enabled_if } =
    let module S = String_with_vars in
    List.map modules ~f:(fun name ->
        let src = name ^ ".mly" in
        { targets =
            Static
              { targets =
                  List.map ~f:(S.make_text loc) [ name ^ ".ml"; name ^ ".mli" ]
              ; multiplicity = Multiple
              }
        ; deps = Bindings.singleton (Dep_conf.File (S.virt_text __POS__ src))
        ; action =
            ( loc
            , Chdir
                ( S.virt_var __POS__ "workspace_root"
                , Run
                    ( S.virt_text __POS__ "ocamlyacc"
                    , [ S.virt_var __POS__ "deps" ] ) ) )
        ; mode
        ; locks = []
        ; loc
        ; enabled_if
        })
end

module Menhir = struct
  type t =
    { merge_into : string option
    ; flags : Ordered_set_lang.Unexpanded.t
    ; modules : string list
    ; mode : Rule.Mode.t
    ; loc : Loc.t
    ; infer : bool
    ; enabled_if : Blang.t
    }

  let syntax =
    Dune_lang.Syntax.create ~name:"menhir" ~desc:"the menhir extension"
      [ (1, 1); (2, 0) ]

  let decode =
    fields
      (let+ merge_into = field_o "merge_into" string
       and+ flags = field_oslu "flags"
       and+ modules = field "modules" (repeat string)
       and+ mode = Rule.Mode.field
       and+ infer =
         field_o_b "infer" ~check:(Dune_lang.Syntax.since syntax (2, 0))
       and+ menhir_syntax = Dune_lang.Syntax.get_exn syntax
       and+ enabled_if = enabled_if ~since:(Some (1, 4))
       and+ loc = loc in
       let infer =
         match infer with
         | Some infer -> infer
         | None -> menhir_syntax >= (2, 0)
       in
       { merge_into; flags; modules; mode; loc; infer; enabled_if })

  type Stanza.t += T of t

  let () =
    Dune_project.Extension.register_simple syntax
      (return [ ("menhir", decode >>| fun x -> [ T x ]) ])
end

module Coqpp = struct
  type t =
    { modules : string list
    ; loc : Loc.t
    }

  let decode =
    fields
      (let+ modules = field "modules" (repeat string)
       and+ loc = loc in
       { modules; loc })

  type Stanza.t += T of t
end

module Coq = struct
  type t =
    { name : Loc.t * Lib_name.Local.t (* TODO: validate name *)
    ; public : Public_lib.t option
    ; synopsis : string option
    ; modules : Ordered_set_lang.t
    ; flags : Ordered_set_lang.Unexpanded.t
    ; libraries : (Loc.t * Lib_name.t) list  (** ocaml libraries *)
    ; loc : Loc.t
    ; enabled_if : Blang.t
    }

  let syntax =
    Dune_lang.Syntax.create ~name:"coq"
      ~desc:"the coq extension (experimental)" [ (0, 1) ]

  let decode =
    fields
      (let+ name = field "name" Lib_name.Local.decode_loc
       and+ loc = loc
       and+ public = Public_lib.public_name_field
       and+ synopsis = field_o "synopsis" string
       and+ flags = field_oslu "flags"
       and+ modules = modules_field "modules"
       and+ libraries =
         field "libraries" (repeat (located Lib_name.decode)) ~default:[]
       and+ enabled_if = enabled_if ~since:None in
       let name =
         let loc, res = name in
         (loc, Lib_name.Local.validate (loc, res))
       in
       { name; public; synopsis; modules; flags; libraries; loc; enabled_if })

  let best_name t =
    match t.public with
    | None -> Lib_name.of_local t.name
    | Some p -> snd p.name

  type Stanza.t += T of t

  let coqlib_warn x =
    User_warning.emit ~loc:x.loc
      [ Pp.text
          "(coqlib ...) is deprecated and will be removed in the Coq language \
           version 1.0, please use (coq.theory ...) instead"
      ];
    x

  let coqlib_p = ("coqlib", decode >>| fun x -> [ T (coqlib_warn x) ])

  let coqtheory_p = ("coq.theory", decode >>| fun x -> [ T x ])

  let coqpp_p = ("coq.pp", Coqpp.(decode >>| fun x -> [ T x ]))

  let unit_stanzas =
    let+ r = return [ coqlib_p; coqtheory_p; coqpp_p ] in
    ((), r)

  let key = Dune_project.Extension.register syntax unit_stanzas Unit.to_dyn
end

module Alias_conf = struct
  type t =
    { name : string
    ; deps : Dep_conf.t Bindings.t
    ; action : (Loc.t * Action_dune_lang.t) option
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; enabled_if : Blang.t
    ; loc : Loc.t
    }

  let alias_name =
    plain_string (fun ~loc s ->
        if Filename.basename s <> s then
          User_error.raise ~loc [ Pp.textf "%S is not a valid alias name" s ]
        else
          s)

  let decode =
    fields
      (let+ name = field "name" alias_name
       and+ loc = loc
       and+ package = field_o "package" Pkg.decode
       and+ action = field_o "action" (located Action_dune_lang.decode)
       and+ locks = field "locks" (repeat String_with_vars.decode) ~default:[]
       and+ deps =
         field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty
       and+ enabled_if =
         field "enabled_if" Blang.decode ~default:Blang.true_
       in
       { name; deps; action; package; locks; enabled_if; loc })
end

module Tests = struct
  type t =
    { exes : Executables.t
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; deps : Dep_conf.t Bindings.t
    ; enabled_if : Blang.t
    ; action : Action_dune_lang.t option
    }

  let gen_parse names =
    fields
      (let+ buildable =
         Buildable.decode ~since_c:(Some (2, 0)) ~allow_re_export:false
       and+ link_flags = field_oslu "link_flags"
       and+ variants = variants_field
       and+ names = names
       and+ package = field_o "package" Pkg.decode
       and+ locks = field "locks" (repeat String_with_vars.decode) ~default:[]
       and+ modes =
         field "modes" Executables.Link_mode.Set.decode
           ~default:Executables.Link_mode.Set.default
       and+ deps =
         field "deps" (Bindings.decode Dep_conf.decode) ~default:Bindings.empty
       and+ enabled_if = enabled_if ~since:(Some (1, 4))
       and+ action =
         field_o "action"
           ( Dune_lang.Syntax.since ~fatal:false Stanza.syntax (1, 2)
           >>> Action_dune_lang.decode )
       and+ forbidden_libraries =
         field "forbidden_libraries"
           ( Dune_lang.Syntax.since Stanza.syntax (2, 0)
           >>> repeat (located Lib_name.decode) )
           ~default:[]
       in
       { exes =
           { Executables.link_flags
           ; link_deps = []
           ; modes
           ; optional = false
           ; buildable
           ; names
           ; variants
           ; package = None
           ; promote = None
           ; install_conf = None
           ; forbidden_libraries
           }
       ; locks
       ; package
       ; deps
       ; enabled_if
       ; action
       })

  let multi = gen_parse (field "names" (repeat (located string)))

  let single = gen_parse (field "name" (located string) >>| List.singleton)
end

module Toplevel = struct
  type t =
    { name : string
    ; libraries : (Loc.t * Lib_name.t) list
    ; variants : (Loc.t * Variant.Set.t) option
    ; loc : Loc.t
    }

  let decode =
    let open Dune_lang.Decoder in
    fields
      (let+ loc = loc
       and+ name = field "name" string
       and+ variants = variants_field
       and+ libraries =
         field "libraries" (repeat (located Lib_name.decode)) ~default:[]
       in
       { name; libraries; loc; variants })
end

module Copy_files = struct
  type t =
    { add_line_directive : bool
    ; glob : String_with_vars.t
    ; syntax_version : Dune_lang.Syntax.Version.t
    }

  let decode = String_with_vars.decode
end

module Documentation = struct
  type t =
    { loc : Loc.t
    ; package : Package.t
    ; mld_files : Ordered_set_lang.t
    }

  let decode =
    fields
      (let+ package = Pkg.field "documentation"
       and+ mld_files = Ordered_set_lang.field "mld_files"
       and+ loc = loc in
       { loc; package; mld_files })
end

module Include_subdirs = struct
  type qualification =
    | Unqualified
    | Qualified

  type t =
    | No
    | Include of qualification

  let decode ~enable_qualified =
    let opts_list =
      [ ("no", No); ("unqualified", Include Unqualified) ]
      @
      if enable_qualified then
        [ ("qualified", Include Qualified) ]
      else
        []
    in
    enum opts_list
end

type Stanza.t +=
  | Library of Library.t
  | Executables of Executables.t
  | Rule of Rule.t
  | Install of File_binding.Unexpanded.t Install_conf.t
  | Alias of Alias_conf.t
  | Copy_files of Copy_files.t
  | Documentation of Documentation.t
  | Tests of Tests.t
  | Include_subdirs of Loc.t * Include_subdirs.t
  | Toplevel of Toplevel.t
  | External_variant of External_variant.t

module Stanzas = struct
  type t = Stanza.t list

  type syntax =
    | OCaml
    | Plain

  let rules l = List.map l ~f:(fun x -> Rule x)

  let execs exe = [ Executables exe ]

  type Stanza.t += Include of Loc.t * string

  type constructors = (string * Stanza.t list Dune_lang.Decoder.t) list

  let stanzas : constructors =
    [ ( "library"
      , let+ x = Library.decode in
        [ Library x ] )
    ; ("executable", Executables.single >>| execs)
    ; ("executables", Executables.multi >>| execs)
    ; ( "rule"
      , let+ loc = loc
        and+ x = Rule.decode in
        [ Rule { x with loc } ] )
    ; ( "ocamllex"
      , let+ loc = loc
        and+ x = Rule.ocamllex in
        rules (Rule.ocamllex_to_rule loc x) )
    ; ( "ocamlyacc"
      , let+ loc = loc
        and+ x = Rule.ocamlyacc in
        rules (Rule.ocamlyacc_to_rule loc x) )
    ; ( "install"
      , let+ x = Install_conf.decode in
        [ Install x ] )
    ; ( "alias"
      , let+ x = Alias_conf.decode in
        [ Alias x ] )
    ; ( "copy_files"
      , let+ glob = Copy_files.decode
        and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
        [ Copy_files { add_line_directive = false; glob; syntax_version } ] )
    ; ( "copy_files#"
      , let+ glob = Copy_files.decode
        and+ syntax_version = Dune_lang.Syntax.get_exn Stanza.syntax in
        [ Copy_files { add_line_directive = true; glob; syntax_version } ] )
    ; ( "include"
      , let+ loc = loc
        and+ fn = relative_file in
        [ Include (loc, fn) ] )
    ; ( "documentation"
      , let+ d = Documentation.decode in
        [ Documentation d ] )
    ; ( "jbuild_version"
      , let+ () = Dune_lang.Syntax.deleted_in Stanza.syntax (1, 0)
        and+ _ = Jbuild_version.decode in
        [] )
    ; ( "tests"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
        and+ t = Tests.multi in
        [ Tests t ] )
    ; ( "test"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 0)
        and+ t = Tests.single in
        [ Tests t ] )
    ; ( "external_variant"
      , let+ () = Dune_lang.Syntax.since library_variants (0, 2)
        and+ t = External_variant.decode in
        [ External_variant t ] )
    ; ( "env"
      , let+ x = Dune_env.Stanza.decode in
        [ Dune_env.T x ] )
    ; ( "include_subdirs"
      , let* project = Dune_project.get_exn () in
        let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 1)
        and+ t =
          let enable_qualified =
            Option.is_some (Dune_project.find_extension_args project Coq.key)
          in
          Include_subdirs.decode ~enable_qualified
        and+ loc = loc in
        [ Include_subdirs (loc, t) ] )
    ; ( "toplevel"
      , let+ () = Dune_lang.Syntax.since Stanza.syntax (1, 7)
        and+ t = Toplevel.decode in
        [ Toplevel t ] )
    ]

  let () = Dune_project.Lang.register Stanza.syntax stanzas

  let parser project =
    let syntax_parser = Dune_project.stanza_parser project in
    Dune_project.set project syntax_parser

  let parse parser = Dune_lang.Decoder.parse parser Univ_map.empty

  let of_ast (project : Dune_project.t) sexp =
    let parser = parser project in
    parse parser sexp

  exception Include_loop of Path.Source.t * (Loc.t * Path.Source.t) list

  let rec parse_file_includes ~stanza_parser ~lexer ~current_file
      ~include_stack sexps =
    List.concat_map sexps ~f:(parse stanza_parser)
    |> List.concat_map ~f:(function
         | Include (loc, fn) ->
           let include_stack = (loc, current_file) :: include_stack in
           let dir = Path.Source.parent_exn current_file in
           let current_file = Path.Source.relative dir fn in
           if not (Path.exists (Path.source current_file)) then
             User_error.raise ~loc
               [ Pp.textf "File %s doesn't exist."
                   (Path.Source.to_string_maybe_quoted current_file)
               ];
           if
             List.exists include_stack ~f:(fun (_, f) ->
                 Path.Source.equal f current_file)
           then
             raise (Include_loop (current_file, include_stack));
           let sexps =
             Dune_lang.Parser.load ~lexer (Path.source current_file) ~mode:Many
           in
           parse_file_includes ~stanza_parser ~lexer ~current_file
             ~include_stack sexps
         | stanza -> [ stanza ])

  let parse ~file (project : Dune_project.t) sexps =
    let stanza_parser = parser project in
    let lexer = Dune_lang.Lexer.token in
    let stanzas =
      try
        parse_file_includes ~stanza_parser ~lexer ~include_stack:[]
          ~current_file:file sexps
      with
      | Include_loop (_, []) -> assert false
      | Include_loop (file, last :: rest) ->
        let loc = fst (Option.value (List.last rest) ~default:last) in
        let line_loc (loc, file) =
          sprintf "%s:%d"
            (Path.Source.to_string_maybe_quoted file)
            loc.Loc.start.pos_lnum
        in
        User_error.raise ~loc
          [ Pp.text "Recursive inclusion of dune files detected:"
          ; Pp.textf "File %s is included from %s"
              (Path.Source.to_string_maybe_quoted file)
              (line_loc last)
          ; Pp.vbox
              (Pp.concat_map rest ~sep:Pp.cut ~f:(fun x ->
                   Pp.box ~indent:3
                     (Pp.seq (Pp.verbatim "-> ")
                        (Pp.textf "included from %s" (line_loc x)))))
          ]
    in
    match
      List.filter_map stanzas ~f:(function
        | Dune_env.T e -> Some e
        | _ -> None)
    with
    | _ :: e :: _ ->
      User_error.raise ~loc:e.loc
        [ Pp.text "The 'env' stanza cannot appear more than once" ]
    | _ -> stanzas
end

let stanza_package = function
  | Library { public = Some { package; _ }; _ }
   |Alias { package = Some package; _ }
   |Install { package; _ }
   |Executables { install_conf = Some { package; _ }; _ }
   |Documentation { package; _ }
   |Tests { package = Some package; _ } ->
    Some package
  | Coq.T { public = Some { package; _ }; _ } -> Some package
  | _ -> None

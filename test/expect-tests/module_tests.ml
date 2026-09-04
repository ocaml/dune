open Stdune
module Module = Dune_rules.Module
module Kind = Module.Kind
module Modules = Dune_rules.Modules
module Module_name = Dune_lang.Module_name
module Module_trie = Dune_rules.For_tests.Module_trie

(* See #10264 *)
let%expect_test "Module.Kind encoding round trip" =
  let module_name s = Dune_lang.Module_name.of_checked_string s in
  let test k =
    let ast = Kind.encode k in
    let sexp = Dune_sexp.Ast.add_loc ~loc:Loc.none ast in
    let decoded =
      match Dune_lang.Decoder.parse Kind.decode Univ_map.empty sexp with
      | r -> Ok r
      | exception e -> Error e
    in
    let dyn =
      Dyn.record
        [ "ast", Dyn.string (Dune_sexp.to_string ast)
        ; "decoded", Or_exn.to_dyn Kind.to_dyn decoded
        ]
    in
    Dune_tests_common.print_dyn dyn
  in
  test Impl;
  [%expect {| { ast = "impl"; decoded = Ok "impl" } |}];
  test (Alias []);
  [%expect {| { ast = "alias"; decoded = Ok "alias" } |}];
  test (Alias [ module_name "A" ]);
  [%expect {| { ast = "(alias (A))"; decoded = Ok [ "alias"; [ "A" ] ] } |}];
  test (Alias [ module_name "A"; module_name "B" ]);
  [%expect {| { ast = "(alias (A B))"; decoded = Ok [ "alias"; [ "A"; "B" ] ] } |}]
;;

let module_name = Module_name.of_checked_string
let obj_dir = Path.Build.relative Path.Build.root "module-tests"
let module_path names = List.map names ~f:module_name |> Nonempty_list.of_list_exn

let generated ?(kind = Kind.Impl) ~obj_name path =
  Module.generated
    ~kind
    ~obj_name:(Module_name.Unique.of_string obj_name)
    ~for_:Ocaml
    ~src_dir:obj_dir
    (module_path path)
;;

let private_module ~obj_name path =
  let file =
    Path.Build.relative obj_dir (obj_name ^ ".ml")
    |> Path.build
    |> Module.File.make Dune_lang.Dialect.ocaml
  in
  let source = Module.Source.make ~impl:(Some file) ~intf:None (module_path path) in
  Module.of_source ~visibility:Private ~kind:Impl source
  |> fun module_ -> Module.set_obj_name module_ (Module_name.Unique.of_string obj_name)
;;

let make_lib
      ?(wrapped = Dune_lang.Wrapped.Simple false)
      ?main_module_name
      ?(implements = false)
      ~lib_name
      modules
  =
  let modules =
    List.fold_left modules ~init:Module_trie.empty ~f:(fun trie module_ ->
      Module_trie.set trie (Module.path module_) module_)
  in
  Modules.lib
    ~obj_dir
    ~main_module_name
    ~wrapped
    ~stdlib:None
    ~lib_name:(Dune_lang.Lib_name.Local.of_string lib_name)
    ~implements
    ~has_instances:false
    ~modules
    ~for_:Ocaml
;;

let%expect_test "qualified group alias source path" =
  let modules =
    make_lib
      ~lib_name:"group"
      [ generated ~obj_name:"Group__A" [ "Group"; "A" ]
      ; generated ~obj_name:"Group__B" [ "Group"; "B" ]
      ]
  in
  Modules.fold modules ~init:[] ~f:(fun module_ paths ->
    match Module.kind module_ with
    | Alias _ -> Module.path module_ :: paths
    | Intf_only | Virtual | Impl | Impl_vmodule | Wrapped_compat | Root | Parameter ->
      paths)
  |> List.rev
  |> Dyn.list Module_name.Path.to_dyn
  |> Dune_tests_common.print_dyn;
  [%expect {| [ [ "Group"; "Group" ] ] |}]
;;

let%expect_test "repeated qualified group paths" =
  let modules =
    make_lib
      ~lib_name:"foo"
      [ generated ~obj_name:"Foo__Foo__Foo__Foo__A" [ "Foo"; "Foo"; "Foo"; "Foo"; "A" ]
      ; generated ~obj_name:"Foo__Foo__Foo__Foo__B" [ "Foo"; "Foo"; "Foo"; "Foo"; "B" ]
      ]
  in
  Modules.fold modules ~init:[] ~f:(fun module_ paths ->
    match Module.kind module_ with
    | Alias _ -> Module.path module_ :: paths
    | Intf_only | Virtual | Impl | Impl_vmodule | Wrapped_compat | Root | Parameter ->
      paths)
  |> List.rev
  |> Dyn.list Module_name.Path.to_dyn
  |> Dune_tests_common.print_dyn;
  [%expect
    {|
    [ [ "Foo"; "Foo" ]
    ; [ "Foo"; "Foo"; "Foo" ]
    ; [ "Foo"; "Foo"; "Foo"; "Foo" ]
    ; [ "Foo"; "Foo"; "Foo"; "Foo"; "Foo" ]
    ]
    |}]
;;

let kind_name = function
  | Kind.Intf_only -> "intf-only"
  | Virtual -> "virtual"
  | Impl -> "impl"
  | Alias _ -> "alias"
  | Impl_vmodule -> "impl-vmodule"
  | Wrapped_compat -> "wrapped-compat"
  | Root -> "root"
  | Parameter -> "parameter"
;;

let module_summary module_ =
  Printf.sprintf
    "%s:%s:%s"
    (Module_name.Unique.to_string (Module.obj_name module_))
    (Module_name.to_string (Module.name module_))
    (kind_name (Module.kind module_))
;;

let find_deps_exn modules ~of_ names =
  match Modules.With_vlib.find_deps modules ~of_ names with
  | Ok modules -> modules
  | Error (`Parent_cycle name) ->
    Code_error.raise "unexpected parent cycle" [ "dependency", Module_name.to_dyn name ]
;;

let check_deps label modules ~of_ names ~expected =
  let summarize modules = List.map modules ~f:module_summary in
  let batch = find_deps_exn modules ~of_ names |> summarize in
  let singleton =
    List.concat_map names ~f:(fun name -> find_deps_exn modules ~of_ [ name ])
    |> summarize
  in
  if not (List.equal String.equal batch singleton)
  then
    Code_error.raise
      "dependency lookup differs from singleton lookups"
      [ "batch", Dyn.list Dyn.string batch; "singleton", Dyn.list Dyn.string singleton ];
  if not (List.equal String.equal batch expected)
  then
    Code_error.raise
      "unexpected dependency result"
      [ "actual", Dyn.list Dyn.string batch; "expected", Dyn.list Dyn.string expected ];
  Format.printf "%s: %s@." label (Dyn.to_string (Dyn.list Dyn.string batch))
;;

let%expect_test "virtual-library dependency lookup" =
  let impl_shared = generated ~obj_name:"Impl__Shared" [ "Shared" ] in
  let impl_only = generated ~obj_name:"Impl__Only" [ "Only_impl" ] in
  let current = generated ~obj_name:"Impl__Current" [ "Current" ] in
  let impl =
    make_lib ~lib_name:"impl" ~implements:true [ impl_shared; impl_only; current ]
  in
  let vlib_shared = generated ~obj_name:"Vlib__Shared" [ "Shared" ] in
  let vlib_only = generated ~obj_name:"Vlib__Only" [ "Only_vlib" ] in
  let vlib_private = private_module ~obj_name:"Vlib__Private" [ "Private_vlib" ] in
  let vlib = make_lib ~lib_name:"vlib" [ vlib_shared; vlib_only; vlib_private ] in
  let modules = Modules.With_vlib.impl impl ~vlib in
  check_deps
    "virtual library"
    modules
    ~of_:current
    [ module_name "Shared"
    ; module_name "Missing"
    ; module_name "Only_vlib"
    ; module_name "Private_vlib"
    ; module_name "Only_impl"
    ; module_name "Current"
    ; module_name "Only_vlib"
    ]
    ~expected:
      [ "impl__Shared:Shared:impl"
      ; "vlib__Only:Only_vlib:impl"
      ; "impl__Only:Only_impl:impl"
      ; "vlib__Only:Only_vlib:impl"
      ];
  [%expect
    {|
    virtual library: [ "impl__Shared:Shared:impl"
    ; "vlib__Only:Only_vlib:impl"
    ; "impl__Only:Only_impl:impl"
    ; "vlib__Only:Only_vlib:impl"
    ]
    |}]
;;

let%expect_test "virtual-library object map after mapping" =
  let impl =
    make_lib
      ~lib_name:"impl"
      ~implements:true
      [ generated ~obj_name:"Shared" [ "Shared" ] ]
  in
  let vlib = make_lib ~lib_name:"vlib" [ generated ~obj_name:"Shared" [ "Shared" ] ] in
  let modules = Modules.With_vlib.impl impl ~vlib in
  ignore (Modules.With_vlib.obj_map modules : _);
  let modules =
    Modules.With_vlib.map modules ~f:(fun module_ ->
      let obj_name =
        Module.obj_name module_
        |> Module_name.Unique.to_string
        |> Printf.sprintf "mapped__%s"
        |> Module_name.Unique.of_string
      in
      Module.set_obj_name module_ obj_name)
  in
  Modules.With_vlib.obj_map modules
  |> Module_name.Unique.Map.to_list_map ~f:(fun obj_name sourced_module ->
    match sourced_module with
    | Modules.Sourced_module.Impl_of_virtual_module { intf; impl } ->
      Printf.sprintf
        "%s -> intf:%s impl:%s"
        (Module_name.Unique.to_string obj_name)
        (Module.obj_name intf |> Module_name.Unique.to_string)
        (Module.obj_name impl |> Module_name.Unique.to_string)
    | Normal _ | Imported_from_vlib _ ->
      Code_error.raise "expected a virtual-library implementation" [])
  |> Dyn.list Dyn.string
  |> Dune_tests_common.print_dyn;
  [%expect {| [ "mapped__shared -> intf:mapped__shared impl:mapped__shared" ] |}]
;;

let%expect_test "qualified-group dependency lookup" =
  let current = generated ~obj_name:"Current__Unit" [ "Current"; "Nested"; "Unit" ] in
  let child_a = generated ~obj_name:"Group__ChildA" [ "Group"; "ChildA" ] in
  let child_b = generated ~obj_name:"Group__ChildB" [ "Group"; "ChildB" ] in
  let loose = generated ~obj_name:"Loose" [ "Loose" ] in
  let modules =
    make_lib ~lib_name:"groups" [ current; child_a; child_b; loose ]
    |> Modules.With_vlib.modules
  in
  check_deps
    "qualified groups"
    modules
    ~of_:current
    [ module_name "Group"; module_name "Unit"; module_name "Loose" ]
    ~expected:
      [ "group:Group:alias"
      ; "group__ChildA:ChildA:impl"
      ; "group__ChildB:ChildB:impl"
      ; "loose:Loose:impl"
      ];
  let alias =
    generated ~kind:(Alias [ module_name "Group" ]) ~obj_name:"Group" [ "Group" ]
  in
  check_deps
    "qualified alias source"
    modules
    ~of_:alias
    [ module_name "Group"; module_name "Loose" ]
    ~expected:[];
  [%expect
    {|
    qualified groups: [ "group:Group:alias"
    ; "group__ChildA:ChildA:impl"
    ; "group__ChildB:ChildB:impl"
    ; "loose:Loose:impl"
    ]
    qualified alias source: []
    |}]
;;

let%expect_test "qualified-group dependency lookup after mapping" =
  let current = generated ~obj_name:"Current" [ "Current" ] in
  let child_a = generated ~obj_name:"Group__ChildA" [ "Group"; "ChildA" ] in
  let child_b = generated ~obj_name:"Group__ChildB" [ "Group"; "ChildB" ] in
  let modules =
    make_lib ~lib_name:"groups" [ current; child_a; child_b ] |> Modules.With_vlib.modules
  in
  let names = [ module_name "Group" ] in
  check_deps
    "before mapping"
    modules
    ~of_:current
    names
    ~expected:
      [ "group:Group:alias"; "group__ChildA:ChildA:impl"; "group__ChildB:ChildB:impl" ];
  let map_module module_ =
    let obj_name =
      Printf.sprintf "mapped__%s" (Module.obj_name module_ |> Module_name.Unique.to_string)
    in
    Module.set_obj_name module_ (Module_name.Unique.of_string obj_name)
  in
  let modules = Modules.With_vlib.map modules ~f:map_module in
  let current = map_module current in
  check_deps
    "after mapping"
    modules
    ~of_:current
    names
    ~expected:
      [ "mapped__group:Group:alias"
      ; "mapped__group__ChildA:ChildA:impl"
      ; "mapped__group__ChildB:ChildB:impl"
      ];
  [%expect
    {|
    before mapping: [ "group:Group:alias"
    ; "group__ChildA:ChildA:impl"
    ; "group__ChildB:ChildB:impl"
    ]
    after mapping: [ "mapped__group:Group:alias"
    ; "mapped__group__ChildA:ChildA:impl"
    ; "mapped__group__ChildB:ChildB:impl"
    ]
    |}]
;;

let%expect_test "wrapped compatibility self dependency" =
  let main = generated ~obj_name:"Main" [ "Main" ] in
  let child = generated ~obj_name:"Child" [ "Child" ] in
  let modules =
    make_lib
      ~wrapped:(Dune_lang.Wrapped.Simple true)
      ~main_module_name:(module_name "Main")
      ~lib_name:"wrapped"
      [ main; child ]
    |> Modules.With_vlib.modules
  in
  let self = generated ~kind:Wrapped_compat ~obj_name:"Compat__Main" [ "Main" ] in
  check_deps
    "wrapped compatibility self"
    modules
    ~of_:self
    [ module_name "Main" ]
    ~expected:[];
  let compat = generated ~kind:Wrapped_compat ~obj_name:"Compat__Child" [ "Child" ] in
  check_deps
    "wrapped compatibility interface"
    modules
    ~of_:compat
    [ module_name "Main" ]
    ~expected:[ "main:Main:impl" ];
  [%expect
    {|
    wrapped compatibility self: []
    wrapped compatibility interface: [ "main:Main:impl" ]
    |}]
;;

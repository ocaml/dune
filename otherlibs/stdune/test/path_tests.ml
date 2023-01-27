open! Stdune
open Path
open Dune_tests_common

let () = Dune_tests_common.init ()

let r = Path.(relative root)

let e = Path.of_filename_relative_to_initial_cwd

let pp_path_local fmt l = Format.pp_print_string fmt (Path.Local.to_string l)

let of_filename_relative_to_initial_cwd s =
  Path.of_filename_relative_to_initial_cwd s |> Path.to_dyn |> print_dyn

let descendant p ~of_ =
  Dyn.option Path.to_dyn (Path.descendant p ~of_) |> print_dyn

let is_descendant p ~of_ = Dyn.bool (Path.is_descendant p ~of_) |> print_dyn

let explode s =
  let open Dyn in
  let exploded = Path.explode (Path.of_string s) in
  option (list string) exploded |> print_dyn

let reach p ~from =
  let p = Path.of_string p in
  let from = Path.of_string from in
  let open Dyn in
  let reach = Path.reach p ~from in
  string reach |> print_dyn

let reach_for_running p ~from =
  Path.reach_for_running p ~from |> Dyn.string |> print_dyn

let relative p s = Path.to_dyn (Path.relative p s) |> print_dyn

let insert_after_build_dir_exn p s =
  Path.insert_after_build_dir_exn p s |> Path.to_dyn |> print_dyn

let append_source x y = Path.append_source x y |> Path.to_dyn |> print_dyn

let drop_build_context p =
  let open Dyn in
  Path.drop_build_context p |> option Path.Source.to_dyn |> print_dyn

let local_part p = Path.local_part p |> Path.Local.to_dyn |> print_dyn

let%expect_test _ =
  let p = Path.(relative root) "foo" in
  descendant p ~of_:p;
  [%expect {|
  Some In_source_tree "."
  |}]

let%expect_test _ =
  (* different strings but same length *)
  descendant (r "foo") ~of_:(r "bar");
  [%expect {|
  None
  |}]

let%expect_test _ =
  is_descendant (r "foo") ~of_:(r "foo");
  [%expect {|
  true
  |}]

let%expect_test _ =
  is_descendant (r "foo") ~of_:(r "foo/");
  [%expect {|
  true
  |}]

let%expect_test _ =
  is_descendant (r "foo/") ~of_:(r "foo");
  [%expect {|
  true
  |}]

let%expect_test _ =
  is_descendant (r "foo") ~of_:(r "bar");
  [%expect {|
  false
  |}]

let%expect_test _ =
  is_descendant (r "foo") ~of_:(r "bar/");
  [%expect {|
false
|}]

let%expect_test _ =
  is_descendant (r "foo/") ~of_:(r "bar");
  [%expect {|
false
|}]

let%expect_test _ =
  is_descendant (r "glob/foo") ~of_:(r "glob");
  [%expect {|
true
|}]

let%expect_test _ =
  is_descendant (r "glob/foo") ~of_:(r "glob/");
  [%expect {|
true
|}]

let%expect_test _ =
  is_descendant (e "/foo/bar") ~of_:(e "/foo");
  [%expect {|
false
|}]

let%expect_test _ =
  is_descendant (e "/foo/bar") ~of_:(e "/foo/bar");
  [%expect {|
false
|}]

let%expect_test _ =
  is_descendant (e "/foo/bar") ~of_:(e "/foo/bar/");
  [%expect {|
false
|}]

let%expect_test _ =
  is_descendant (e "/foo/bar/") ~of_:(e "/foo/bar");
  [%expect {|
false
|}]

let%expect_test _ =
  is_descendant (e "/foo/bar") ~of_:(e "/");
  [%expect {|
false
|}]

let%expect_test _ =
  descendant (r "foo") ~of_:(r "foo/");
  [%expect {|
Some In_source_tree "."
|}]

let%expect_test _ =
  descendant (r "foo/") ~of_:(r "foo");
  [%expect {|
Some In_source_tree "."
|}]

let%expect_test _ =
  descendant (r "foo/bar") ~of_:(r "foo");
  [%expect {|
Some In_source_tree "bar"
|}]

let%expect_test _ =
  descendant Path.root ~of_:(r "foo");
  [%expect {|
None
|}]

let%expect_test _ =
  descendant Path.root ~of_:Path.root;
  [%expect {|
Some In_source_tree "."
|}]

let%expect_test _ =
  descendant (r "foo") ~of_:Path.root;
  [%expect {|
Some In_source_tree "foo"
|}]

let%expect_test _ =
  descendant (Path.relative build_dir "foo") ~of_:root;
  [%expect {|
None
|}]

let%expect_test _ =
  descendant (Path.relative build_dir "foo") ~of_:(Path.of_string "/foo/bar");
  [%expect {|
None
|}]

let%expect_test _ =
  descendant (Path.relative build_dir "foo/bar") ~of_:build_dir;
  [%expect {|
Some In_source_tree "foo/bar"
|}]

let%expect_test _ =
  descendant
    (Path.relative build_dir "foo/bar")
    ~of_:(Path.relative build_dir "foo");
  [%expect {|
Some In_source_tree "bar"
|}]

let%expect_test _ =
  descendant
    (Path.relative build_dir "foo/bar")
    ~of_:(Path.relative build_dir "foo");
  [%expect {|
Some In_source_tree "bar"
|}]

let%expect_test _ =
  descendant (Path.of_string "/foo/bar") ~of_:(Path.of_string "/foo");
  [%expect {|
None
|}]

let%expect_test _ =
  explode "a/b/c";
  [%expect {|
Some [ "a"; "b"; "c" ]
|}]

let%expect_test _ =
  explode "a/b";
  [%expect {|
Some [ "a"; "b" ]
|}]

let%expect_test _ =
  explode "a";
  [%expect {|
Some [ "a" ]
|}]

let%expect_test _ =
  explode "";
  [%expect {|
Some []
|}]

let%expect_test _ =
  reach "/foo/baz" ~from:"/foo/bar";
  [%expect {|
"/foo/baz"
|}]

let%expect_test _ =
  reach "/foo/bar" ~from:"baz";
  [%expect {|
"/foo/bar"
|}]

let%expect_test _ =
  reach "bar/foo" ~from:"bar/baz/y";
  [%expect {|
"../../foo"
|}]

let%expect_test _ =
  relative (Path.of_string "relative") "/absolute/path";
  [%expect {|
External "/absolute/path"
|}]

let%expect_test _ =
  relative (Path.of_string "/abs1") "/abs2";
  [%expect {|
External "/abs2"
|}]

let%expect_test _ =
  relative (of_string "/abs1") "";
  [%expect {|
External "/abs1"
|}]

let%expect_test _ =
  relative root "/absolute/path";
  [%expect {|
External "/absolute/path"
|}]

let%expect_test _ =
  of_filename_relative_to_initial_cwd "/absolute/path";
  [%expect {|
External "/absolute/path"
|}]

let%expect_test _ =
  Path.is_managed (e "relative/path") |> Dyn.bool |> print_dyn;
  [%expect {|
false
|}]

let%expect_test _ =
  insert_after_build_dir_exn Path.root "foobar";
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ( "(\"Path.insert_after_build_dir_exn\",\
   \n{ path = In_source_tree \".\"; insert = \"foobar\" })") |}]

let%expect_test _ =
  insert_after_build_dir_exn Path.build_dir "foobar";
  [%expect {|
In_build_dir "foobar"
|}]

let%expect_test _ =
  insert_after_build_dir_exn (Path.relative Path.build_dir "qux") "foobar";
  [%expect {|
In_build_dir "foobar/qux"
|}]

let%expect_test _ =
  append_source Path.build_dir (Path.Source.relative Path.Source.root "foo");
  [%expect {|
In_build_dir "foo"
|}]

let%expect_test _ =
  append_source Path.root (Path.Source.relative Path.Source.root "foo");
  [%expect {|
In_source_tree "foo"
|}]

let%expect_test _ =
  append_source (Path.of_string "/root")
    (Path.Source.relative Path.Source.root "foo");
  [%expect {|
External "/root/foo"
|}]

let%expect_test _ =
  Path.rm_rf (Path.of_string "/does/not/exist/foo/bar/baz")
  |> Dyn.unit |> print_dyn;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  ( "(\"Path.rm_rf called on external dir\",\
   \n{ t = External \"/does/not/exist/foo/bar/baz\" })") |}]

let%expect_test _ =
  drop_build_context (Path.relative Path.build_dir "foo/bar");
  [%expect {|
Some In_source_tree "bar"
|}]

let%expect_test _ =
  drop_build_context (Path.of_string "foo/bar");
  [%expect {|
None
|}]

let%expect_test _ =
  drop_build_context (e "/foo/bar");
  [%expect {|
None
|}]

let%expect_test _ =
  drop_build_context Path.build_dir;
  [%expect {|
None
|}]

let%expect_test _ =
  Path.is_in_build_dir Path.build_dir |> Dyn.bool |> print_dyn;
  [%expect {|
true
|}]

let%expect_test _ =
  Path.is_strict_descendant_of_build_dir Path.build_dir |> Dyn.bool |> print_dyn;
  [%expect {|
false
|}]

let%expect_test _ =
  Path.reach_for_running Path.build_dir ~from:Path.root
  |> Dyn.string |> print_dyn;
  [%expect {|
"./_build"
|}]

let%expect_test _ =
  reach_for_running
    (Path.relative build_dir "foo/baz")
    ~from:(Path.relative build_dir "foo/bar/baz");
  [%expect {|
"../../baz"
|}]

let%expect_test _ =
  reach_for_running (e "/fake/path")
    ~from:(Path.relative build_dir "foo/bar/baz");
  [%expect {|
"/fake/path"
|}]

let%expect_test _ =
  reach_for_running (Path.relative root "foo") ~from:(Path.relative root "foo");
  [%expect {|
"./."
|}]

let%expect_test _ =
  relative Path.root "_build";
  [%expect {|
In_build_dir "."
|}]

let%expect_test _ =
  (* This is not right, but kind of annoying to fix :/ *)
  relative (r "foo") "../_build";
  [%expect {|
In_build_dir "."
|}]

let%expect_test _ =
  local_part (Path.of_string "/c/d");
  [%expect {|
"c/d"
|}]

let%expect_test _ =
  local_part (Path.insert_after_build_dir_exn Path.build_dir "c/d");
  [%expect {|
"c/d"
|}]

let%expect_test _ =
  local_part (r "c/d");
  [%expect {|
"c/d"
|}]

let%expect_test _ =
  Path.Build.extract_first_component Path.Build.root
  |> Dyn.(option (pair string Local.to_dyn))
  |> print_dyn;
  [%expect {|
None
|}]

open Stdune

let () = Dune_tests_common.init ()

(* Testing patch parsing *)

let git_ext_to_dyn = function
  | Patch.Rename_only (from, to_) ->
    Dyn.variant "Rename_only" [ Dyn.string from; Dyn.string to_ ]
  | Delete_only -> Dyn.variant "Delete_only" []
  | Create_only -> Dyn.variant "Create_only" []
;;

let operation_to_dyn = function
  | Patch.Edit (mine, their) -> Dyn.variant "Edit" [ Dyn.string mine; Dyn.string their ]
  | Delete mine -> Dyn.variant "Delete" [ Dyn.string mine ]
  | Create their -> Dyn.variant "Create" [ Dyn.string their ]
  | Git_ext (mine, their, git_ext) ->
    Dyn.variant "Git_ext" [ Dyn.string mine; Dyn.string their; git_ext_to_dyn git_ext ]
;;

let hunk_to_dyn { Patch.mine_start; mine_len; mine; their_start; their_len; their } =
  Dyn.record
    [ "mine_start", Dyn.int mine_start
    ; "mine_len", Dyn.int mine_len
    ; "mine", Dyn.list Dyn.string mine
    ; "their_start", Dyn.int their_start
    ; "their_len", Dyn.int their_len
    ; "their", Dyn.list Dyn.string their
    ]
;;

let patch_to_dyn { Patch.operation; hunks; mine_no_nl; their_no_nl } =
  Dyn.record
    [ "operation", operation_to_dyn operation
    ; "hunks", Dyn.list hunk_to_dyn hunks
    ; "mine_no_nl", Dyn.bool mine_no_nl
    ; "their_no_nl", Dyn.bool their_no_nl
    ]
;;

let test p =
  Dune_patch.For_tests.parse_patches ~loc:Loc.none p
  |> Dyn.list patch_to_dyn
  |> Dyn.pp
  |> Format.printf "%a" Pp.to_fmt
;;

let%expect_test "parse_patches - basic" =
  test Patch_examples.basic;
  [%expect
    {|
    [ { operation = Edit ("foo.ml", "foo.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - subdirectory" =
  test Patch_examples.subdirectory;
  [%expect
    {|
    [ { operation = Edit ("dir/foo.ml", "dir/foo.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - combined" =
  test Patch_examples.combined;
  [%expect
    {|
    [ { operation = Edit ("foo.ml", "foo.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ; { operation = Edit ("dir/foo.ml", "dir/foo.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - new_file" =
  test Patch_examples.new_file;
  [%expect
    {|
    [ { operation = Create "foo.ml"
      ; hunks =
          [ { mine_start = 0
            ; mine_len = 0
            ; mine = []
            ; their_start = 1
            ; their_len = 2
            ; their = [ "This is right"; "" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - delete_file" =
  test Patch_examples.delete_file;
  [%expect
    {|
    [ { operation = Delete "foo.ml"
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 0
            ; their_len = 0
            ; their = []
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - unified" =
  test Patch_examples.unified;
  [%expect
    {|
    [ { operation = Edit ("foo.ml", "foo.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - no_prefix" =
  test Patch_examples.no_prefix;
  [%expect
    {|
    [ { operation = Edit ("foo.ml", "foo.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - random_prefix" =
  test Patch_examples.random_prefix;
  [%expect
    {|
    [ { operation = Edit ("foo.ml", "foo.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - spaces" =
  (* BUG: Should be Edit ("foo bar", "foo bar") but parser truncates at space *)
  test Patch_examples.spaces;
  [%expect
    {|
    [ { operation = Edit ("foo", "foo")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - unified_spaces" =
  test Patch_examples.unified_spaces;
  [%expect
    {|
    [ { operation = Edit ("foo bar", "foo bar")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - hello_world" =
  test Patch_examples.hello_world;
  [%expect
    {|
    [ { operation = Create "foo.ml"
      ; hunks =
          [ { mine_start = 0
            ; mine_len = 0
            ; mine = []
            ; their_start = 1
            ; their_len = 1
            ; their = [ "Hello World" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - rename_patch" =
  test Patch_examples.rename_patch;
  [%expect
    {|
    [ { operation =
          Git_ext ("old.ml", "new.ml", Rename_only ("old.ml", "new.ml"))
      ; hunks = []
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - git_ext_delete_only" =
  test Patch_examples.git_ext_delete_only;
  [%expect
    {|
    [ { operation = Delete "foo.ml"
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "Hello World" ]
            ; their_start = 0
            ; their_len = 0
            ; their = []
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - git_ext_create_only" =
  test Patch_examples.git_ext_create_only;
  [%expect
    {|
    [ { operation = Create "foo.ml"
      ; hunks =
          [ { mine_start = 0
            ; mine_len = 0
            ; mine = []
            ; their_start = 1
            ; their_len = 1
            ; their = [ "Hello World" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

let%expect_test "parse_patches - edit_with_rename" =
  test Patch_examples.edit_with_rename;
  [%expect
    {|
    [ { operation = Edit ("source.ml", "target.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

(* Spurious "new file mode" - the ---/+++ headers indicate an Edit, which takes
   precedence over the git metadata. *)
let%expect_test "parse_patches - spurious new file mode" =
  test Patch_examples.spurious_new_file_mode;
  [%expect
    {|
    [ { operation = Edit ("dir/baz.ml", "dir/baz.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

(* Spurious "deleted file mode" - the ---/+++ headers indicate an Edit, which takes
   precedence over the git metadata. *)
let%expect_test "parse_patches - spurious deleted file mode" =
  test Patch_examples.spurious_deleted_file_mode;
  [%expect
    {|
    [ { operation = Edit ("foo.ml", "foo.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

(* Combined patch where second entry has spurious "new file mode" -
   the ---/+++ headers correctly override the git metadata. *)
let%expect_test "parse_patches - combined with spurious new file mode" =
  test Patch_examples.combined_with_spurious_new_file_mode;
  [%expect
    {|
    [ { operation = Edit ("bar.ml", "bar.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ; { operation = Edit ("dir/baz.ml", "dir/baz.ml")
      ; hunks =
          [ { mine_start = 1
            ; mine_len = 1
            ; mine = [ "This is wrong" ]
            ; their_start = 1
            ; their_len = 1
            ; their = [ "This is right" ]
            }
          ]
      ; mine_no_nl = false
      ; their_no_nl = false
      }
    ]
    |}]
;;

(* Testing parsing of bad patch names *)

let bad_name name =
  {|
--- /dev/null
+++ |}
  ^ name
  ^ {|
@@ -0,0 +1 @@
+x
|}
;;

let test name =
  let loc = Loc.in_file (Path.of_string "dummy.file") in
  match Dune_patch.For_tests.parse_patches ~loc (bad_name name) with
  | exception e -> Exn.pp e |> Format.printf "%a" Pp.to_fmt
  | _ -> print_endline "No error!"
;;

let%expect_test "parse_patches - reject current dir" =
  (* We don't allow "." *)
  test ".";
  [%expect
    {|
    File "dummy.file", line 1, characters 0-0:
    Error: Directory "." in patch file is
    invalid.
  |}]
;;

let%expect_test "parse_patches - reject parent paths" =
  (* We wish to reject all patches beginning with "..". *)
  test "../a";
  [%expect
    {|
    File "dummy.file", line 1, characters 0-0:
    Error: Patch files may not reference paths starting with ".." as they
    would
    access files outside the
    project.
    |}]
;;

let%expect_test "parse_patches - allow relative parent" =
  (* This is fine, since Dune is able to understand the path. *)
  test "a/../b";
  [%expect {| No error! |}]
;;

let%expect_test "parse_patches - reject invalid parent" =
  test "a/..";
  [%expect
    {|
    File "dummy.file", line 1, characters 0-0:
    Error: Directory "a/.." in patch file is
    invalid.
    |}]
;;

let%expect_test "parse_patches - reject absolute paths" =
  test "/a";
  [%expect
    {|
    File "dummy.file", line 1, characters 0-0:
    Error: Absolute path "/a" in patch file is not
    allowed.
    |}]
;;

let%expect_test "parse_patches - reject empty/unparseable patch" =
  let loc = Loc.in_file (Path.of_string "dummy.file") in
  (match Dune_patch.For_tests.parse_patches ~loc "this is not a valid patch file" with
   | exception e -> Exn.pp e |> Format.printf "%a" Pp.to_fmt
   | _ -> print_endline "No error!");
  [%expect
    {|
    File "dummy.file", line 1, characters 0-0:
    Error: Could not parse the patch file. Only unified diff format is
    supported.
    Context diffs and ed commands are not
    supported.
    |}]
;;

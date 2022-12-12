open Stdune

let cmd fmt =
  Printf.ksprintf
    (fun s ->
      let (_ : int) = Sys.command s in
      ())
    fmt

module Test = struct
  type t = { dir : Path.t }

  let restore_cwd =
    let cwd = Sys.getcwd () in
    fun () -> Sys.chdir cwd

  let file t ~fname ~contents =
    let path = Path.relative t.dir fname in
    Io.write_file path contents;
    path

  let print_errors =
    List.iteri ~f:(fun i report ->
        printfn ">> error %d" i;
        print_endline (Dyn.to_string (Ocamlc_loc.dyn_of_report report)))

  let create f =
    let dir = Temp.create Dir ~prefix:"dune." ~suffix:".test" in
    let t = { dir } in
    Sys.chdir (Path.to_string dir);
    let output =
      let out_file = Exn.protect ~f:(fun () -> f t) ~finally:restore_cwd in
      let output = Io.read_file out_file in
      Format.asprintf "%a@." Pp.to_fmt (Ansi_color.parse output)
    in
    (* Format.eprintf "print raw output:@.%s@.%!" output; *)
    Ocamlc_loc.parse output |> print_errors
end

(* FIXME: unused value warning isn't parsed correctly - the file excerpt isn't
   extracted *)
let%expect_test "unused value" =
  let raw_error =
    String.trim
      {|
File "test.ml", line 1, characters 4-7:
1 | let foo = ()
        ^^^
Error (warning 32 [unused-value-declaration]): unused value foo.
    |}
  in
  String.split_lines raw_error
  |> String.concat ~sep:"\r\n" |> Ocamlc_loc.parse |> Test.print_errors;
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Single 1; chars = Some (4, 7) }
    ; message = "unused value foo."
    ; related = []
    ; severity = Error Some { code = 32; name = "unused-value-declaration" }
    } |}]

let test_error raw_error =
  String.trim raw_error |> Ocamlc_loc.parse |> Test.print_errors

let test_error_raw raw_error =
  String.trim raw_error |> Ocamlc_loc.parse_raw |> Ocamlc_loc.dyn_of_raw
  |> Dyn.to_string |> print_endline

let%expect_test "mli mismatch" =
  test_error
    {|
File "test.ml", line 1:
Error: The implementation test.ml does not match the interface test.cmi:
       Values do not match: val x : bool is not included in val x : int
       The type bool is not compatible with the type int
       File "test.mli", line 1, characters 0-11: Expected declaration
       File "test.ml", line 1, characters 4-5: Actual declaration
|};
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Single 1; chars = None }
    ; message =
        "The implementation test.ml does not match the interface test.cmi:\n\
         Values do not match: val x : bool is not included in val x : int\n\
         The type bool is not compatible with the type int"
    ; related =
        [ ({ path = "test.mli"; line = Single 1; chars = Some (0, 11) },
          "Expected declaration")
        ; ({ path = "test.ml"; line = Single 1; chars = Some (4, 5) },
          "Actual declaration")
        ]
    ; severity = Error None
    } |}]

let%expect_test "" =
  test_error
    {|
File "test.ml", line 1, characters 9-12:
1 | let () = 123
             ^^^
Error: This expression has type int but an expression was expected of type
         unit
|};
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Single 1; chars = Some (9, 12) }
    ; message =
        "This expression has type int but an expression was expected of type\n\
        \  unit"
    ; related = []
    ; severity = Error None
    } |}]

let%expect_test "warning" =
  test_error
    {|
File "test.ml", line 1, characters 13-14:
1 | let () = let x = 2 in ()
                 ^
Warning 26 [unused-var]: unused variable x.
|};
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Single 1; chars = Some (13, 14) }
    ; message = "unused variable x."
    ; related = []
    ; severity = Warning { code = 26; name = "unused-var" }
    } |}]

let%expect_test "" =
  test_error
    {|
File "test.ml", lines 3-5, characters 6-3:
3 | ......struct
4 |   let x y = y +. 2.0
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val x : float -> float end
       is not included in
         sig val x : int -> int end
       Values do not match:
         val x : float -> float
       is not included in
         val x : int -> int
       The type float -> float is not compatible with the type int -> int
       Type float is not compatible with type int
       File "test.ml", line 2, characters 2-20: Expected declaration
       File "test.ml", line 4, characters 6-7: Actual declaration
|};
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Range 3,5; chars = Some (6, 3) }
    ; message =
        "Signature mismatch:\n\
         Modules do not match:\n\
        \  sig val x : float -> float end\n\
         is not included in\n\
        \  sig val x : int -> int end\n\
         Values do not match:\n\
        \  val x : float -> float\n\
         is not included in\n\
        \  val x : int -> int\n\
         The type float -> float is not compatible with the type int -> int\n\
         Type float is not compatible with type int"
    ; related =
        [ ({ path = "test.ml"; line = Single 2; chars = Some (2, 20) },
          "Expected declaration")
        ; ({ path = "test.ml"; line = Single 4; chars = Some (6, 7) },
          "Actual declaration")
        ]
    ; severity = Error None
    } |}]

let%expect_test "ml mli mismatch 2" =
  test_error
    {|
File "src/dune_rules/artifacts.ml", line 1:
Error: The implementation src/dune_rules/artifacts.ml
       does not match the interface src/dune_rules/.dune_rules.objs/byte/dune_rules__Artifacts.cmi:
        ... ... In module Bin.Local:
       Values do not match:
         val equal :
           Import.Path.Build.t Import.String.Set.map ->
           Import.Path.Build.t Import.String.Set.map -> bool
       is not included in
         val equal : t -> bool -> bool
       The type
         Import.Path.Build.t Import.String.Set.map ->
         Import.Path.Build.t Import.String.Set.map -> bool
       is not compatible with the type t -> bool -> bool
       Type Import.Path.Build.t Import.String.Set.map
       is not compatible with type bool
       File "src/dune_rules/artifacts.mli", line 20, characters 4-33:
         Expected declaration
       File "src/dune_rules/artifacts.ml", line 50, characters 8-13:
         Actual declaration
         |};
  [%expect
    {|
    >> error 0
    { loc =
        { path = "src/dune_rules/artifacts.ml"; line = Single 1; chars = None }
    ; message =
        "The implementation src/dune_rules/artifacts.ml\n\
         does not match the interface src/dune_rules/.dune_rules.objs/byte/dune_rules__Artifacts.cmi:\n\
        \ ... ... In module Bin.Local:\n\
         Values do not match:\n\
        \  val equal :\n\
        \    Import.Path.Build.t Import.String.Set.map ->\n\
        \    Import.Path.Build.t Import.String.Set.map -> bool\n\
         is not included in\n\
        \  val equal : t -> bool -> bool\n\
         The type\n\
        \  Import.Path.Build.t Import.String.Set.map ->\n\
        \  Import.Path.Build.t Import.String.Set.map -> bool\n\
         is not compatible with the type t -> bool -> bool\n\
         Type Import.Path.Build.t Import.String.Set.map\n\
         is not compatible with type bool"
    ; related =
        [ ({ path = "src/dune_rules/artifacts.mli"
           ; line = Single 20
           ; chars = Some (4, 33)
           },
          "Expected declaration")
        ; ({ path = "src/dune_rules/artifacts.ml"
           ; line = Single 50
           ; chars = Some (8, 13)
           },
          "Actual declaration")
        ]
    ; severity = Error None
    } |}]

let%expect_test "" =
  test_error
    {|
File "fooexe.ml", line 3, characters 0-7:
3 | Bar.run ();;
    ^^^^^^^
Error (alert deprecated): module Bar
Will be removed past 2020-20-20. Use Mylib.Bar instead.
File "fooexe.ml", line 4, characters 0-7:
4 | Foo.run ();;
    ^^^^^^^
Error (alert deprecated): module Foo
Will be removed past 2020-20-20. Use Mylib.Foo instead.
File "fooexe.ml", line 7, characters 11-22:
7 | module X : Intf_only.S = struct end
               ^^^^^^^^^^^
Error (alert deprecated): module Intf_only
Will be removed past 2020-20-20. Use Mylib.Intf_only instead.
|};
  [%expect
    {|
    >> error 0
    { loc = { path = "fooexe.ml"; line = Single 3; chars = Some (0, 7) }
    ; message =
        "module Bar\n\
         Will be removed past 2020-20-20. Use Mylib.Bar instead."
    ; related = []
    ; severity = Error Some "deprecated"
    }
    >> error 1
    { loc = { path = "fooexe.ml"; line = Single 4; chars = Some (0, 7) }
    ; message =
        "module Foo\n\
         Will be removed past 2020-20-20. Use Mylib.Foo instead."
    ; related = []
    ; severity = Error Some "deprecated"
    }
    >> error 2
    { loc = { path = "fooexe.ml"; line = Single 7; chars = Some (11, 22) }
    ; message =
        "module Intf_only\n\
         Will be removed past 2020-20-20. Use Mylib.Intf_only instead."
    ; related = []
    ; severity = Error Some "deprecated"
    } |}]

let%expect_test "undefined fields" =
  test_error
    {|
File "test/expect-tests/timer_tests.ml", lines 6-10, characters 2-3:
 6 | ..{ Scheduler.Config.concurrency = 1
 7 |   ; display = { verbosity = Short; status_line = false }
 8 |   ; stats = None
 9 |   ; insignificant_changes = `React
10 |   }
Error: Some record fields are undefined: signal_watcher
|};
  [%expect
    {|
    >> error 0
    { loc =
        { path = "test/expect-tests/timer_tests.ml"
        ; line = Range 6,10
        ; chars = Some (2, 3)
        }
    ; message = "Some record fields are undefined: signal_watcher"
    ; related = []
    ; severity = Error None
    } |}]

let%expect_test "undefined fields" =
  test_error_raw {|
Error: Some record fields are undefined: signal_watcher
|};
  [%expect
    {|
    [ "Error: Some record fields are undefined: signal_watcher" ] |}]

let%expect_test "test error from merlin" =
  test_error_raw
    {|Signature mismatch:
Modules do not match:
  sig val x : int end
is not included in
  sig val x : unit end
Values do not match: val x : int is not included in val x : unit
The type int is not compatible with the type unit
File "test.ml", line 2, characters 2-14: Expected declaration
File "test.ml", line 4, characters 6-7: Actual declaration
  |};
  [%expect
    {|
    [ "Signature mismatch:\n\
       Modules do not match:\n\
      \  sig val x : int end\n\
       is not included in\n\
      \  sig val x : unit end\n\
       Values do not match: val x : int is not included in val x : unit\n\
       The type int is not compatible with the type unit"
    ; { path = "test.ml"; line = Single 2; chars = Some (2, 14) }
    ; "Expected declaration"
    ; { path = "test.ml"; line = Single 4; chars = Some (6, 7) }
    ; "Actual declaration"
    ] |}]

let%expect_test "ml/mli error" =
  test_error
    {|
File "src/dune_engine/build_system.ml", line 1:
Error: The implementation src/dune_engine/build_system.ml
      does not match the interface src/dune_engine/.dune_engine.objs/byte/dune_engine__Build_system.cmi:
        The value `dune_stats' is required but not provided
      File "src/dune_engine/build_system.mli", line 8, characters 0-40:
        Expected declaration
        |};
  [%expect
    {|
    >> error 0
    { loc =
        { path = "src/dune_engine/build_system.ml"
        ; line = Single 1
        ; chars = None
        }
    ; message =
        "The implementation src/dune_engine/build_system.ml\n\
         does not match the interface src/dune_engine/.dune_engine.objs/byte/dune_engine__Build_system.cmi:\n\
        \  The value `dune_stats' is required but not provided"
    ; related =
        [ ({ path = "src/dune_engine/build_system.mli"
           ; line = Single 8
           ; chars = Some (0, 40)
           },
          "Expected declaration")
        ]
    ; severity = Error None
    } |}]

let%expect_test "ml/mli error" =
  test_error
    {|
File "bin/common.ml", line 1004, characters 8-43:
1004 |         Dune_engine.Build_system.dune_stats := Some stats;
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unbound value Dune_engine.Build_system.dune_stats
          |};
  [%expect
    {|
    >> error 0
    { loc = { path = "bin/common.ml"; line = Single 1004; chars = Some (8, 43) }
    ; message = "Unbound value Dune_engine.Build_system.dune_stats"
    ; related = []
    ; severity = Error None
    } |}]

let%expect_test "alert" =
  test_error
    {|
File "foo.ml", line 8, characters 9-12:
8 | let () = A.f
             ^^^
Alert deprecated: A.f
foo
  |};
  [%expect
    {|
    >> error 0
    { loc = { path = "foo.ml"; line = Single 8; chars = Some (9, 12) }
    ; message = "foo"
    ; related = []
    ; severity = Alert { name = "deprecated"; source = " A.f" }
    } |}];
  test_error
    {|
File "foo.ml", line 8, characters 9-12:
8 | let () = A.f
             ^^^
Alert foobar: A.f
blah
|};
  [%expect
    {|
    >> error 0
    { loc = { path = "foo.ml"; line = Single 8; chars = Some (9, 12) }
    ; message = "blah"
    ; related = []
    ; severity = Alert { name = "foobar"; source = " A.f" }
    } |}]

let%expect_test "multiple errors in one file" =
  test_error
    {|
File "foo.ml", line 8, characters 8-11:
8 | let f = A.f
            ^^^
Alert deprecated: A.f
foo
File "foo.ml", line 9, characters 8-11:
9 | let g = A.f
            ^^^
Alert deprecated: A.f
foo
File "foo.ml", line 10, characters 8-11:
10 | let h = A.f
             ^^^
Alert deprecated: A.f
foo
|};
  [%expect
    {|
    >> error 0
    { loc = { path = "foo.ml"; line = Single 8; chars = Some (8, 11) }
    ; message = "foo"
    ; related = []
    ; severity = Alert { name = "deprecated"; source = " A.f" }
    }
    >> error 1
    { loc = { path = "foo.ml"; line = Single 9; chars = Some (8, 11) }
    ; message = "foo"
    ; related = []
    ; severity = Alert { name = "deprecated"; source = " A.f" }
    }
    >> error 2
    { loc = { path = "foo.ml"; line = Single 10; chars = Some (8, 11) }
    ; message = "foo"
    ; related = []
    ; severity = Alert { name = "deprecated"; source = " A.f" }
    } |}]

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

  let create f =
    let dir = Temp.create Dir ~prefix:"dune." ~suffix:".test" in
    let t = { dir } in
    Sys.chdir (Path.to_string dir);
    let out_file = Exn.protect ~f:(fun () -> f t) ~finally:restore_cwd in
    let output = Io.read_file out_file in
    (* Format.eprintf "print raw output:@.%s@.%!" output; *)
    let locs =
      Ocamlc_loc.parse
        (Format.asprintf "%a@." Pp.to_fmt (Ansi_color.parse output))
    in
    List.iteri locs ~f:(fun i report ->
        printfn ">> error %d" i;
        print_endline (Dyn.to_string (Ocamlc_loc.dyn_of_report report)))
end

let%expect_test "" =
  Test.create (fun t ->
      let open Test in
      let (_ : Path.t) = file t ~fname:"test.ml" ~contents:"let () = 123" in
      cmd "ocamlc -c test.ml 2> out";
      Path.relative t.dir "out");
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Single 1; chars = Some (9, 12) }
    ; message =
        Structured
          { file_excerpt = Some "1 | let () = 123\n\
                                \             ^^^\n\
                                 "
          ; message =
              "This expression has type int but an expression was expected of type\n\
              \         unit\n\
               "
          ; severity = Error
          }
    ; related = []
    } |}]

let%expect_test "" =
  Test.create (fun t ->
      let open Test in
      let (_ : Path.t) =
        file t ~fname:"test.ml"
          ~contents:
            {ocaml|
module X : sig
  val x : int -> int
end = struct
  let x y = y +. 2.0
end
|ocaml}
      in
      cmd "ocamlc -c test.ml 2> out";
      Path.relative t.dir "out");
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Range 4,6; chars = Some (6, 3) }
    ; message =
        Structured
          { file_excerpt =
              Some "4 | ......struct\n\
                    5 |   let x y = y +. 2.0\n\
                    6 | end\n\
                    "
          ; message =
              "Signature mismatch:\n\
              \       Modules do not match:\n\
              \         sig val x : float -> float end\n\
              \       is not included in\n\
              \         sig val x : int -> int end\n\
              \       Values do not match:\n\
              \         val x : float -> float\n\
              \       is not included in\n\
              \         val x : int -> int\n\
              \       The type float -> float is not compatible with the type int -> int\n\
              \       Type float is not compatible with type int "
          ; severity = Error
          }
    ; related =
        [ ({ path = "test.ml"; line = Single 3; chars = Some (2, 20) },
          Raw "Expected declaration")
        ; ({ path = "test.ml"; line = Single 5; chars = Some (6, 7) },
          Raw "Actual declaration\n\
               ")
        ]
    } |}]

let%expect_test "warning" =
  Test.create (fun t ->
      let open Test in
      let (_ : Path.t) =
        file t ~fname:"test.ml" ~contents:"let () = let x = 2 in ()"
      in
      cmd "ocamlc -c test.ml 2> out";
      Path.relative t.dir "out");
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Single 1; chars = Some (13, 14) }
    ; message =
        Structured
          { file_excerpt =
              Some "1 | let () = let x = 2 in ()\n\
                   \                 ^\n\
                    "
          ; message = "unused variable x.\n\
                       "
          ; severity = Warning Some { code = 26; name = "unused-var" }
          }
    ; related = []
    } |}]

let%expect_test "mli mismatch" =
  Test.create (fun t ->
      let open Test in
      let (_ : Path.t) = file t ~fname:"test.mli" ~contents:"val x : int" in
      let (_ : Path.t) = file t ~fname:"test.ml" ~contents:"let x = false" in
      cmd "ocamlc -c test.mli 2> /dev/null";
      cmd "ocamlc -c test.ml 2> out";
      Path.relative t.dir "out");
  [%expect
    {|
    >> error 0
    { loc = { path = "test.ml"; line = Single 1; chars = None }
    ; message =
        Structured
          { file_excerpt = None
          ; message =
              "The implementation test.ml does not match the interface test.cmi: \n\
              \       Values do not match: val x : bool is not included in val x : int\n\
              \       The type bool is not compatible with the type int"
          ; severity = Error
          }
    ; related =
        [ ({ path = "test.mli"; line = Single 1; chars = Some (0, 11) },
          Raw "Expected declaration")
        ; ({ path = "test.ml"; line = Single 1; chars = Some (4, 5) },
          Raw "Actual declaration\n\
               ")
        ]
    } |}]

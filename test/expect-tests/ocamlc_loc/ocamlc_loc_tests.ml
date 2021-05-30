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

  let string_of_loc { Ocamlc_loc.path; line; chars } =
    sprintf "file %s, line %s%s" path
      (match line with
      | `Single i -> string_of_int i
      | `Range (x, y) -> sprintf "%d-%d" x y)
      (match chars with
      | None -> ""
      | Some (x, y) -> sprintf ", chars %d-%d" x y)

  let print_message message =
    match (message : Ocamlc_loc.message) with
    | Raw s -> printfn "raw message =\n%s" s
    | Structured { preview; message; severity } ->
      printfn "severity = %s"
        (match severity with
        | Error -> "error"
        | Warning { code; name } -> sprintf "warning %d [%s]" code name);
      Option.iter preview ~f:(printfn "preview =\n%s");
      printfn "message = %s" message

  let create f =
    let dir = Temp.create Dir ~prefix:"dune." ~suffix:".test" in
    let t = { dir } in
    Sys.chdir (Path.to_string dir);
    let out_file = Exn.protect ~f:(fun () -> f t) ~finally:restore_cwd in
    let output = Io.read_file out_file in
    (* Format.eprintf "print raw output:@.%s@.%!" output; *)
    let locs = Ocamlc_loc.parse output in
    List.iteri locs ~f:(fun i { Ocamlc_loc.loc; message; related } ->
        printfn ">> error %d" i;
        printfn "loc = %s" (string_of_loc loc);
        print_message message;
        if related <> [] then (
          printfn "related errors:";
          List.iter related ~f:(fun (loc, message) ->
              print_message message;
              printfn "loc = %s" (string_of_loc loc))
        ))
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
    loc = file test.ml, line 1, chars 9-12
    severity = error
    preview =
    1 | let () = 123
                 ^^^

    message = This expression has type int but an expression was expected of type
             unit |}]

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
    loc = file test.ml, line 4-6, chars 6-3
    severity = error
    preview =
    4 | ......struct
    5 |   let x y = y +. 2.0
    6 | end

    message = Signature mismatch:
           Modules do not match:
             sig val x : float -> float end
           is not included in
             sig val x : int -> int end
           Values do not match:
             val x : float -> float
           is not included in
             val x : int -> int

    >> error 1
    loc = file test.ml, line 3, chars 2-20
    raw message =
    Expected declaration

    >> error 2
    loc = file test.ml, line 5, chars 6-7
    raw message =
    Actual declaration |}]

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
   loc = file test.ml, line 1, chars 13-14
   severity = warning 26 [unused-var]
   preview =
   1 | let () = let x = 2 in ()
                    ^

   message = unused variable x. |}]

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
   loc = file test.ml, line 1
   severity = error
   message = The implementation test.ml does not match the interface test.cmi:
          Values do not match: val x : bool is not included in val x : int

   >> error 1
   loc = file test.mli, line 1, chars 0-11
   raw message =
   Expected declaration

   >> error 2
   loc = file test.ml, line 1, chars 4-5
   raw message =
   Actual declaration |}]

open Stdune
module Re = Dune_re

let () =
  Dune_tests_common.init ();
  (* We explitily override the console width to fall back to 40. *)
  Dune_config.Config.init
    (String.Map.of_list_exn [ "console_width_fallback", (Loc.none, "40") ])
;;

let escape_and_split_lines str = String.split_lines str |> List.map ~f:String.escaped

(* Split on new lines and then escape remaining control characters. *)
let escape str = escape_and_split_lines str |> List.iter ~f:print_endline

(* Split on new lines and then escape remaining control characters. Also print:
   - Number of lines in [str].
   - Number of line clears "\027[2K".
   - Number of carriage returns.
*)
let escape_and_count =
  let clear_re = Re.compile (Re.str "\027[2K") in
  fun str ->
    let lines = escape_and_split_lines str in
    List.iter lines ~f:print_endline;
    print_endline "-------";
    Printf.printf "Line count: %d\n" (List.length lines);
    ((* We pad the string with spaces to ensure that we don't miss any clears at the end
        of the string. *)
     let lines_split_by_clear = Re.split clear_re (" " ^ str ^ " ") in
     List.length lines_split_by_clear - 1 |> Printf.printf "Clear count: %d\n");
    let lines_split_by_cr = String.split_on_char ~sep:'\r' str in
    List.length lines_split_by_cr - 1 |> Printf.printf "Carriage return count: %d\n"
;;

(* Creation of Dune_console is stateful so we introduce a new module for each test. *)
module New () = Dune_console

module type New_console = module type of Dune_console

(* In order to keep tests accross different backends consistent, we create some generic
   test scripts here that take the created [Console]. We then test these for each
   backend. *)

let test_basic_usage (module Console : New_console) =
  Console.printf "Hello World!";
  Console.print
    [ Pp.textf
        "Hello this is a very long sentence that will probably wrap the console by the \
         time that this is over."
    ]
;;

let test_status_clearing (module Console : New_console) =
  let open Console in
  Status.set (Status.Constant [ Pp.text "Here is a status line" ]);
  Status.clear ()
;;

let test_status_clearing_with_wrapping (module Console : New_console) =
  let open Console in
  Status.set
    (Status.Constant
       [ Pp.hovbox
         @@ Pp.text
              "This status line is a problem because of the fact that it is especially \
               long and therefore will not be cleared properly."
       ]);
  Status.clear ()
;;

let test_status_clearing_multiline (module Console : New_console) =
  let open Console in
  Status.set
    (Status.Constant
       [ Pp.verbatim "Some"
       ; Pp.verbatim "multiline"
       ; Pp.verbatim "status"
       ; Pp.verbatim "line"
       ]);
  Status.clear ()
;;

let test_status_overwrite (module Console : New_console) =
  let open Console in
  Status.set (Status.Constant [ Pp.text "Here is a status line" ]);
  Status.set (Status.Constant [ Pp.text "Here is another status line" ])
;;

(* Dumb backend *)

let%expect_test "basic usage" =
  let module Console = New () in
  Console.Backend.set Console.Backend.dumb;
  test_basic_usage (module Console);
  escape [%expect.output];
  [%expect
    {|
Hello World!
Hello this is a very long sentence that will probably wrap the console by the
time that this is over.
  |}]
;;

let%expect_test "Status clearing." =
  let module Console = New () in
  Console.Backend.set Console.Backend.dumb;
  test_status_clearing (module Console);
  escape [%expect.output];
  [%expect {|
Here is a status line
  |}]
;;

let%expect_test "Status clearing with wrapping." =
  let module Console = New () in
  Console.Backend.set Console.Backend.dumb;
  test_status_clearing_with_wrapping (module Console);
  escape [%expect.output];
  [%expect
    {|
This status line is a problem because of the fact that it is especially long
and therefore will not be cleared properly.
  |}]
;;

let%expect_test "Multi-line status clearing." =
  let module Console = New () in
  Console.Backend.set Console.Backend.dumb;
  test_status_clearing_multiline (module Console);
  escape [%expect.output];
  [%expect {|
Some
multiline
status
line
  |}]
;;

let%expect_test "Status overwriting." =
  let module Console = New () in
  Console.Backend.set Console.Backend.dumb;
  test_status_overwrite (module Console);
  escape [%expect.output];
  [%expect {|
Here is a status line
Here is another status line
  |}]
;;

(* Progress backend *)

(* The status lines here will be truncated to the value of the console fallback width we
   set above. *)

let%expect_test "basic usage" =
  let module Console = New () in
  Console.Backend.set Console.Backend.progress;
  test_basic_usage (module Console);
  escape_and_count [%expect.output];
  [%expect
    {|
Hello World!
Hello this is a very long sentence that will probably wrap the console by the
time that this is over.
-------
Line count: 3
Clear count: 0
Carriage return count: 0
  |}]
;;

let%expect_test "Status clearing." =
  let module Console = New () in
  Console.Backend.set Console.Backend.progress;
  test_status_clearing (module Console);
  escape_and_count [%expect.output];
  [%expect
    {|
    Here is a status line\r\027[2K
    -------
    Line count: 1
    Clear count: 1
    Carriage return count: 1 |}]
;;

let%expect_test "Status clearing with wrapping." =
  let module Console = New () in
  Console.Backend.set Console.Backend.progress;
  test_status_clearing_with_wrapping (module Console);
  escape_and_count [%expect.output];
  [%expect
    {|
    This status line is a problem because...\r\027[2K
    -------
    Line count: 1
    Clear count: 1
    Carriage return count: 1 |}]
;;

let%expect_test "Multi-line status clearing." =
  let module Console = New () in
  Console.Backend.set Console.Backend.progress;
  test_status_clearing_multiline (module Console);
  escape_and_count [%expect.output];
  [%expect
    {|
    Some
    multiline
    status
    line\r\027[2K\027M\027[2K\027M\027[2K\027M\027[2K
    -------
    Line count: 4
    Clear count: 4
    Carriage return count: 1 |}]
;;

let%expect_test "Status line overwriting." =
  let module Console = New () in
  Console.Backend.set Console.Backend.progress;
  test_status_overwrite (module Console);
  escape_and_count [%expect.output];
  [%expect
    {|
    Here is a status line\r\027[2KHere is another status line
    -------
    Line count: 1
    Clear count: 1
    Carriage return count: 1 |}]
;;

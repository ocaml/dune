open Stdune
module Re = Dune_re

let escape str =
  str |> String.split_lines |> List.map ~f:String.escaped |> List.iter ~f:prerr_endline
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

let test_status_line_clearing (module Console : New_console) =
  let open Console in
  Status_line.set (Status_line.Constant (Pp.text "Here is a status line"));
  Status_line.set (Status_line.Constant (Pp.text "Here is another"));
  Status_line.clear ()
;;

let test_status_line_clearing_with_wrapping (module Console : New_console) =
  let open Console in
  Status_line.set
    (Status_line.Constant
       (Pp.hovbox
        @@ Pp.text
             "This status line is a problem because of the fact that it is especially \
              long and therefore will not be cleared properly."));
  Status_line.set (Status_line.Constant (Pp.text "Here is another"));
  Status_line.clear ()
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

let%expect_test "Status line clearing." =
  let module Console = New () in
  Console.Backend.set Console.Backend.dumb;
  test_status_line_clearing (module Console);
  escape [%expect.output];
  [%expect {|
Here is a status line
Here is another
  |}]
;;

let%expect_test "Status line clearing with wrapping." =
  let module Console = New () in
  Console.Backend.set Console.Backend.dumb;
  test_status_line_clearing_with_wrapping (module Console);
  escape [%expect.output];
  [%expect
    {|
This status line is a problem because of the fact that it is especially long
and therefore will not be cleared properly.
Here is another
  |}]
;;

(* Progress backend *)

let%expect_test "basic usage" =
  let module Console = New () in
  Console.Backend.set Console.Backend.progress;
  test_basic_usage (module Console);
  escape [%expect.output];
  [%expect
    {|
Hello World!
Hello this is a very long sentence that will probably wrap the console by the
time that this is over.
  |}]
;;

let%expect_test "Status line clearing." =
  let module Console = New () in
  Console.Backend.set Console.Backend.progress;
  test_status_line_clearing (module Console);
  escape [%expect.output];
  [%expect
    {| 
Here is a status line\r                     \rHere is another\r               \r
 |}]
;;

(* CR-someday alizter: this should insert the appropriate number of "\r"s in order to
   fully clear the previous lines when wrapped. *)
let%expect_test "Status line clearing with wrapping." =
  let module Console = New () in
  Console.Backend.set Console.Backend.progress;
  test_status_line_clearing_with_wrapping (module Console);
  escape [%expect.output];
  [%expect
    {| 
This status line is a problem because of the fact that it is especially long
and therefore will not be cleared properly.\r                                                                                                                        \rHere is another\r               \r
 |}]
;;

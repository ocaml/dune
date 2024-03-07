open Stdune
module Console = Dune_console

(** [dummy_results a b c d] creates a dummy result with [a]/[b] immediate dependencies and
    [c]/[d] transitive dependencies. The total number of dependencies will be [b] + [d]
    of which [a] + [b] will be outdated. *)
let dummy_results
  number_of_immediate
  total_number_of_immediate
  number_of_transitive
  total_number_of_transitive
  =
  List.init (total_number_of_immediate - number_of_immediate) ~f:(fun _ ->
    Dune_pkg_outdated.For_tests.package_is_best_candidate)
  @ List.init number_of_immediate ~f:(fun i ->
    Dune_pkg_outdated.For_tests.better_candidate
      ~is_immediate_dep_of_local_package:true
      ~name:(sprintf "foo%d" i)
      ~newer_version:(Dune_pkg.Package_version.of_string "2.0.0")
      ~outdated_version:(Dune_pkg.Package_version.of_string "1.0.0"))
  @ List.init (total_number_of_transitive - number_of_transitive) ~f:(fun _ ->
    Dune_pkg_outdated.For_tests.package_is_best_candidate)
  @ List.init number_of_transitive ~f:(fun i ->
    Dune_pkg_outdated.For_tests.better_candidate
      ~is_immediate_dep_of_local_package:false
      ~name:(sprintf "bar%d" i)
      ~newer_version:(Dune_pkg.Package_version.of_string "2.0.0")
      ~outdated_version:(Dune_pkg.Package_version.of_string "1.0.0"))
;;

(* This will comb through a [User_message.Style.t Pp.t] message and find the style that
   has been applied to the first line. It will then output the same line with the style
   pretty printed in front of it. *)
let show_styles_of_line line =
  if line = Pp.nop
  then line
  else (
    let found_style = ref None in
    let (_ : unit Pp.t) = Pp.map_tags line ~f:(fun style -> found_style := Some style) in
    match !found_style with
    | None -> Pp.concat [ Pp.text "[no style] "; line ]
    | Some styles ->
      Pp.concat
        [ styles |> User_message.Style.to_dyn |> Dyn.to_string |> Pp.textf "[%s]  "
        ; line
        ])
;;

(* [test_message ~transitive a b c d] prints a message saying that out of [b] immediate
   dependencies [a] were outdated and out of [d] transitive dependencies [c] were
   outdated. Depending on the value of [transitive] it may output a helper message. It
   will also prefix the lines with the style that has been applied. *)
let test_message
  ~transitive
  number_of_immediate
  total_number_of_immediate
  number_of_transitive
  total_number_of_transitive
  =
  let results =
    dummy_results
      number_of_immediate
      total_number_of_immediate
      number_of_transitive
      total_number_of_transitive
  in
  let lock_dir_path = Stdune.Path.Source.of_string "dune.lock" in
  let message =
    Dune_pkg_outdated.For_tests.explain_results ~transitive ~lock_dir_path results
  in
  Console.print (List.map ~f:show_styles_of_line message)
;;

(* Testing the oudated packages message.

   This message we give with "dune pkg outdated" needs to have the following properties
   which we will check for here.

   1. The message should be clear and concise.

   2. It should contain information about the total number of packages in the lock file.

   3. It should contain information about the number of outdated packages in the lock
   file.

   4. It should contain information about outdated transitive dependencies. By default we
   choose to show only immediate dependencies, however in the case there are outdated
   dependencies, we should go out of our way to inform the user that --transitive may
   be passed to see these. Note that when --transitive is passed, this helper message
   will no longer be displayed.

   We will begin with the 4th property and then test different combinations of transitive
   and immediate deps to assertain the satisfaction of properties 1-3.
*)

(* When --transitive is not passed, we include a helper message to inform the user that
   there are transitive dependencies that are outdated. This message should only appear
   when there are transitive dependencies present however. *)
let%expect_test "transitive helper message" =
  (* Transitive dependencies, helper message in the transitive = true case. *)
  test_message ~transitive:true 0 0 10 20;
  [%expect {| [Warning]  10/20 packages in dune.lock are outdated. |}];
  test_message ~transitive:false 0 0 10 20;
  [%expect
    {|
    [Warning]  10/20 packages in dune.lock are outdated.
    [no style] Showing immediate dependencies, use --transitive to see the rest. |}];
  (* No transitive dependencies, no helper message in both cases. *)
  test_message ~transitive:true 10 20 0 0;
  [%expect {| [Warning]  10/20 packages in dune.lock are outdated. |}];
  test_message ~transitive:false 10 20 0 0;
  [%expect {| [Warning]  10/20 packages in dune.lock are outdated. |}]
;;

(* [test a b c d] prints a message saying that out of [b] immediate dependencies [a] were
   outdated and out of [d] transitive dependencies [c] were outdated. Notably it assumes
   that [transitive] is true which means we will not output a helper message. It will also
   prefix the lines with the style that has been applied. *)
let test
  number_of_immediate
  total_number_of_immediate
  number_of_transitive
  total_number_of_transitive
  =
  test_message
    ~transitive:true
    number_of_immediate
    total_number_of_immediate
    number_of_transitive
    total_number_of_transitive
;;

(* Testing different combinations of immediate and transitive dependencies. *)

(* We should always report an empty lock file as up to date. *)
let%expect_test "no packages" =
  test 0 0 0 0;
  [%expect {| [Success]  dune.lock is up to date. |}]
;;

let%expect_test "single immediate package" =
  test 0 1 0 0;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 1 1 0 0;
  [%expect {| [Warning]  1/1 packages in dune.lock are outdated. |}]
;;

let%expect_test "two immediate packages" =
  test 0 2 0 0;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 1 2 0 0;
  [%expect {| [Warning]  1/2 packages in dune.lock are outdated. |}];
  test 2 2 0 0;
  [%expect {| [Warning]  2/2 packages in dune.lock are outdated. |}]
;;

let%expect_test "three immediate packages" =
  test 0 3 0 0;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 1 3 0 0;
  [%expect {| [Warning]  1/3 packages in dune.lock are outdated. |}];
  test 2 3 0 0;
  [%expect {| [Warning]  2/3 packages in dune.lock are outdated. |}];
  test 3 3 0 0;
  [%expect {| [Warning]  3/3 packages in dune.lock are outdated. |}]
;;

(* This case will never happen as having at least a single transitive dependency means
   that there is at least one immediate dependency. The message is not the place to check
   this however, so for consistency we include what it would say. *)
let%expect_test "single transitive package" =
  test 0 0 0 1;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 0 0 1 1;
  [%expect {| [Warning]  1/1 packages in dune.lock are outdated. |}]
;;

(* Same as above. *)
let%expect_test "two transitives packages" =
  test 0 0 0 2;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 0 0 1 2;
  [%expect {| [Warning]  1/2 packages in dune.lock are outdated. |}];
  test 0 0 2 2;
  [%expect {| [Warning]  2/2 packages in dune.lock are outdated. |}]
;;

(* Same as above. *)
let%expect_test "three transitive packages" =
  test 0 0 0 3;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 0 0 1 3;
  [%expect {| [Warning]  1/3 packages in dune.lock are outdated. |}];
  test 0 0 2 3;
  [%expect {| [Warning]  2/3 packages in dune.lock are outdated. |}];
  test 0 0 3 3;
  [%expect {| [Warning]  3/3 packages in dune.lock are outdated. |}]
;;

(* A lockfile with two packages, one an immediate dependency and one a transitive
   dependency. Should have the appropriate message depending on which packages are
   outdated. Since we only show the *)
let%expect_test "one immediate and one transitive" =
  test 0 1 0 1;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 1 1 0 1;
  [%expect {| [Warning]  1/2 packages in dune.lock are outdated. |}];
  test 0 1 1 1;
  [%expect {| [Warning]  1/2 packages in dune.lock are outdated. |}];
  test 1 1 1 1;
  [%expect {| [Warning]  2/2 packages in dune.lock are outdated. |}]
;;

let%expect_test "one immediate and two transitive" =
  test 0 1 0 2;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 1 1 0 2;
  [%expect {| [Warning]  1/3 packages in dune.lock are outdated. |}];
  test 0 1 1 2;
  [%expect {| [Warning]  1/3 packages in dune.lock are outdated. |}];
  test 1 1 1 2;
  [%expect {| [Warning]  2/3 packages in dune.lock are outdated. |}];
  test 0 1 2 2;
  [%expect {| [Warning]  2/3 packages in dune.lock are outdated. |}];
  test 1 1 2 2;
  [%expect {| [Warning]  3/3 packages in dune.lock are outdated. |}]
;;

let%expect_test "two immediate and one transitive" =
  test 0 2 0 1;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 1 2 0 1;
  [%expect {| [Warning]  1/3 packages in dune.lock are outdated. |}];
  test 2 2 0 1;
  [%expect {| [Warning]  2/3 packages in dune.lock are outdated. |}];
  test 0 2 1 1;
  [%expect {| [Warning]  1/3 packages in dune.lock are outdated. |}];
  test 1 2 1 1;
  [%expect {| [Warning]  2/3 packages in dune.lock are outdated. |}];
  test 2 2 1 1;
  [%expect {| [Warning]  3/3 packages in dune.lock are outdated. |}]
;;

let%expect_test "two immediate and two transitive" =
  test 0 2 0 2;
  [%expect {| [Success]  dune.lock is up to date. |}];
  test 1 2 0 2;
  [%expect {| [Warning]  1/4 packages in dune.lock are outdated. |}];
  test 2 2 0 2;
  [%expect {| [Warning]  2/4 packages in dune.lock are outdated. |}];
  test 0 2 1 2;
  [%expect {| [Warning]  1/4 packages in dune.lock are outdated. |}];
  test 1 2 1 2;
  [%expect {| [Warning]  2/4 packages in dune.lock are outdated. |}];
  test 2 2 1 2;
  [%expect {| [Warning]  3/4 packages in dune.lock are outdated. |}];
  test 0 2 2 2;
  [%expect {| [Warning]  2/4 packages in dune.lock are outdated. |}];
  test 1 2 2 2;
  [%expect {| [Warning]  3/4 packages in dune.lock are outdated. |}];
  test 2 2 2 2;
  [%expect {| [Warning]  4/4 packages in dune.lock are outdated. |}]
;;

let%expect_test "some larger examples" =
  test 0 0 10 100;
  [%expect {| [Warning]  10/100 packages in dune.lock are outdated. |}];
  test 12 34 56 78;
  [%expect {| [Warning]  68/112 packages in dune.lock are outdated. |}]
;;

(* [test_entire_output a b c d] prints the message from before and also all the outdated
   packages the command will output. Unlike before we do not print style information. *)
let test_entire_output
  ~transitive
  number_of_immediate
  total_number_of_immediate
  number_of_transitive
  total_number_of_transitive
  =
  let results =
    dummy_results
      number_of_immediate
      total_number_of_immediate
      number_of_transitive
      total_number_of_transitive
  in
  let lock_dir_path = Stdune.Path.Source.of_string "dune.lock" in
  let message = Dune_pkg_outdated.For_tests.pp ~transitive ~lock_dir_path results in
  Console.print [ message ]
;;

(* We now test the entire output of the command to see how it will look. *)
let%expect_test "testing entire output" =
  test_entire_output ~transitive:false 2 3 2 3;
  [%expect
    {|
4/6 packages in dune.lock are outdated.
Showing immediate dependencies, use --transitive to see the rest.
- foo0 1.0.0 < 2.0.0
- foo1 1.0.0 < 2.0.0
  |}];
  test_entire_output ~transitive:true 2 3 2 3;
  [%expect
    {|
4/6 packages in dune.lock are outdated.
- foo0 1.0.0 < 2.0.0
- foo1 1.0.0 < 2.0.0
- bar0 1.0.0 < 2.0.0
- bar1 1.0.0 < 2.0.0
  |}]
;;

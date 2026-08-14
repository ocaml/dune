open Stdune

let a = Env.Var.of_string "A"
let b = Env.Var.of_string "B"

let%expect_test "environment variable names cannot contain NUL bytes" =
  (match Env.Var.of_string "A\000B" with
   | _ -> print_endline "accepted"
   | exception Code_error.E _ -> print_endline "rejected");
  [%expect {| rejected |}]
;;

let find_assignment env var =
  Env.to_unix env
  |> List.find_map ~f:(fun assignment ->
    match String.lsplit2 assignment ~on:'=' with
    | Some (key, _) when String.equal key var -> Some assignment
    | Some _ | None -> None)
  |> Option.value_exn
;;

let%expect_test "updating a binding preserves the others" =
  let env = Env.empty |> Env.add ~var:a ~value:"one" |> Env.add ~var:b ~value:"two" in
  let updated = Env.add env ~var:b ~value:"three" in
  print_endline (find_assignment updated "A");
  print_endline (find_assignment updated "B");
  [%expect
    {|
    A=one
    B=three |}]
;;

let%expect_test "rendering preserves the environment hash" =
  let env = Env.empty |> Env.add ~var:a ~value:"one" in
  let before = Env.hash env in
  ignore (Env.to_unix env);
  print_endline (Bool.to_string (Int.equal before (Env.hash env)));
  [%expect {| true |}]
;;

let%expect_test "environment values reject NUL bytes when added" =
  (match Env.add Env.empty ~var:a ~value:"value\000" with
   | exception Code_error.E _ -> print_endline "rejected"
   | _ -> print_endline "accepted");
  [%expect {| rejected |}]
;;

let%expect_test "setting an unchanged variable reuses the environment" =
  let env = Env.empty |> Env.add ~var:a ~value:"one" in
  let unchanged = Env.add env ~var:a ~value:"one" in
  let changed = Env.add env ~var:a ~value:"two" in
  print_endline (Bool.to_string (env == unchanged));
  print_endline (Bool.to_string (env == changed));
  [%expect
    {|
    true
    false |}]
;;

open Stdune

let a = Env.Var.of_string "A"
let b = Env.Var.of_string "B"

let find_assignment env var =
  Env.to_unix env
  |> List.find_map ~f:(fun assignment ->
    match String.lsplit2 assignment ~on:'=' with
    | Some (key, _) when String.equal key var -> Some assignment
    | Some _ | None -> None)
  |> Option.value_exn
;;

let%expect_test "unchanged assignments are shared between environments" =
  let env = Env.empty |> Env.add ~var:a ~value:"one" |> Env.add ~var:b ~value:"two" in
  let assignment = find_assignment env "A" in
  let updated = Env.add env ~var:b ~value:"three" in
  let updated_assignment = find_assignment updated "A" in
  print_endline (Bool.to_string (assignment == updated_assignment));
  [%expect {| true |}]
;;

let%expect_test "rendering preserves the environment hash" =
  let env = Env.empty |> Env.add ~var:a ~value:"one" in
  let before = Env.hash env in
  ignore (Env.to_unix env);
  print_endline (Bool.to_string (Int.equal before (Env.hash env)));
  [%expect {| true |}]
;;

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

let path_env dirs =
  let value = List.map dirs ~f:Path.to_absolute_filename |> Bin.encode_strings in
  Env.add Env.empty ~var:Env_path.var ~value
;;

let%expect_test "bulk PATH assembly preserves ordered prepending" =
  let first = Path.of_filename_relative_to_initial_cwd "first" in
  let second = Path.of_filename_relative_to_initial_cwd "second" in
  let old = Bin.encode_strings [ "old"; ""; "tail" ] in
  let cases =
    [ [], None
    ; [], Some ""
    ; [], Some old
    ; [ first ], None
    ; [ first ], Some ""
    ; [ first; second ], Some old
    ; [ first; second; first ], Some old
    ]
  in
  List.iter cases ~f:(fun (dirs, path) ->
    let env = Env.add Env.empty ~var:a ~value:"keep" in
    let env =
      match path with
      | None -> env
      | Some value -> Env.add env ~var:Env_path.var ~value
    in
    let expected =
      List.fold_right dirs ~init:env ~f:(fun dir env -> Env_path.cons env ~dir)
    in
    let actual = Env_path.extend_env_concat_path env (path_env dirs) in
    print_endline (Bool.to_string (Env.equal expected actual)));
  [%expect
    {|
    true
    true
    true
    true
    true
    true
    true |}]
;;

let%expect_test "bulk PATH allocation grows linearly" =
  let dir = Path.of_filename_relative_to_initial_cwd "directory" in
  let allocated dirs =
    let prefix = path_env dirs in
    Gc.full_major ();
    let before = Gc.allocated_bytes () in
    ignore (Sys.opaque_identity (Env_path.extend_env_concat_path Env.empty prefix));
    Gc.allocated_bytes () -. before
  in
  let small = allocated (List.init 1000 ~f:(fun _ -> dir)) in
  let large = allocated (List.init 2000 ~f:(fun _ -> dir)) in
  let linear =
    match Float.compare large (3. *. small) with
    | Lt -> true
    | Eq | Gt -> false
  in
  print_endline (Bool.to_string linear);
  [%expect {| true |}]
;;

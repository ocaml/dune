open Stdune
open Dune_rules

let () = Dune_tests_common.init ()

let expand args =
  let { Action_builder.With_targets.build; targets = _ } =
    Command.expand ~dir:Path.root args
  in
  let result, _deps =
    Fiber.run
      (Memo.run (Action_builder.evaluate_and_collect_deps build))
      ~iter:(fun () -> assert false)
  in
  Appendable_list.to_list result
;;

let%expect_test "nested argument groups preserve order" =
  let open Command.Args in
  let args =
    S
      [ A "a"
      ; S []
      ; S [ As [ "b"; "c" ]; S [ A "d"; S []; S [ As [ "e"; "f" ] ] ] ]
      ; A "g"
      ]
  in
  List.iter (expand args) ~f:(printfn "%S");
  [%expect
    {|
    "a"
    "b"
    "c"
    "d"
    "e"
    "f"
    "g"
    |}]
;;

let%expect_test "static dependencies preserve lazy sets and eager facts" =
  let module Dep = Dune_engine.Dep in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let legacy deps = Action_builder.dyn_memo_deps (Memo.return (deps, ())) in
  let env = Dep.env (Env.Var.of_string "STATIC_DEPS_TEST") in
  let missing =
    Dep.file
      (Path.build (Path.Build.relative Path.Build.root "default/unbuilt-static-dep"))
  in
  let deps = Dep.Set.of_list [ missing; env; Dep.universe ] in
  (* No build configuration is installed, and lazy evaluation must not try to
     build the missing target. *)
  let static = Action_builder.deps deps in
  print_endline "constructed without a build configuration";
  let (), actual = run (Action_builder.evaluate_and_collect_deps static) in
  let (), previous = run (Action_builder.evaluate_and_collect_deps (legacy deps)) in
  printfn
    "lazy dependencies kept: %b"
    (Dep.Set.equal actual deps && Dep.Set.equal actual previous);
  let check_facts deps expected =
    let (), actual =
      run (Action_builder.evaluate_and_collect_facts (Action_builder.deps deps))
    in
    let (), previous = run (Action_builder.evaluate_and_collect_facts (legacy deps)) in
    Dep.Facts.equal actual expected && Dep.Facts.equal actual previous
  in
  printfn "empty eager facts kept: %b" (check_facts Dep.Set.empty Dep.Facts.empty);
  let expected =
    Dep.Facts.union
      (Dep.Facts.singleton env Dep.Fact.nothing)
      (Dep.Facts.singleton Dep.universe Dep.Fact.nothing)
  in
  printfn
    "nonempty eager facts kept: %b"
    (check_facts (Dep.Set.of_list [ env; Dep.universe ]) expected);
  [%expect
    {|
    constructed without a build configuration
    lazy dependencies kept: true
    empty eager facts kept: true
    nonempty eager facts kept: true
    |}]
;;

let%expect_test "singleton list maps preserve evaluation and dependencies" =
  let module Dep = Dune_engine.Dep in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let first, second = ref 1, ref 2 in
  let deps = Dep.Set.singleton Dep.universe in
  let facts = Dep.Facts.singleton Dep.universe Dep.Fact.nothing in
  List.iter
    [ []; [ first ]; [ first; second; first ] ]
    ~f:(fun values ->
      let calls = ref 0 in
      let fact_calls = ref 0 in
      let mapped =
        Action_builder.List.map values ~f:(fun value ->
          incr calls;
          Action_builder.record value deps ~f:(fun _ ->
            incr fact_calls;
            Memo.return Dep.Fact.nothing))
      in
      assert (!calls = 0 && !fact_calls = 0);
      let lazy_values, lazy_deps =
        run (Action_builder.evaluate_and_collect_deps mapped)
      in
      assert (List.equal ( == ) lazy_values values);
      assert (!calls = List.length values && !fact_calls = 0);
      assert (Dep.Set.equal lazy_deps (if values = [] then Dep.Set.empty else deps));
      let eager_values, eager_facts =
        run (Action_builder.evaluate_and_collect_facts mapped)
      in
      assert (List.equal ( == ) eager_values values);
      assert (!calls = 2 * List.length values && !fact_calls = List.length values);
      assert (Dep.Facts.equal eager_facts (if values = [] then Dep.Facts.empty else facts)));
  let release = Fiber.Ivar.create () in
  let calls = ref 0 in
  let suspended =
    Action_builder.List.map [ first ] ~f:(fun value ->
      incr calls;
      let open Fiber.O in
      Action_builder.of_memo
        (Memo.of_reproducible_fiber
           (let+ () = Fiber.Ivar.read release in
            value)))
  in
  assert (!calls = 0);
  let released = ref false in
  let values, deps =
    Fiber.run
      (Memo.run (Action_builder.evaluate_and_collect_deps suspended))
      ~iter:(fun () ->
        assert (!calls = 1 && not !released);
        released := true;
        [ Fiber.Fill (release, ()) ])
  in
  assert (!released && !calls = 1);
  assert (List.equal ( == ) values [ first ] && Dep.Set.is_empty deps);
  let exception Failed in
  List.iter [ false; true ] ~f:(fun delayed ->
    let calls = ref 0 in
    let failing =
      Action_builder.List.map [ () ] ~f:(fun () ->
        incr calls;
        if delayed
        then Action_builder.of_memo (Memo.of_thunk (fun () -> raise Failed))
        else raise Failed)
    in
    assert (!calls = 0);
    let result =
      Fiber.run
        (Fiber.collect_errors (fun () ->
           Memo.run (Action_builder.evaluate_and_collect_deps failing)))
        ~iter:(fun () -> failwith "unexpected suspension")
    in
    assert (!calls = 1);
    assert (
      match result with
      | Error [ { Exn_with_backtrace.exn = Failed; _ } ] -> true
      | Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
        (match Memo.Error.get error with
         | Failed -> true
         | _ -> false)
      | _ -> false));
  print_endline "singleton map invariants hold";
  [%expect {| singleton map invariants hold |}]
;;

let%expect_test "literal dynamic arguments match the general dynamic oracle" =
  let module Dep = Dune_engine.Dep in
  let open Command.Args in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let dir = Path.Build.relative Path.Build.root "default/literal-dynamic-args" in
  let target name = Path.Build.relative dir name in
  let targets_are targets name =
    match Targets.validate targets with
    | Valid targets ->
      Path.Build.equal targets.root dir
      && Filename.Set.equal
           targets.files
           (Filename.Set.singleton (Filename.of_string_exn name))
      && Filename.Set.is_empty targets.dirs
    | _ -> false
  in
  let env = Dep.env (Env.Var.of_string "LITERAL_DYNAMIC_ARGS") in
  let deps = Dep.Set.of_list [ env; Dep.universe ] in
  let facts =
    Dep.Facts.union
      (Dep.Facts.singleton env Dep.Fact.nothing)
      (Dep.Facts.singleton Dep.universe Dep.Fact.nothing)
  in
  List.iter
    [ []; [ "one" ]; [ "two"; ""; "words with spaces" ] ]
    ~f:(fun strings ->
      let calls = ref 0 in
      let args =
        Action_builder.map (Action_builder.deps deps) ~f:(fun () ->
          incr calls;
          strings)
      in
      let dynamic = Command.Args.dyn args in
      let first =
        Command.expand ~dir:(Path.build dir) (S [ Target (target "first"); dynamic ])
      in
      let second =
        Command.expand ~dir:Path.root (S [ Hidden_targets [ target "second" ]; dynamic ])
      in
      let legacy =
        Command.expand_no_targets
          ~dir:Path.root
          (Dyn (Action_builder.map args ~f:(fun args -> As args)))
      in
      let delayed = !calls = 0 in
      let first_args, first_deps =
        run (Action_builder.evaluate_and_collect_deps first.build)
      in
      let old_args, old_deps = run (Action_builder.evaluate_and_collect_deps legacy) in
      let second_args, second_facts =
        run (Action_builder.evaluate_and_collect_facts second.build)
      in
      let old_eager_args, old_facts =
        run (Action_builder.evaluate_and_collect_facts legacy)
      in
      let same_args args =
        List.equal String.equal (Appendable_list.to_list args) strings
      in
      printfn
        "%d literals: delayed %b; lazy %b; eager %b; targets %b; evaluations %d"
        (List.length strings)
        delayed
        (List.equal String.equal (Appendable_list.to_list first_args) ("first" :: strings)
         && same_args old_args
         && Dep.Set.equal first_deps deps
         && Dep.Set.equal first_deps old_deps)
        (same_args second_args
         && same_args old_eager_args
         && Dep.Facts.equal second_facts facts
         && Dep.Facts.equal second_facts old_facts)
        (targets_are first.targets "first" && targets_are second.targets "second")
        !calls);
  [%expect
    {|
    0 literals: delayed true; lazy true; eager true; targets true; evaluations 4
    1 literals: delayed true; lazy true; eager true; targets true; evaluations 4
    3 literals: delayed true; lazy true; eager true; targets true; evaluations 4
    |}]
;;

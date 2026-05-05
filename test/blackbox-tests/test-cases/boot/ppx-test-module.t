Testing that the bootstrap preprocessor strips let%test_module blocks.

  $ init_bootstrap

  $ mkdir -p src/a

  $ cat > src/a/a.ml <<EOF
  > module Root = Root
  > let x = 42
  > let%test_module "tests" =
  >   (module struct
  >     let _ = ")"
  >     let _ = ')'
  >     let _ = '\''
  >     let _ = fun (x : 'a) -> x
  >     let _ = "\" ) ;; [%%endif]"
  >     let _ = {|) ;; [%%else]|}
  >     let _ = {boot|[%%endif]|boot}
  >     let _ = "[%%endif]"
  >     let _ = 0 (* outer (* ) ;; [%%else] *) [%%endif] *)
  >     let%expect_test "inner test" =
  >       print_int x;
  >       [%expect {| 42 |}]
  >     ;;
  >     let helper () = x + 1
  >     let%expect_test "another" =
  >       print_int (helper ());
  >       [%expect {| 43 |}]
  >     ;;
  >   end)
  > ;;
  > let () = Printf.printf "x = %d\n" x
  > EOF

  $ make_module src/a/root.ml

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a))
  > EOF

  $ create_dune a <<EOF
  > let () = Printf.printf "Hello, x = %d" A.x
  > EOF
  ocamllex -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  x = 42
  Hello, x = 42

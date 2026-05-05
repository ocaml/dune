Testing that the bootstrap preprocessor strips let%expect_test blocks.

  $ init_bootstrap

  $ mkdir -p src/a

  $ cat > src/a/a.ml <<EOF
  > module Root = Root
  > let x = 42
  > let%expect_test "this should be stripped" =
  >   let _ = ";;" in
  >   let _ = "\" ;; [%%else]" in
  >   let _ = '\'' in
  >   let _ = fun (x : 'a) -> x in
  >   let _ = {|;; [%%endif]|} in
  >   let _ = {boot|[%%else]|boot} in
  >   let _ = "[%%else]" in
  >   let _ = 0 (* outer (* ;; [%%endif] *) [%%else] *) in
  >   print_int x;
  >   [%expect {| 42 |}]
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

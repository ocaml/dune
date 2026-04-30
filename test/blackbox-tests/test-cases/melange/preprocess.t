Test (preprocess) field on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (modules main)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (preprocess
  >   (action
  >    (run cat %{input-file}))))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @mel
  $ node _build/default/output/main.js
  hello

Reason sources with PPX preprocessing should pass the OCaml AST produced by the
Reason dialect preprocessor to the PPX driver.

  $ mkdir -p ppx reason-pps

  $ cat > ppx/dune <<'EOF'
  > (library
  >  (name reason_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF

  $ cat > ppx/reason_ppx.ml <<'EOF'
  > let () =
  >   Array.iter
  >     (fun arg ->
  >       if Filename.check_suffix arg ".re" || Filename.check_suffix arg ".rei"
  >       then (
  >         Printf.eprintf "ppx saw Reason source: %s\n" arg;
  >         exit 1))
  >     Sys.argv
  > ;;
  > let () = Ppxlib.Driver.register_transformation "reason_ppx"
  > EOF

  $ cat > reason-pps/dune <<'EOF'
  > (melange.emit
  >  (target out)
  >  (modules main)
  >  (alias mel)
  >  (emit_stdlib false)
  >  (preprocess (pps reason_ppx)))
  > EOF

  $ cat > reason-pps/main.re <<'EOF'
  > let add x y = x + y
  > let result = add 1 2
  > EOF

  $ dune build @reason-pps/mel

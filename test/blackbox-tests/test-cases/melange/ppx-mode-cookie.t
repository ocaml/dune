The ppx driver receives a "mode" cookie that names the compilation mode: "ocaml"
for OCaml builds and "melange" for Melange builds.

  $ export DUNE_TRACE="process"
  $ make_melange_project 3.26 1.0

  $ cat > dune <<EOF
  > (library
  >  (name driver)
  >  (modules driver)
  >  (ppx.driver (main Driver.main)))
  > (library
  >  (name the_ppx)
  >  (kind ppx_rewriter)
  >  (modules ())
  >  (libraries driver))
  > (library
  >  (name both)
  >  (modules both)
  >  (modes :standard melange)
  >  (preprocess (pps the_ppx)))
  > (melange.emit
  >  (target out)
  >  (alias mel)
  >  (modules main)
  >  (emit_stdlib false)
  >  (preprocess (pps the_ppx)))
  > EOF

  $ cat > driver.ml <<EOF
  > let main () =
  >   let out = ref "" in
  >   let args =
  >     [ ("-o", Arg.Set_string out, "")
  >     ; ("--impl", Arg.Set_string (ref ""), "")
  >     ; ("--as-ppx", Arg.Set (ref false), "")
  >     ; ("--cookie", Arg.String (fun _ -> ()), "")
  >     ; ("-loc-filename", Arg.String (fun _ -> ()), "")
  >     ]
  >   in
  >   Arg.parse (Arg.align args) (fun _ -> ()) "";
  >   let oc = open_out !out in
  >   close_out oc
  > EOF

  $ touch both.ml main.ml

A library built for both modes is preprocessed once per mode, and each run gets
the mode it is preprocessed for:

  $ dune build ./both.cma
  $ dune trace cat | jq_dune -c '
  >   processes
  > | select(.args.prog | endswith("ppx.exe"))
  > | .args.process_args
  > | map(select(startswith("mode=") or endswith(".pp.ml")))
  > '
  ["mode=\"ocaml\"","both.pp.ml"]

  $ dune build ./.both.objs/melange/both.cmj
  $ dune trace cat | jq_dune -c '
  >   processes
  > | select(.args.prog | endswith("ppx.exe"))
  > | .args.process_args
  > | map(select(startswith("mode=") or endswith(".pp.ml")))
  > '
  ["mode=\"melange\"",".melange_src/both.pp.ml"]

A melange.emit stanza is preprocessed for Melange:

  $ dune build @mel
  $ dune trace cat | jq_dune -c '
  >   processes
  > | select(.args.prog | endswith("ppx.exe"))
  > | .args.process_args
  > | map(select(startswith("mode=") or endswith(".pp.ml")))
  > '
  ["mode=\"melange\"",".melange_src/main.pp.ml"]

With an older dune lang version, the cookie is not passed:

  $ make_melange_project 3.25 1.0
  $ dune build ./both.cma
  $ dune trace cat | jq_dune -c '
  >   processes
  > | select(.args.prog | endswith("ppx.exe"))
  > | .args.process_args
  > | map(select(startswith("mode=") or endswith(".pp.ml")))
  > '
  ["both.pp.ml"]

A ppx that declares its own "mode" cookie with another value conflicts with the
one set by dune:

  $ make_melange_project 3.26 1.0
  $ cat >> dune <<EOF
  > (library
  >  (name clash_ppx)
  >  (kind (ppx_rewriter (cookies (mode "custom"))))
  >  (modules ())
  >  (libraries driver))
  > (library
  >  (name clash)
  >  (modules clash)
  >  (preprocess (pps clash_ppx)))
  > EOF
  $ touch clash.ml
  $ dune build ./clash.cma
  File "dune", line 29, characters 13-28:
  29 |  (preprocess (pps clash_ppx)))
                    ^^^^^^^^^^^^^^^
  Error: dune and clash_ppx have inconsistent requests for cookie "mode"; dune
  requests "ocaml" and clash_ppx requests "custom"
  [1]

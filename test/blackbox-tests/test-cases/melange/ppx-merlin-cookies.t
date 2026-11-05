Library-declared string cookies are preserved in Merlin's PPX commands for
OCaml and Melange, with both ordinary and staged preprocessing.

  $ make_melange_project 3.25 1.0
  $ mkdir ppx ordinary staged
  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name cookie_ppx)
  >  (kind (ppx_rewriter (cookies (message "hello world"))))
  >  (modules)
  >  (libraries ppxlib))
  > EOF
  $ cat >ordinary/dune <<'EOF'
  > (library
  >  (name ordinary)
  >  (modes :standard melange)
  >  (preprocess (pps cookie_ppx)))
  > EOF
  $ cat >staged/dune <<'EOF'
  > (library
  >  (name staged)
  >  (modes :standard melange)
  >  (preprocess (staged_pps cookie_ppx)))
  > EOF
  $ touch ordinary/example.ml staged/example.ml
  $ dune build ordinary/.merlin-conf/lib-ordinary \
  >   staged/.merlin-conf/lib-staged
  $ merlin_ppx () {
  >   dune ocaml merlin dump-config --format=json "$PWD/$1" | jq -r '
  >     .[] | select(.source_path | endswith("/example.ml"))
  >     | select(any(.config[];
  >         .[0] == "B" and (.[1] | endswith($obj_dir))))
  >     | .config[] | select(.[0] == "FLG" and .[1][0] == "-ppx")
  >     | .[1][1]' --arg obj_dir ".$1.objs/$2" | sort -u | censor
  > }
  $ merlin_ppx ordinary byte
  $PWD/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name="ordinary"' --cookie 'message="hello world"'
  $ merlin_ppx ordinary melange
  $PWD/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name="ordinary"' --cookie 'message="hello world"'
  $ merlin_ppx staged byte
  $PWD/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name="staged"' --cookie 'message="hello world"'
  $ merlin_ppx staged melange
  $PWD/_build/default/.ppx/$DIGEST/ppx.exe --as-ppx --cookie 'library-name="staged"' --cookie 'message="hello world"'

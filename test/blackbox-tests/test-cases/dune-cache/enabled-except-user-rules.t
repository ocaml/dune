Test that enabled-except-user-rules allows OCaml compilation to be cached.

  $ export XDG_RUNTIME_DIR=$(dune_cmd native-path $PWD/.xdg-runtime)
  $ export XDG_CACHE_HOME=$(dune_cmd native-path $PWD/.xdg-cache)
  $ export DUNE_CACHE_ROOT=$PWD/.cache

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > config <<EOF
  > (lang dune 3.22)
  > (cache enabled-except-user-rules)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name foo))
  > EOF

  $ cat > foo.ml <<EOF
  > let x = 42
  > EOF

  $ export DUNE_TRACE=cache
  $ dune build @all --config-file config

OCaml compilation should be cached. Verify no "can't go in shared cache" misses
for foo compilation artifacts:

  $ dune trace cat | jq -c '
  >   select(.cat == "cache")
  >   | select(.name == "miss")
  >   | select(.args.reason == "can'"'"'t go in shared cache")
  >   | .args.head
  >   | select(test("foo\\.(cmi|cmo|cmx|cma|cmxa|cmxs|a)$"))
  > ' | sort
  "_build/default/.foo.objs/byte/foo.cmi"
  "_build/default/.foo.objs/native/foo.cmx"
  "_build/default/foo.a"
  "_build/default/foo.cma"
  "_build/default/foo.cmxs"


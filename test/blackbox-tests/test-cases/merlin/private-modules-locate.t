A library with private modules keeps its public CMIs separate from its other
artifacts. Merlin needs the byte object directory as an annotation-only CMT
path to locate definitions from the library (#4892).

  $ mkdir lib exe

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > EOF
  $ cat > lib/dune <<'EOF'
  > (library
  >  (name dep)
  >  (private_modules private))
  > EOF
  $ cat > lib/private.ml <<'EOF'
  > let answer = 42
  > EOF
  $ cat > lib/api.ml <<'EOF'
  > let answer = Private.answer
  > EOF
  $ cat > exe/dune <<'EOF'
  > (executable
  >  (name main)
  >  (libraries dep))
  > EOF
  $ cat > exe/main.ml <<'EOF'
  > let answer = Dep.Api.answer
  > EOF

The public module's annotation is in the byte object directory and not in the
public CMI directory.

  $ dune build
  $ ls _build/default/lib/.dep.objs/byte/dep__Api.cmt
  _build/default/lib/.dep.objs/byte/dep__Api.cmt
  $ test ! -e _build/default/lib/.dep.objs/public_cmi/dep__Api.cmt

Show the build and annotation paths Dune gives Merlin for the dependency.

  $ dune ocaml merlin dump-config --format=json "$PWD/exe" \
  > | jq_dune -r '
  >   [.[]
  >    | select(.module_name == "Main")
  >    | .config[]
  >    | select(.[0] == "B" or .[0] == "CMT")
  >    | select(.[1] | contains("/lib/.dep.objs/"))
  >    | "\(.[0]) \(.[1])"]
  >   | unique[]'
  B $TESTCASE_ROOT/_build/default/lib/.dep.objs/public_cmi
  CMT $TESTCASE_ROOT/_build/default/lib/.dep.objs/byte

Compilers with hidden includes can type a public alias of a private module.
BUG (#16531): replaying Merlin's B/BH paths fails because the hidden CMI path
is missing. Adding -H restores typing without exposing the private module.

  $ if ocamlc -H . -version > /dev/null 2>&1; then
  >   printf '\nmodule P = Private\n' >> lib/api.ml &&
  >   printf 'let answer = Dep.Api.P.answer\n' > exe/main.ml &&
  >   dune build &&
  >   dune ocaml merlin dump-config --format=json "$PWD/exe" > merlin.json &&
  >   jq -r '
  >     [.[]
  >      | select(.module_name == "Main")
  >      | .config[]
  >      | select(.[0] == "B" or .[0] == "BH")
  >      | [(if .[0] == "B" then "-I" else "-H" end), .[1]]]
  >     | unique[] | .[]' merlin.json > includes &&
  >   ! ocamlc -color never -args includes -c exe/main.ml > errors 2>&1 &&
  >   ocamlc -color never -args includes \
  >     -H "$PWD/_build/default/lib/.dep.objs/byte" -c exe/main.ml &&
  >   printf 'let answer = Dep__Private.answer\n' > exe/main.ml &&
  >   ! ocamlc -color never -args includes \
  >     -H "$PWD/_build/default/lib/.dep.objs/byte" \
  >     -c exe/main.ml > errors 2>&1
  > fi

A hidden transitive dependency has the same missing hidden CMI path. Adding
that path restores alias typing without exposing either the dependency or its
private module.

  $ if ocamlc -H . -version > /dev/null 2>&1; then
  >   printf '\n(implicit_transitive_deps false)\n' >> dune-project &&
  >   mkdir bridge &&
  >   printf '(library (name bridge) (libraries dep))\n' > bridge/dune &&
  >   printf 'module P = Dep.Api.P\n' > bridge/bridge.ml &&
  >   printf '(executable (name main) (libraries bridge))\n' > exe/dune &&
  >   printf 'let answer = Bridge.P.answer\n' > exe/main.ml &&
  >   dune build &&
  >   dune ocaml merlin dump-config --format=json "$PWD/exe" > merlin.json &&
  >   jq -e '
  >     [.[]
  >      | select(.module_name == "Main")
  >      | .config[]
  >      | select(.[0] == "CMT")
  >      | .[1] | endswith("/lib/.dep.objs/byte")]
  >     | any' merlin.json > /dev/null &&
  >   jq -r '
  >     [.[]
  >      | select(.module_name == "Main")
  >      | .config[]
  >      | select(.[0] == "B" or .[0] == "BH")
  >      | [(if .[0] == "B" then "-I" else "-H" end), .[1]]]
  >     | unique[] | .[]' merlin.json > includes &&
  >   ! ocamlc -color never -args includes -c exe/main.ml > errors 2>&1 &&
  >   ocamlc -color never -args includes \
  >     -H "$PWD/_build/default/lib/.dep.objs/byte" -c exe/main.ml &&
  >   printf 'let answer = Dep.Api.answer\n' > exe/main.ml &&
  >   ! ocamlc -color never -args includes \
  >     -H "$PWD/_build/default/lib/.dep.objs/byte" -c exe/main.ml > errors 2>&1 &&
  >   printf 'let answer = Dep__Private.answer\n' > exe/main.ml &&
  >   ! ocamlc -color never -args includes \
  >     -H "$PWD/_build/default/lib/.dep.objs/byte" -c exe/main.ml > errors 2>&1
  > fi

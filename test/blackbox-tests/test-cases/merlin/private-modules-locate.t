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

  $ if ocamlc -help | grep -q ' -H '; then
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
  >   grep -q 'alias.*Dep__Private.*missing' errors &&
  >   ocamlc -color never -args includes \
  >     -H "$PWD/_build/default/lib/.dep.objs/byte" -c exe/main.ml &&
  >   printf 'let answer = Dep__Private.answer\n' > exe/main.ml &&
  >   ! ocamlc -color never -args includes \
  >     -H "$PWD/_build/default/lib/.dep.objs/byte" \
  >     -c exe/main.ml > errors 2>&1 &&
  >   grep -q 'Unbound module Dep__Private' errors
  > fi

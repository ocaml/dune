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

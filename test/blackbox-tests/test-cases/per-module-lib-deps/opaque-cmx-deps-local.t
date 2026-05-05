A consumer's [.cmx] compilation rule depends on a local library's
[.cmx] under the release profile (opaque=false), but only on the
[.cmi] under the dev profile (opaque=true). External libraries
behave differently; see [opaque-cmx-deps-external.t].

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ mkdir local_dep
  $ cat > local_dep/dune <<EOF
  > (library (name local_dep))
  > EOF
  $ cat > local_dep/local_dep.ml <<EOF
  > let v = 42
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries local_dep))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_int Local_dep.v
  > EOF

--- Release profile (opaque=false): both .cmi and .cmx globs ---

  $ cat > dune-workspace <<EOF
  > (lang dune 3.23)
  > (profile release)
  > EOF

  $ dune build ./main.exe
  $ dune rules --root . --format=json --deps _build/default/.main.eobjs/native/dune__exe__Main.cmx |
  > jq -r 'include "dune"; .[] | depsGlobPredicates' | sort -u
  *.cmi
  *.cmx

--- Dev profile (opaque=true): only .cmi glob ---

  $ cat > dune-workspace <<EOF
  > (lang dune 3.23)
  > (profile dev)
  > EOF

  $ dune build ./main.exe
  $ dune rules --root . --format=json --deps _build/default/.main.eobjs/native/dune__exe__Main.cmx |
  > jq -r 'include "dune"; .[] | depsGlobPredicates' | sort -u
  *.cmi

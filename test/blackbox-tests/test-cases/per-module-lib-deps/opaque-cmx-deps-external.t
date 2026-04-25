A consumer's [.cmx] compilation rule depends on an external library's
[.cmx] under both the release and dev profiles. Counterpart to
[opaque-cmx-deps-local.t], which shows the dev-profile behaviour that
omits the [.cmx] from the dep set for *local* libraries. The [unix]
stdlib library, resolved through findlib, plays the role of "external".

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries unix))
  > EOF
  $ cat > main.ml <<EOF
  > let () = ignore (Unix.gettimeofday ())
  > EOF

--- Release profile (opaque=false): both .cmi and .cmx globs ---

  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (profile release)
  > EOF

  $ dune build ./main.exe
  $ dune rules --root . --format=json --deps _build/default/.main.eobjs/native/dune__exe__Main.cmx |
  > jq -r 'include "dune"; .[] | depsGlobPredicates' | sort -u
  *.cmi
  *.cmx

--- Dev profile (opaque=true): both .cmi and .cmx globs (unchanged for external libs) ---

  $ cat > dune-workspace <<EOF
  > (lang dune 3.0)
  > (profile dev)
  > EOF

  $ dune build ./main.exe
  $ dune rules --root . --format=json --deps _build/default/.main.eobjs/native/dune__exe__Main.cmx |
  > jq -r 'include "dune"; .[] | depsGlobPredicates' | sort -u
  *.cmi
  *.cmx

Verify that the [groups_for_cm_kind] optimisation in [lib_file_deps.ml]
is gated on [Lib.is_local lib]: a consumer's [.cmx] rule depends on an
external library's [.cmx] regardless of profile. Counterpart to
[opaque-cmx-deps-local.t]. The [unix] stdlib library, resolved through
findlib, plays the role of "external".

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

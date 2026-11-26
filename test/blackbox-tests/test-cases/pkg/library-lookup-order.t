Which opaque locked packages a library lookup builds. A package built by its
recipe only reveals its libraries once it is installed, so looking in one
means building it. The lookup tries the package named like the library, then
the compiler's own libraries, and only then the other packages.

  $ make_lockdir
  $ (make_dune_project 3.25)
  $ lib() {
  >   cat <<EOF
  > (version 1.0)
  > (build
  >  (progn
  >   (run echo built $1)
  >   (write-file $2.ml "let v = \"$2\"")
  >   (run ocamlc -a -o $2.cma $2.ml)
  >   (run ocamlopt -a -o $2.cmxa $2.ml)
  >   (write-file META "archive(byte) = \"$2.cma\"\narchive(native) = \"$2.cmxa\"\n")
  >   (write-file $1.install "lib: [ \"META\" \"$2.cmi\" \"$2.cma\" \"$2.cmx\" \"$2.cmxa\" \"$2.a\" ]")))
  > EOF
  > }

[first] installs a library of its own name:

  $ lib first first | make_lockpkg first

[second] installs a library under another name, [mismatch]:

  $ make_lockpkg second <<'EOF'
  > (version 1.0)
  > (build
  >  (progn
  >   (run echo built second)
  >   (run mkdir mismatch)
  >   (write-file mismatch.ml "let v = \"mismatch\"")
  >   (run ocamlc -a -o mismatch.cma mismatch.ml)
  >   (run ocamlopt -a -o mismatch.cmxa mismatch.ml)
  >   (write-file META "archive(byte) = \"mismatch.cma\"\narchive(native) = \"mismatch.cmxa\"\n")
  >   (write-file second.install "lib_root: [ \"META\" {\"mismatch/META\"} \"mismatch.cmi\" {\"mismatch/mismatch.cmi\"} \"mismatch.cma\" {\"mismatch/mismatch.cma\"} \"mismatch.cmx\" {\"mismatch/mismatch.cmx\"} \"mismatch.cmxa\" {\"mismatch/mismatch.cmxa\"} \"mismatch.a\" {\"mismatch/mismatch.a\"} ]")))
  > EOF

[other] installs no library:

  $ make_lockpkg other <<'EOF'
  > (version 1.0)
  > (build (run echo built other))
  > EOF

  $ try() {
  >   rm -rf _build
  >   echo "(executable (name main) (libraries $1))" > dune
  >   dune build ./main.exe 2>&1 | sort
  > }

A library of the compiler:

  $ echo 'let _ = Unix.getpid' > main.ml
  $ try unix

A library named after its package:

  $ echo 'let _ = First.v' > main.ml
  $ try first
  built first

A library whose name differs from its package needs the scan:

  $ echo 'let _ = Mismatch.v' > main.ml
  $ try mismatch
  built first
  built other
  built second

So does a library that nothing provides:

  $ rm main.ml
  $ touch main.with.ml main.without.ml
  $ try '(select main.ml from (absent -> main.with.ml) (-> main.without.ml))'
  built first
  built other
  built second

A package-owned workspace requester does not build undeclared opaque providers,
whether the library is named like the provider or requires a fallback scan.

  $ app() {
  >   printf '(lang dune 3.25)\n(package (name app) (dir .) (depends %s))\n' "$*" \
  >     > dune-project; }
  $ app
  $ echo 'let () = print_endline "selected"' > main.with.ml
  $ echo 'let () = print_endline "fallback"' > main.without.ml
  $ try '(select main.ml from (first -> main.with.ml) (-> main.without.ml))'
  $ _build/default/main.exe
  fallback
  $ test ! -e _build/default/.pkg/first/_install/cookie
  $ try '(select main.ml from (mismatch -> main.with.ml) (-> main.without.ml))'
  $ _build/default/main.exe
  fallback
  $ test ! -e _build/default/.pkg/first/_install/cookie
  $ test ! -e _build/default/.pkg/other/_install/cookie
  $ test ! -e _build/default/.pkg/second/_install/cookie

An absolute findlib configuration must not provide a second route around the
requester's opaque eligibility, either cold or after explicit installation.

  $ touch findlib.conf
  $ export OCAMLFIND_CONF="$PWD/findlib.conf"
  $ try '(select main.ml from (first -> main.with.ml) (-> main.without.ml))'
  $ _build/default/main.exe
  fallback
  $ test ! -e _build/default/.pkg/first/_install/cookie
  $ try '(select main.ml from (mismatch -> main.with.ml) (-> main.without.ml))'
  $ _build/default/main.exe
  fallback
  $ test ! -e _build/default/.pkg/other/_install/cookie
  $ build_pkg first > warm-first.log 2>&1
  $ build_pkg second > warm-second.log 2>&1
  $ test -f _build/default/.pkg/first/_install/cookie
  $ test -f _build/default/.pkg/second/_install/cookie
  $ echo '(executable (name main) (libraries (select main.ml from (first -> main.with.ml) (-> main.without.ml))))' > dune
  $ dune build main.exe
  $ _build/default/main.exe
  fallback
  $ echo '(executable (name main) (libraries (select main.ml from (mismatch -> main.with.ml) (-> main.without.ml))))' > dune
  $ dune build main.exe
  $ _build/default/main.exe
  fallback

Compiler libraries remain available with the same findlib configuration.

  $ echo 'let _ = Unix.getpid' > main.ml
  $ try unix
  $ rm main.ml
  $ unset OCAMLFIND_CONF

A declared transitive dependency still permits the differently named provider.
The unrelated packages need not be built to discover it.

  $ app bridge
  $ make_lockpkg bridge <<'EOF'
  > (version 1.0)
  > (depends second)
  > (build (run echo built bridge))
  > EOF
  $ try '(select main.ml from (mismatch -> main.with.ml) (-> main.without.ml))'
  built bridge
  built second
  $ _build/default/main.exe
  selected
  $ test ! -e _build/default/.pkg/first/_install/cookie
  $ test ! -e _build/default/.pkg/other/_install/cookie

An opaque library's requirements use its provider's scope, not the consumer's.
The consumer declares both first and second, but first does not declare second.

  $ app first second
  $ lib first first |
  >   dune_cmd subst 'archive\(byte\)' 'requires = \"mismatch\"\narchive(byte)' |
  >   make_lockpkg first
  $ echo '(executable (name main) (libraries first))' > dune
  $ echo 'let () = print_endline First.v' > main.ml
  $ rm -rf _build
  $ dune build main.exe > provider.log 2>&1
  [1]
  $ grep '^Error: Library' provider.log
  Error: Library "mismatch" not found.
  $ test ! -e _build/default/main.exe
  $ test ! -e _build/default/.pkg/second/_install/cookie

Declaring the dependency on its provider repairs the lookup.

  $ cp dune.lock/first.pkg first.pkg
  $ { echo '(depends second)'; cat first.pkg; } > dune.lock/first.pkg
  $ dune build main.exe 2>&1 | sort
  built first
  built second
  $ _build/default/main.exe
  first

Two eligible opaque packages exporting the same differently named library still
report the duplicate provider, rather than selecting one by lookup order.

  $ dune_cmd subst second third < dune.lock/second.pkg > dune.lock/third.pkg
  $ app second third
  $ echo '(executable (name main) (libraries mismatch))' > dune
  $ echo 'let () = print_endline Mismatch.v' > main.ml
  $ dune build main.exe > duplicate.log 2>&1
  [1]
  $ cat duplicate.log
  built third
  File "_build/default/.pkg/third/_install/lib/mismatch/META", line 1, characters 0-0:
  Error: Library with name "mismatch" is already defined in
  _build/default/.pkg/second/_install/lib/mismatch/META:1. Either change one of
  the names, or enable them conditionally using the 'enabled_if' field.

An opaque META requirement can name an undeclared native provider. The native
public library has ordinary Dune visibility even in the opaque provider's own
scope; it is not borrowed from the consumer's declarations.

  $ mkdir _native
  $ (cd _native && make_dune_project_with_package 3.25 native)
  $ echo '(library (public_name native))' > _native/dune
  $ echo 'let value = ()' > _native/native.ml
  $ make_dune_source_lockpkg native native "$PWD/_native"
  $ dune_cmd subst 'archive\(byte\)' 'requires = \"native\"\narchive(byte)' < dune.lock/second.pkg > second.pkg
  $ mv second.pkg dune.lock/second.pkg
  $ app second
  $ dune build main.exe
  built second
  $ grep '^requires' _build/default/.pkg/second/_install/lib/mismatch/META
  requires = "native"
  $ _build/default/main.exe
  mismatch
  $ test -f _build/default/.pkg/native/native.cmxa

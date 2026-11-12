The native package scalar supplies both a library and a prepared recipe for the
opaque package geometry. Geometry installs the libraries vector and matrix.
A package-owned native client consumes them through ordinary library lookup.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name scalar) (dir scalar))
  > (package (name geometry) (dir geometry) (depends scalar) (allow_empty))
  > (package (name client) (dir client) (depends geometry))
  > (package (name downstream) (dir downstream) (depends geometry) (allow_empty))
  > (package (name unrelated) (dir unrelated) (allow_empty))
  > EOF
  $ mkdir scalar geometry client downstream unrelated
  $ for package in downstream unrelated; do
  >   cat > "$package/dune" <<EOF
  > (opam
  >  (package $package)
  >  (dir src)
  >  (version 1.0)
  >  (build (run sh -c "echo $package >&2; exit 1")))
  > EOF
  > done
  $ echo 'let value = 40' > scalar/scalar.ml
  $ cat > scalar/recipe.sh <<'EOF'
  > #!/bin/sh
  > set -eu
  > printf 'let () = Printf.printf "%%d" Scalar.value\n' > scalar_value.ml
  > ocamlc -I "$1" scalar.cma scalar_value.ml -o scalar_value.exe
  > scalar_value=$(./scalar_value.exe)
  > printf 'let value = %s + 2\n' "$scalar_value" > vector.ml
  > ocamlc -c vector.ml
  > ocamlc -a vector.cmo -o vector.cma
  > printf 'let value = Vector.value + 1\n' > matrix.ml
  > ocamlc -c matrix.ml
  > ocamlc -a matrix.cmo -o matrix.cma
  > printf 'requires = "scalar"\narchive(byte) = "vector.cma"\n' > META.vector
  > printf 'requires = "vector"\narchive(byte) = "matrix.cma"\n' > META.matrix
  > printf '#!/bin/sh\necho geometry-tool\n' > geometry-tool
  > chmod +x geometry-tool
  > cat > geometry.install <<'INSTALL'
  > bin: [ "geometry-tool" ]
  > lib_root: [
  >   "META.vector" {"vector/META"}
  >   "vector.cmi" {"vector/vector.cmi"}
  >   "vector.cma" {"vector/vector.cma"}
  >   "META.matrix" {"matrix/META"}
  >   "matrix.cmi" {"matrix/matrix.cmi"}
  >   "matrix.cma" {"matrix/matrix.cma"}
  > ]
  > INSTALL
  > EOF
  $ cat > scalar/dune <<'EOF'
  > (library (public_name scalar))
  > (install (package scalar) (section share) (files recipe.sh))
  > EOF
  $ cat > geometry/dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir src)
  >  (version 1.0)
  >  (files (%{pkg:scalar:share:recipe.sh} recipe.sh))
  >  (build (run sh ./recipe.sh %{pkg:scalar:lib})))
  > EOF
  $ echo 'let value = Vector.value' > client/client.ml
  $ cat > client/dune <<'EOF'
  > (library (public_name client) (modes byte) (libraries vector))
  > EOF
  $ echo 'let () = Printf.printf "%d\n" Client.value' > main.ml
  $ cat > dune <<'EOF'
  > (executable (name main) (modes byte) (libraries client))
  > (rule
  >  (target installed)
  >  (deps (package geometry))
  >  (action
  >   (with-stdout-to %{target} (cat %{pkg:geometry:lib_root:vector/META}))))
  > EOF
  $ dune build main.bc
  $ dune build installed
  $ dune exec ./main.bc
  42
  $ cat _build/default/installed
  requires = "scalar"
  archive(byte) = "vector.cma"

The native client's required-library closure adds geometry to its explicit
package layout and makes geometry's executable available to binary lookup.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target from-client)
  >  (deps (package client))
  >  (action (with-stdout-to %{target} (run %{bin:geometry-tool}))))
  > EOF
  $ dune build from-client
  $ cat _build/default/from-client
  geometry-tool

Neither a downstream consumer of geometry nor an unrelated package was forced.

  $ find _build/_private/default/.opam -name cookie | wc -l
  1

Changing the native producer invalidates the opaque build and the final native
consumer. The recipe bakes the producer's value into the opaque archive, so the
new result cannot come from relinking alone. The caller-supplied recipe remains
unchanged.

  $ cp scalar/recipe.sh recipe.before
  $ echo 'let value = 50' > scalar/scalar.ml
  $ dune exec ./main.bc
  52
  $ cmp recipe.before scalar/recipe.sh

An installed library can depend on another library from the same opaque package.
Both libraries retain geometry as their owner, not their findlib root names.

  $ echo '(library (public_name client) (libraries matrix))' > client/dune
  $ echo 'let value = Matrix.value - 1' > client/client.ml
  $ dune exec ./main.bc
  52

Sublibraries and exists_if conditions use the ordinary metadata loader. Changing
only the prepared recipe invalidates the opaque library information.

  $ cat >> scalar/recipe.sh <<'EOF'
  > cat >> META.vector <<'META'
  > package "api" (
  >   requires = "scalar"
  >   archive(byte) = "vector.cma"
  > )
  > package "hidden" (
  >   archive(byte) = "missing.cma"
  >   exists_if = "missing.cma"
  > )
  > META
  > EOF
  $ echo '(library (public_name client) (libraries vector.api))' > client/dune
  $ echo 'let value = Vector.value' > client/client.ml
  $ dune exec ./main.bc
  52
  $ echo '(library (public_name client) (libraries vector.hidden))' > client/dune
  $ dune build main.bc 2>&1 | censor
  File "client/dune", line 1, characters 41-54:
  1 | (library (public_name client) (libraries vector.hidden))
                                               ^^^^^^^^^^^^^
  Error: Library "vector.hidden" in
  _build/_private/default/.opam/geometry-$DIGEST/target/lib/vector
  is hidden (unsatisfied 'exists_if').
  -> required by library "client" in _build/default/client
  -> required by executable main in dune:1
  -> required by _build/default/.main.eobjs/byte/dune__exe__Main.cmo
  -> required by _build/default/main.bc
  [1]

Unowned code uses the broad workspace scope. A missing eager library name forces
all visible opaque roots, including ones outside the client's dependency closure.

  $ echo '(library (public_name client) (libraries vector))' > client/dune
  $ for package in downstream unrelated; do
  >   echo "(opam (package $package) (dir src) (version 1.0))" > "$package/dune"
  > done
  $ echo '(executable (name main) (modes byte) (libraries vector))' > dune
  $ echo 'let () = Printf.printf "%d\n" Vector.value' > main.ml
  $ dune exec ./main.bc
  52
  $ find _build/_private/default/.opam -name cookie | wc -l
  3

Native metadata is consulted before lazy roots. A disabled native provider lets
lookup proceed to the selected opaque packages, ahead of ambient installations.

  $ mkdir lookup && cd lookup
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name geometry) (dir geometry) (allow_empty))
  > (package (name application) (dir application) (depends geometry) (allow_empty))
  > (package (name unrelated) (dir unrelated) (allow_empty))
  > EOF
  $ mkdir geometry application unrelated native-vector
  $ cat > native-vector/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name vector))
  > EOF
  $ echo '(library (public_name vector))' > native-vector/dune
  $ echo 'let value = 1' > native-vector/vector.ml
  $ for package in geometry unrelated; do
  >   cat > "$package/dune" <<EOF
  > (opam
  >  (package $package)
  >  (dir src)
  >  (version 1.0)
  >  (build (run sh -c "echo $package >&2; exit 1")))
  > EOF
  > done
  $ cat > application/dune <<'EOF'
  > (executable (name main) (modes byte) (libraries vector))
  > EOF
  $ echo 'let () = Printf.printf "%d\n" Vector.value' > application/main.ml
  $ dune exec ./application/main.bc
  1
  $ test ! -d _build/_private/default/.opam

  $ mkdir -p ambient/lib/vector
  $ cat > ambient/lib/vector/META <<'EOF'
  > archive(byte) = "vector.cma"
  > EOF
  $ echo 'let value = 99' > ambient/lib/vector/vector.ml
  $ (cd ambient/lib/vector && ocamlc -c vector.ml && ocamlc -a vector.cmo -o vector.cma)
  $ export OCAMLPATH="$PWD/ambient/lib${OCAMLPATH:+:$OCAMLPATH}"
  $ echo '(library (public_name vector) (enabled_if false))' > native-vector/dune
  $ cat > geometry/recipe.sh <<'EOF'
  > set -eu
  > echo 'let value = 2' > vector.ml
  > ocamlc -c vector.ml
  > ocamlc -a vector.cmo -o vector.cma
  > echo 'archive(byte) = "vector.cma"' > META
  > cat > geometry.install <<'INSTALL'
  > lib_root: [
  >   "META" {"vector/META"}
  >   "vector.cmi" {"vector/vector.cmi"}
  >   "vector.cma" {"vector/vector.cma"}
  > ]
  > INSTALL
  > EOF
  $ cat > geometry/dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir src)
  >  (version 1.0)
  >  (files (../recipe.sh recipe.sh))
  >  (build (run sh ./recipe.sh)))
  > EOF
  $ dune exec ./application/main.bc
  2
  $ cp geometry/recipe.sh recipe.saved

Removing the package dependency stops opaque discovery for this consumer. The
ambient library remains available and a changed opaque recipe is not forced.

  $ sed 's/(depends geometry)//' dune-project > dune-project.new
  $ mv dune-project.new dune-project
  $ echo 'echo geometry >&2; exit 1' > geometry/recipe.sh
  $ dune exec ./application/main.bc
  99

Two selected opaque providers use the normal library ambiguity diagnostic.

  $ cp recipe.saved geometry/recipe.sh
  $ sed 's/(name application)/(name application) (depends geometry other-geometry)/' dune-project > dune-project.new
  $ mv dune-project.new dune-project
  $ echo '(package (name other-geometry) (dir other-geometry) (allow_empty))' >> dune-project
  $ mkdir other-geometry
  $ cp recipe.saved other-geometry/recipe.sh
  $ cat > other-geometry/dune <<'EOF'
  > (opam
  >  (package other-geometry)
  >  (dir src)
  >  (version 1.0)
  >  (files (../recipe.sh recipe.sh))
  >  (build
  >   (progn
  >    (run sh ./recipe.sh)
  >    (run mv geometry.install other-geometry.install))))
  > EOF
  $ dune build application/main.bc 2>duplicate.error
  [1]
  $ grep -o 'Library with name "vector" is already defined' duplicate.error
  Library with name "vector" is already defined
  $ grep -q '/geometry-' duplicate.error
  $ grep -q '/other-geometry-' duplicate.error

Compiler libraries are not attributed to each opaque provider during discovery.

  $ echo '(executable (name main) (modes byte) (libraries unix))' > application/dune
  $ echo 'let () = Printf.printf "%b\n" (Unix.getpid () > 0)' > application/main.ml
  $ dune exec ./application/main.bc
  true

An opaque library resolves its transitive requirements in the producing
package's scope, not in the broader scope of the native consumer. Both support
packages below provide a library named support. Geometry depends only on
support-a, while the client can also see support-b. Vector's requirement must
therefore select support-a without reporting an ambiguity.

  $ cd ..
  $ mkdir scopes && cd scopes
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name support-a) (dir support-a) (allow_empty))
  > (package (name support-b) (dir support-b) (allow_empty))
  > (package (name geometry) (dir geometry) (depends support-a) (allow_empty))
  > (package (name client) (dir client) (depends geometry support-b))
  > EOF
  $ mkdir support-a support-b geometry client
  $ cat > support-a/dune <<'EOF'
  > (opam
  >  (package support-a)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file support.ml "let value = 10\n")
  >    (run ocamlc -c support.ml)
  >    (run ocamlc -a support.cmo -o support.cma)
  >    (write-file META "archive(byte) = \"support.cma\"\n")
  >    (write-file support-a.install
  >     "\| lib_root: [
  >     "\|  "META" {"support/META"}
  >     "\|  "support.cmi" {"support/support.cmi"}
  >     "\|  "support.cma" {"support/support.cma"}
  >     "\| ]
  >     ))))
  > EOF
  $ sed 's/support-a/support-b/g; s/value = 10/value = 100/' \
  > support-a/dune > support-b/dune
  $ cat > geometry/recipe.sh <<'EOF'
  > set -eu
  > support_dir=$(dirname "$1")
  > echo 'let value = Support.value + 1' > vector.ml
  > ocamlc -I "$support_dir" -c vector.ml
  > ocamlc -a vector.cmo -o vector.cma
  > printf 'requires = "support"\narchive(byte) = "vector.cma"\n' > META
  > cat > geometry.install <<'INSTALL'
  > lib_root: [
  >   "META" {"vector/META"}
  >   "vector.cmi" {"vector/vector.cmi"}
  >   "vector.cma" {"vector/vector.cma"}
  > ]
  > INSTALL
  > EOF
  $ cat > geometry/dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir src)
  >  (version 1.0)
  >  (files (../recipe.sh recipe.sh))
  >  (build
  >   (run sh ./recipe.sh %{pkg:support-a:lib_root:support/support.cmi})))
  > EOF
  $ cat > client/dune <<'EOF'
  > (executable
  >  (name main)
  >  (public_name scoped-client)
  >  (package client)
  >  (modes byte)
  >  (libraries vector))
  > EOF
  $ echo 'let () = Printf.printf "%d\n" Vector.value' > client/main.ml
  $ dune exec ./client/main.bc
  11

Optional executables outside package directories must use their explicit owner
for installation and binary availability as well as compilation.

  $ mkdir ../optional && cd ../optional
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name geometry) (dir geometry) (allow_empty))
  > (package (name tools) (depends geometry) (allow_empty))
  > (package (name unrelated) (dir unrelated) (allow_empty))
  > EOF
  $ mkdir geometry standalone unrelated
  $ cat > geometry/dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file META "description = \"vector\"\n")
  >    (write-file geometry.install "lib_root: [ \"META\" {\"vector/META\"} ]"))))
  > EOF
  $ echo '(opam (package unrelated) (dir src) (version 1.0) (build (run false)))' > unrelated/dune
  $ cat > standalone/dune <<'EOF'
  > (executable
  >  (name main)
  >  (public_name optional-tool)
  >  (package tools)
  >  (optional)
  >  (modes byte_complete)
  >  (libraries vector))
  > EOF
  $ echo 'let () = print_endline "optional-tool"' > standalone/main.ml
  $ cat > dune <<'EOF'
  > (rule
  >  (target tool-package)
  >  (deps (package tools))
  >  (action (write-file %{target} installed)))
  > (rule
  >  (target run-tool)
  >  (action (with-stdout-to %{target} (run %{bin:optional-tool}))))
  > EOF
  $ dune build standalone/main.bc.exe
  $ dune build tool-package run-tool
  $ cat _build/default/tool-package
  installed
  $ cat _build/default/run-tool
  optional-tool

Public library-file lookups must also use the owning package's dependency view.
Both lib and libexec leave unrelated producers unbuilt, including when libexec
selects the host context.

  $ echo '(package (name application) (dir application) (depends geometry) (allow_empty))' >> dune-project
  $ mkdir application
  $ cat > application/dune <<'EOF'
  > (rule
  >  (target metadata)
  >  (action (with-stdout-to %{target} (cat %{lib:vector:META}))))
  > (rule
  >  (target host-metadata)
  >  (action (with-stdout-to %{target} (cat %{libexec:vector:META}))))
  > EOF
  $ dune build application/metadata application/host-metadata
  $ cat _build/default/application/metadata _build/default/application/host-metadata
  description = "vector"
  description = "vector"
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (context (default))
  > (context (default (name target) (host default)))
  > EOF
  $ dune build _build/target/application/host-metadata
  $ cat _build/target/application/host-metadata
  description = "vector"

Producer-scoped lookup must still detect different identities for the same
library in one consumer's closure. Empty libraries isolate Dune's overlap check
from compiler diagnostics.

  $ mkdir ../overlap && cd ../overlap
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name p) (dir p) (allow_empty))
  > (package (name q) (dir q) (allow_empty))
  > (package (name app) (dir app) (depends p q) (allow_empty))
  > EOF
  $ mkdir p q app
  $ for package in p q; do
  >   cat > "$package/dune" <<EOF
  > (opam
  >  (package $package)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file META "requires = \"shared\"\n")
  >    (write-file META.shared "description = \"$package\"\n")
  >    (write-file $package.install
  >     "lib_root: [ \"META\" {\"$package/META\"} \"META.shared\" {\"shared/META\"} ]"))))
  > EOF
  > done
  $ echo '(executable (name main) (modes byte) (libraries p q))' > app/dune
  $ echo 'let () = ()' > app/main.ml
  $ dune build app/main.bc 2>&1 | censor
  Error: Conflict between the following libraries:
  - "shared" in
    _build/_private/default/.opam/p-$DIGEST1/target/lib/shared
    -> required by library "p" in
       _build/_private/default/.opam/p-$DIGEST1/target/lib/p
  - "shared" in
    _build/_private/default/.opam/q-$DIGEST2/target/lib/shared
    -> required by library "q" in
       _build/_private/default/.opam/q-$DIGEST2/target/lib/q
  -> required by executable main in app/dune:1
  -> required by _build/default/app/.main.eobjs/byte/dune__exe__Main.cmo
  -> required by _build/default/app/main.bc
  [1]

The existing opt-in policy should continue to allow overlapping dependencies.

  $ echo '(executable (name main) (modes byte) (libraries p q) (allow_overlapping_dependencies))' > app/dune
  $ dune build app/main.bc

Repeated metadata requests must not mix build contexts. Changing the opaque
recipe must still invalidate both contexts' library views.

  $ mkdir ../contexts && cd ../contexts
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name geometry) (dir geometry) (allow_empty))
  > (package (name client) (dir client) (depends geometry))
  > EOF
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (context (default))
  > (context (default (name other)))
  > EOF
  $ mkdir geometry client
  $ cat > geometry/dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file vector.ml "let value = \"%{context_name}\"\n")
  >    (run ocamlc -c vector.ml)
  >    (run ocamlc -a vector.cmo -o vector.cma)
  >    (write-file META "archive(byte) = \"vector.cma\"\n")
  >    (write-file geometry-context-tool "#!/bin/sh\necho %{context_name}\n")
  >    (run chmod +x geometry-context-tool)
  >    (write-file geometry.install
  >     "bin: [ \"geometry-context-tool\" ] lib_root: [ \"META\" {\"vector/META\"} \"vector.cmi\" {\"vector/vector.cmi\"} \"vector.cma\" {\"vector/vector.cma\"} ]"))))
  > EOF
  $ echo '(library (public_name client) (modes byte) (libraries vector))' > client/dune
  $ echo 'let value = Vector.value' > client/client.ml
  $ cat > dune <<'EOF'
  > (executable (name main) (modes byte) (libraries client))
  > (rule
  >  (target tool)
  >  (deps (package geometry))
  >  (action (with-stdout-to %{target} (run %{bin:geometry-context-tool}))))
  > EOF
  $ echo 'let () = print_endline Client.value' > main.ml
  $ dune build _build/default/main.bc _build/other/main.bc _build/default/client/META.client _build/default/client/client.dune-package _build/other/client/META.client _build/other/client/client.dune-package
  $ dune exec --context default ./main.bc
  default
  $ dune exec --context other ./main.bc
  other
  $ dune build _build/default/tool _build/other/tool
  $ cat _build/default/tool _build/other/tool
  default
  other
  $ sed 's/%{context_name}/%{context_name}-updated/' geometry/dune > geometry/dune.new
  $ mv geometry/dune.new geometry/dune
  $ dune exec --context default ./main.bc
  default-updated
  $ dune exec --context other ./main.bc
  other-updated
  $ dune build _build/default/tool _build/other/tool
  $ cat _build/default/tool _build/other/tool
  default-updated
  other-updated

An opaque producer can provide dune-package metadata without META. Removing that
metadata must invalidate an unchanged consumer rather than retain a cached entry.

  $ mkdir ../dune-package && cd ../dune-package
  $ unset OCAMLPATH
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name geometry) (dir geometry) (allow_empty))
  > (package (name client) (dir client) (depends geometry) (allow_empty))
  > EOF
  $ mkdir geometry client
  $ cat > geometry/dune-package <<'EOF'
  > (lang dune 3.25)
  > (name vector)
  > (library
  >  (name vector)
  >  (kind normal)
  >  (modes byte)
  >  (archives (byte vector.cma))
  >  (modules
  >   (singleton
  >    (obj_name vector)
  >    (visibility public)
  >    (source (path Vector) (impl (path vector.ml))))))
  > EOF
  $ cat > geometry/geometry.install <<'EOF'
  > lib_root: [
  >   "dune-package" {"vector/dune-package"}
  >   "vector.ml" {"vector/vector.ml"}
  >   "vector.cmi" {"vector/vector.cmi"}
  >   "vector.cma" {"vector/vector.cma"}
  > ]
  > EOF
  $ cat > geometry/dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir src)
  >  (version 1.0)
  >  (files (../dune-package dune-package) (../geometry.install geometry.install))
  >  (build
  >   (progn
  >    (write-file vector.ml "let value = 42\n")
  >    (run ocamlc -c vector.ml)
  >    (run ocamlc -a vector.cmo -o vector.cma))))
  > EOF
  $ echo '(executable (name main) (modes byte) (libraries vector))' > client/dune
  $ echo 'let () = Printf.printf "%d\n" Vector.value' > client/main.ml
  $ dune exec ./client/main.bc
  42
  $ test ! -e _build/_private/default/.opam/geometry-*/target/lib/vector/META
  $ cp geometry/geometry.install manifest.before
  $ grep -v '"dune-package"' manifest.before > geometry/geometry.install
  $ dune build client/main.bc 2>error
  [1]
  $ grep '^Error:' error
  Error: Library "vector" not found.
  $ cp manifest.before geometry/geometry.install
  $ dune exec ./client/main.bc
  42

Multiple invalid metadata providers must not discard the metadata diagnostic.
The original error is retained when a second invalid provider becomes visible.

  $ cp geometry/dune-package metadata.before
  $ echo '' > geometry/dune-package
  $ dune build client/main.bc 2>error
  [1]
  $ grep '^Error:' error
  Error: Invalid first line, expected: (lang <lang> <version>)
  $ echo '(package (name other) (dir other) (allow_empty))' >> dune-project
  $ sed 's/(depends geometry)/(depends geometry other)/' dune-project > dune-project.new
  $ mv dune-project.new dune-project
  $ mkdir other
  $ cat > other/dune <<'EOF'
  > (opam
  >  (package other)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file metadata "")
  >    (write-file other.install "lib_root: [ \"metadata\" {\"vector/dune-package\"} ]"))))
  > EOF
  $ dune build client/main.bc 2>error
  [1]
  $ grep '^Error:' error
  Error: Invalid first line, expected: (lang <lang> <version>)

A third valid provider must not hide a duplicate already found between two
others.

  $ cp metadata.before geometry/dune-package
  $ cat > other/dune <<'EOF'
  > (opam
  >  (package other)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file metadata "description = \"other\"\n")
  >    (write-file other.install "lib_root: [ \"metadata\" {\"vector/META\"} ]"))))
  > EOF
  $ echo '(package (name third) (dir third) (allow_empty))' >> dune-project
  $ sed 's/(depends geometry other)/(depends geometry other third)/' dune-project > dune-project.new
  $ mv dune-project.new dune-project
  $ mkdir third
  $ sed 's/other/third/g' other/dune > third/dune
  $ echo 'let () = ()' > client/main.ml
  $ dune build client/main.bc 2>error
  [1]
  $ grep -o 'Library with name "vector" is already defined' error
  Library with name "vector" is already defined

An opaque-installed PPX must retain its selected provider when the generated
PPX driver is reconstructed, without searching unrelated roots.

  $ mkdir ../ppx && cd ../ppx
  $ export DUNE_DEBUG_PACKAGE_LOGS=0
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name geometry) (dir geometry) (allow_empty))
  > (package (name client) (dir client) (depends geometry) (allow_empty))
  > (package (name unrelated) (dir unrelated) (allow_empty))
  > EOF
  $ mkdir -p geometry/src client unrelated
  $ cat > geometry/dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir src)
  >  (version 1.0)
  >  (source .)
  >  (build (run dune build @install))
  >  (install (run dune install --prefix %{prefix})))
  > EOF
  $ cat > geometry/src/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name noop))
  > EOF
  $ cat > geometry/src/dune <<'EOF'
  > (library
  >  (public_name noop)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Noop.main)))
  > EOF
  $ cat > geometry/src/noop.ml <<'EOF'
  > let main () =
  >   match Sys.argv with
  >   | [| _; "--as-ppx"; "--cookie"; _; input; output |]
  >   | [| _; "--as-ppx"; input; output |] ->
  >     exit (Sys.command (Filename.quote_command "cp" [input; output]))
  >   | _ -> failwith "unexpected PPX arguments"
  > EOF
  $ echo '(opam (package unrelated) (dir src) (version 1.0) (build (run false)))' > unrelated/dune
  $ cat > client/dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes byte)
  >  (preprocess (staged_pps noop)))
  > EOF
  $ echo 'let () = print_endline "preprocessed"' > client/main.ml
  $ dune exec ./client/main.bc
  preprocessed

A public library outside an exclusive package directory must use its owner for
both preprocessing and archive flag expansion.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name geometry) (dir geometry) (allow_empty))
  > (package (name client) (depends geometry) (allow_empty))
  > (package (name unrelated) (dir unrelated) (allow_empty))
  > EOF
  $ echo '(install (section lib) (files flags))' >> geometry/src/dune
  $ echo '()' > geometry/src/flags
  $ cat > client/dune <<'EOF'
  > (library
  >  (public_name client)
  >  (library_flags (:include %{lib:noop:flags}))
  >  (preprocess (staged_pps noop)))
  > EOF
  $ dune build client/client.cma
  $ cat > client/dune <<'EOF'
  > (library
  >  (public_name client)
  >  (library_flags (:include %{lib:noop:flags})))
  > EOF
  $ dune build client/client.cma

Materializing native inputs must not load the entire legacy install namespace.
Otherwise an optional executable depending on the opaque producer closes a
spurious cycle through that producer's native dependency layout.

  $ mkdir ../install-root && cd ../install-root
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package (name source) (dir source))
  > (package (name geometry) (dir geometry) (depends source) (allow_empty))
  > (package (name tools) (depends geometry) (allow_empty))
  > EOF
  $ mkdir source geometry standalone
  $ echo input > source/data
  $ echo '(install (package source) (section share) (files data))' > source/dune
  $ cat > geometry/dune <<'EOF'
  > (opam
  >  (package geometry)
  >  (dir src)
  >  (version 1.0)
  >  (build
  >   (progn
  >    (write-file META "description = \"vector\"\n")
  >    (write-file geometry.install "lib_root: [ \"META\" {\"vector/META\"} ]"))))
  > EOF
  $ cat > standalone/dune <<'EOF'
  > (executable
  >  (name main)
  >  (public_name optional-tool)
  >  (package tools)
  >  (optional)
  >  (modes byte_complete)
  >  (libraries vector))
  > EOF
  $ echo 'let () = print_endline "independent"' > standalone/main.ml
  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package tools))
  >  (action (with-stdout-to %{target} (run %{bin:optional-tool}))))
  > EOF
  $ dune build result
  $ cat _build/default/result
  independent
  $ dune build tools.install
  $ test -f _build/_private/default/.packages/*/share/source/data
  $ dune build result
  $ cat _build/default/result
  independent

An explicitly owned executable must use its package view for expansions too,
not just its libraries field. A library-file link dependency must leave unrelated
producers unbuilt.

  $ echo '(package (name unrelated) (dir unrelated) (allow_empty))' >> dune-project
  $ mkdir unrelated
  $ echo '(opam (package unrelated) (dir src) (version 1.0) (build (run false)))' > unrelated/dune
  $ cat > standalone/dune <<'EOF'
  > (executable
  >  (name main)
  >  (public_name optional-tool)
  >  (package tools)
  >  (optional)
  >  (modes byte_complete)
  >  (link_deps %{lib:vector:META})
  >  (libraries vector))
  > EOF
  $ dune exec ./standalone/main.bc.exe
  independent

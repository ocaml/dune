A regression test for https://github.com/ocaml/dune/issues/15642. The
user-facing problem is that the standard library is not found, even when it
should be available as a required dependency for a rule.

The core mechanism responsible for the error's possibility is as follows:

Compilers installed by the dune package manager are stored in [_build]. If
possible, sandboxed actions invoke the compiler through a *symlink* located in
the relevant [_build/.sandbox], with the link pointing to the aforementioned
installed compiler. Now, the runtime of a *relocatable* compiler will try to
locate its Stdlib *relative to the invoked compiler executable* (see
https://github.com/ocaml/ocaml/commit/1df30d4f172b2dd46966cbfd9eaae9bba6743063).
When the location of the installed compiler is not properly identified, the
Stdlib can end up missing.

On *Linux*, where dune generally defaults to using a symlink to the compiler
executable, the symlink gets fully resolved by the runtime to a *real path* via
[readlink]
(https://github.com/ocaml/ocaml/blob/5087cbb24c35b9115cd01e1e8fbc0a4936dace9f/runtime/unix.c#L363).
However, on macOS, `_NSGetExecutablePath` is used, which doesn't actually
resolve to a real path, instead remaining the symbolic link (see
https://github.com/ocaml/ocaml/blob/5087cbb24c35b9115cd01e1e8fbc0a4936dace9f/runtime/unix.c#L390-L396).
On any other non-linux Unix, the runtime will try searching in the path for the
ocaml executable if invoked with a bare name (e.g., [ocamlopt]) with a fallback
to the unaltered `argv[0]` if the invocation has the form of a path (such as
[../_private/default/.pkg/.../target/bin/ocamlc.opt]), and this can again end up
with an unresolved symlink. To find the stdlib from a relative path, the runtime
just takes the directory of the executable path it derives and appends the
relative directory for the default Stdlib
(https://github.com/ocaml/ocaml/blob/5087cbb24c35b9115cd01e1e8fbc0a4936dace9f/runtime/unix.c#L588-L591).
But when the executable location is not resolved to its real path, instead being
taken as the linked location, this lookup will fail to find the Stdlib, *unless
the stdlib itself was also linked to the expected location*.

Dune hit this error because the Stdlib has not been specified as a dependency
for the interface generation rule, and so was *not* put in the expected place in
sandbox. Thus, when Menhir's type inference ran `ocamlc -i` using the
relocatable compiler installed by dune package management on non-Linux
platforms, it failed with "Unbound module Stdlib".

The wrapper below stands in for a relocatable compiler. To reproduce the
runtime's Stdlib lookup behavior outside of Linux, it locates its standard
library relative to `$0` and then delegates to the actual compiler. A shell sees
the unresolved link in $0 on every Unix, so this reproduces the behaviour
encountered on macOS everywhere:

  $ real_ocaml_bin=$(dirname "$(command -v ocamlc)")
  $ real_ocaml_lib=$(ocamlc -where)
  $ real_menhir=$(command -v menhir)

  $ mkdir fake-compiler
  $ {
  > # This block neeeds variable expansion
  >   cat <<EOF
  > #!/bin/sh
  > real_ocaml_bin='$real_ocaml_bin'
  > EOF
  > # The next block should be verbatim
  >   cat <<'EOF'
  > tool=$(basename "$0")
  > case "$tool" in
  > ocamlc | ocamlc.opt | ocamlopt | ocamlopt.opt)
  >   # derive the unresolved directory of the compiler invocation path
  >   self_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
  >   # use this derived path as the basis for "locating" the relative Stdlib
  >   OCAMLLIB="$self_dir/../lib/ocaml"
  >   export OCAMLLIB
  >   ;;
  > esac
  > exec "$real_ocaml_bin/$tool" "$@"
  > EOF
  > } > fake-compiler/compiler
  $ chmod +x fake-compiler/compiler

Menhir must come from the lock directory too, so wrap the test's menhir:

  $ mkdir fake-menhir
  $ {
  >   echo '#!/bin/sh'
  >   echo "exec '$real_menhir' \"\$@\""
  > } > fake-menhir/menhir
  $ chmod +x fake-menhir/menhir

Only a compiler installed inside _build can hit this bug, but compilers won't
end up in _build unless the compiler package is version 5.5.0 or later or it
depends on a relocatable-compiler marker package (see [Pkg_rules.resolve_impl]).
We declare the compiler version as 5.2.1, to match the bug report, but this then
requires the marker package to ensure the compiler ends up in _build. Dune does
not compare the declared version with the compiler's own reported version.

  $ make_lockdir
  $ cat >> dune.lock/lock.dune <<'EOF'
  > (ocaml ocaml-base-compiler)
  > EOF

  $ make_lockpkg relocatable-compiler <<EOF
  > (version 5.2.1)
  > EOF

  $ make_lockpkg ocaml-base-compiler <<EOF
  > (version 5.2.1)
  > (depends relocatable-compiler)
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin %{prefix}/lib/ocaml)
  >   (run sh -c
  >    "for t in ocamlc ocamlc.opt ocamldep ocamldep.opt ocamlmklib \
  >       ocamlobjinfo ocamlopt ocamlopt.opt ocaml; \
  >     do cp compiler %{prefix}/bin/\$t; done")
  >   (run cp $real_ocaml_lib/Makefile.config
  >        %{prefix}/lib/ocaml/Makefile.config)
  >   (run sh -c "cp $real_ocaml_lib/*.cmi %{prefix}/lib/ocaml")))
  > (source (copy $PWD/fake-compiler))
  > EOF

  $ make_lockpkg menhir <<EOF
  > (version 1)
  > (install
  >  (progn
  >   (run mkdir -p %{prefix}/bin)
  >   (run cp menhir %{prefix}/bin/menhir)))
  > (source (copy $PWD/fake-menhir))
  > EOF

  $ cat > dune-project <<'EOF'
  > (lang dune 3.24)
  > (using menhir 2.0)
  > (package
  >  (name repro)
  >  (allow_empty)
  >  (depends ocaml-base-compiler menhir))
  > EOF

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.24)
  > (pkg enabled)
  > (context default)
  > EOF

  $ cat > dune <<'EOF'
  > (library
  >  (name repro))
  > (menhir
  >  (modules parser))
  > EOF

  $ cat > parser.mly <<'EOF'
  > %token <int> INT
  > %token EOF
  > %start <int> main
  > %%
  > main:
  > | i = INT EOF { i + 1 }
  > EOF

The compiler runs through the sandbox link, computes its standard library path
relative to it, and finds nothing there:

  $ dune build _build/default/parser__mock.mli.inferred
  File "command line", line 1:
  Error: Unbound module Stdlib
  [1]

A regression test for https://github.com/ocaml/dune/issues/16521

Setup to reproduce the OxCaml build layout, in which a standard library is build
inside the _build directory by a  `make` target, outside of any registered build
context. OxCaml does this for its boot compiler (_build/_bootinstall) and for
the freshly built runtime stdlib (_build/runtime_stdlib_install), and points the
context's OCAMLLIB at them, so `ocamlc -config` reports a standard_library under
_build. The setup also includes `using menhir`, which is required to trigger the
path that will look up the stdlib during interface generation, required to
trigger the error.

  $ W=$(ocamlc -where)
  $ setup() {
  >   mkdir -p "$1"
  >   cp "$W"/*.cm* "$W"/*.a "$W"/*.o "$W"/Makefile.config "$1"/
  >   cp -r "$W"/unix "$W"/str "$W"/caml "$1"/
  >   cat > dune-workspace <<EOF
  > (lang dune 3.24)
  > (context
  >  (default
  >   (paths (OCAMLLIB ("$1")))))
  > EOF
  >   cat > dune-project <<EOF
  > (lang dune 3.24)
  > (using menhir 2.1)
  > EOF
  >   cat > dune <<EOF
  > (executable (name main) (libraries unix))
  > (menhir (modules parser))
  > EOF
  >   cat > main.ml <<EOF
  > let () = ignore (Unix.getpid ()); print_endline (Filename.basename (Sys.getcwd ()))
  > EOF
  >   cat > parser.mly <<EOF
  > %token <int> INT
  > %token EOF
  > %start <int> main
  > %%
  > main:
  > | i = INT EOF { i + 1 }
  > EOF
  > }

When the stdlib is inside a subdirectory of the _build, but not in any
registered dune context, (as in OxCaml's Makefile, which puts it in
_build/_bootinstall/lib/ocaml), dune must avoid treating the stdlib is if it
were part of the build graph, leave the path "external", enabling the build to
succeed. In other words, only paths inside a registered build context can be
considered dune's own artifacts, and only those should be localized, and users
should otherwise be able to stash their non-dune build artifacts in
subdirectories of _build.

  $ mkdir inbuild && cd inbuild
  $ setup "$PWD/_build/_bootinstall/lib/ocaml"
  $ dune build ./main.exe 2>&1 | sed "s|$PWD|PWD|g"

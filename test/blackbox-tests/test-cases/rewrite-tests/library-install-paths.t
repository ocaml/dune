Test the rewriting of a minimal library, and show that it does not conform to
the locations where the artifacts are installed.

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (name minlib)
  > (package
  >  (name minlib))
  > EOF

  $ mkdir lib

  $ cat > lib/dune <<EOF
  > (library
  >  (name minlib)
  >  (public_name minlib))
  > (rule
  >  (target x)
  >  (action
  >    (system "\| dune_cmd rewrite-path $PWD/_build/default/lib/run.ml \\
  >            "\| | grep -c '^/install_root/lib/minlib/run.ml$';
  >            "\| touch x
  >    )))
  > EOF

  $ cat > lib/run.ml <<EOF
  > let run () =
  >   print_endline "minlib.run"
  > EOF

In the following, we want the rewrite to give /install_root/lib/minlib/run.ml.
It currently gives /workspace_root/lib/run.mm and so does not match the regular
expression. The grep count should be 1 rather than 0, and the exit code from
the grep 0.

  $ dune build @all 2>&2 | dune_cmd sanitize
  0
  $ dune install --dry-run --prefix _install --display short
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/META
  Installing _install/lib/minlib/META
  Copying _build/install/default/lib/minlib/META to _install/lib/minlib/META (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/dune-package
  Installing _install/lib/minlib/dune-package
  Copying _build/install/default/lib/minlib/dune-package to _install/lib/minlib/dune-package (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib.a
  Installing _install/lib/minlib/minlib.a
  Copying _build/install/default/lib/minlib/minlib.a to _install/lib/minlib/minlib.a (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib.cma
  Installing _install/lib/minlib/minlib.cma
  Copying _build/install/default/lib/minlib/minlib.cma to _install/lib/minlib/minlib.cma (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib.cmi
  Installing _install/lib/minlib/minlib.cmi
  Copying _build/install/default/lib/minlib/minlib.cmi to _install/lib/minlib/minlib.cmi (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib.cmt
  Installing _install/lib/minlib/minlib.cmt
  Copying _build/install/default/lib/minlib/minlib.cmt to _install/lib/minlib/minlib.cmt (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib.cmx
  Installing _install/lib/minlib/minlib.cmx
  Copying _build/install/default/lib/minlib/minlib.cmx to _install/lib/minlib/minlib.cmx (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib.cmxa
  Installing _install/lib/minlib/minlib.cmxa
  Copying _build/install/default/lib/minlib/minlib.cmxa to _install/lib/minlib/minlib.cmxa (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib.ml
  Installing _install/lib/minlib/minlib.ml
  Copying _build/install/default/lib/minlib/minlib.ml to _install/lib/minlib/minlib.ml (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib__Run.cmi
  Installing _install/lib/minlib/minlib__Run.cmi
  Copying _build/install/default/lib/minlib/minlib__Run.cmi to _install/lib/minlib/minlib__Run.cmi (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib__Run.cmt
  Installing _install/lib/minlib/minlib__Run.cmt
  Copying _build/install/default/lib/minlib/minlib__Run.cmt to _install/lib/minlib/minlib__Run.cmt (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib__Run.cmx
  Installing _install/lib/minlib/minlib__Run.cmx
  Copying _build/install/default/lib/minlib/minlib__Run.cmx to _install/lib/minlib/minlib__Run.cmx (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/run.ml
  Installing _install/lib/minlib/run.ml
  Copying _build/install/default/lib/minlib/run.ml to _install/lib/minlib/run.ml (executable: false)
  Creating directory _install/lib/minlib
  Removing (if it exists) _install/lib/minlib/minlib.cmxs
  Installing _install/lib/minlib/minlib.cmxs
  Copying _build/install/default/lib/minlib/minlib.cmxs to _install/lib/minlib/minlib.cmxs (executable: true)

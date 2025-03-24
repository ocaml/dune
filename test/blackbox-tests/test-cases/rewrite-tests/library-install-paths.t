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
  Removing (if it exists) _install/lib/minlib/META
  Installing _install/lib/minlib/META
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/META to _install/lib/minlib/META (executable: false)
  Removing (if it exists) _install/lib/minlib/dune-package
  Installing _install/lib/minlib/dune-package
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/dune-package to _install/lib/minlib/dune-package (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib.a
  Installing _install/lib/minlib/minlib.a
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib.a to _install/lib/minlib/minlib.a (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib.cma
  Installing _install/lib/minlib/minlib.cma
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib.cma to _install/lib/minlib/minlib.cma (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib.cmi
  Installing _install/lib/minlib/minlib.cmi
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib.cmi to _install/lib/minlib/minlib.cmi (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib.cmt
  Installing _install/lib/minlib/minlib.cmt
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib.cmt to _install/lib/minlib/minlib.cmt (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib.cmx
  Installing _install/lib/minlib/minlib.cmx
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib.cmx to _install/lib/minlib/minlib.cmx (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib.cmxa
  Installing _install/lib/minlib/minlib.cmxa
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib.cmxa to _install/lib/minlib/minlib.cmxa (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib.ml
  Installing _install/lib/minlib/minlib.ml
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib.ml to _install/lib/minlib/minlib.ml (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib__Run.cmi
  Installing _install/lib/minlib/minlib__Run.cmi
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib__Run.cmi to _install/lib/minlib/minlib__Run.cmi (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib__Run.cmt
  Installing _install/lib/minlib/minlib__Run.cmt
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib__Run.cmt to _install/lib/minlib/minlib__Run.cmt (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib__Run.cmx
  Installing _install/lib/minlib/minlib__Run.cmx
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib__Run.cmx to _install/lib/minlib/minlib__Run.cmx (executable: false)
  Removing (if it exists) _install/lib/minlib/run.ml
  Installing _install/lib/minlib/run.ml
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/run.ml to _install/lib/minlib/run.ml (executable: false)
  Removing (if it exists) _install/lib/minlib/minlib.cmxs
  Installing _install/lib/minlib/minlib.cmxs
  Creating directory _install/lib/minlib
  Copying _build/install/default/lib/minlib/minlib.cmxs to _install/lib/minlib/minlib.cmxs (executable: true)

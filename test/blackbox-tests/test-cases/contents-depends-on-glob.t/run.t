There are some install rules, and there is also some globbing.

This can create a dependency cycle if "install rules" try to
produce targets in the directory being globbed.

(install rules depend on library artifacts, which in turn depend
on the directory listing)

  $ dune build @install

Check that the command did build things:

  $ ls _build/install/default/lib/bar/*.cmxa
  _build/install/default/lib/bar/bar.cmxa

When a file is installed to a custom libexec site, it should have the
executable bit set after `dune install`. The libexec section is meant for
executable files, so dune must set permissions accordingly.

See https://github.com/ocaml/dune/issues/11110

  $ cat > dune-project << EOF
  > (lang dune 3.23)
  > (using dune_site 0.1)
  > (package
  >  (name mypkg)
  >  (sites (libexec tools)))
  > EOF

  $ cat > dune << EOF
  > (install
  >  (section (site (mypkg tools)))
  >  (files mytool.sh))
  > EOF

  $ cat > mytool.sh << EOF
  > #!/bin/sh
  > echo "hello"
  > EOF

The generated .install file should place the file under the libexec_root
section, which causes dune install to set the executable bit:

  $ dune build @install
  $ cat _build/default/mypkg.install
  lib: [
    "_build/install/default/lib/mypkg/META"
    "_build/install/default/lib/mypkg/dune-package"
  ]
  libexec_root: [
    "_build/install/default/lib/mypkg/tools/mytool.sh" {"mypkg/tools/mytool.sh"}
  ]

Verify the executable bit is set after install:

  $ dune install --prefix _install 2>&1
  $ test -x _install/lib/mypkg/tools/mytool.sh && echo "executable" || echo "not executable"
  executable

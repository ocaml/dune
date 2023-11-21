Private packages are packages that don't generate install rules. This allows private
libraries and executables to be assodicated with them, without having to install them.

This allows for private libraries to have their dependencies resolved without having to
associate them to a public library.

First we declare a package named "hidden" as (private).

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > 
  > (package
  >  (name hidden)
  >  (private))
  > EOF

Next we make a private executable and private library.

  $ cat > dune << EOF
  > (library
  >  (name other_secret)
  >  (modules other_secret)
  >  (package hidden))
  > (executable
  >  (name secret)
  >  (modules secret)
  >  (package hidden))
  > EOF

  $ cat > secret.ml
  $ cat > other_secret.ml

We build the install layout.

  $ dune build @install

The executable is not in the install layout.

  $ ! [ -e _build/install/default/bin ]

The library is not in the install layout.

  $ ! [ -e _build/install/default/lib/other_secret.cmi ]

The install layout isn't even created since there are no install rules.

  $ ! [ -e _build/install ]

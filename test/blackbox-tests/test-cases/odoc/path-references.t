odoc 3 path references name a page or module by the tree it belongs to:
{!/pkg/page} for a page of package [pkg], {!/lib/Module} for a module of
library [lib]. odoc resolves them at link time from the page trees (-P) and
module trees (-L), which are rooted at the parent id of each package and
library.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package (name my_pkg))
  > EOF

  $ mkdir foo bar baz priv
  $ cat > foo/dune <<EOF
  > (library (public_name my_pkg.foo) (name foo))
  > EOF
  $ cat > foo/foo.mli <<EOF
  > val x : int
  > EOF
  $ cat > foo/foo.ml <<EOF
  > let x = 1
  > EOF

  $ cat > bar/dune <<EOF
  > (library (public_name my_pkg.bar) (name bar) (libraries my_pkg.foo))
  > EOF
  $ cat > bar/bar.mli <<EOF
  > (** Flat reference: {!Foo.x}.
  > 
  >     Module path reference: {!/my_pkg.foo/Foo.x}.
  > 
  >     Page path reference: {!/my_pkg/guide}.
  > 
  >     Current package reference: {!//guide}.
  > 
  >     Reference to a library that is not a dependency: {!/my_pkg.baz/Baz.z}. *)
  > val y : int
  > EOF
  $ cat > bar/bar.ml <<EOF
  > let y = Foo.x
  > EOF

A library of the package that bar does not depend on:

  $ cat > baz/dune <<EOF
  > (library (public_name my_pkg.baz) (name baz))
  > EOF
  $ cat > baz/baz.mli <<EOF
  > val z : int
  > EOF
  $ cat > baz/baz.ml <<EOF
  > let z = 2
  > EOF

A private library of the package:

  $ cat > priv/dune <<EOF
  > (library (name priv) (package my_pkg))
  > EOF
  $ cat > priv/priv.mli <<EOF
  > val p : int
  > EOF
  $ cat > priv/priv.ml <<EOF
  > let p = 3
  > EOF

  $ cat > dune <<EOF
  > (documentation (package my_pkg))
  > EOF
  $ cat > index.mld <<EOF
  > {0 my_pkg}
  > 
  > Flat page reference: {!page-guide}.
  > 
  > Page path reference: {!/my_pkg/guide}.
  > 
  > Module path reference: {!/my_pkg.bar/Bar}.
  > 
  > Private library reference: {!/priv/Priv}.
  > EOF
  $ cat > guide.mld <<EOF
  > {0 Guide}
  > EOF

Dune passes -P and -L to odoc, so path references resolve:

  $ dune build @doc

Both flat and path references are rendered as links:

  $ grep -o '<p>.*</p>' \
  >   _build/default/_doc/_html/my_pkg/my_pkg.bar/Bar/index.html \
  >   _build/default/_doc/_html/my_pkg/index.html
  _build/default/_doc/_html/my_pkg/my_pkg.bar/Bar/index.html:<p>Flat reference: <a href="../../my_pkg.foo/Foo/index.html#val-x"><code>Foo.x</code></a>.</p><p>Module path reference: <a href="../../my_pkg.foo/Foo/index.html#val-x"><code>Foo.x</code></a>.</p><p>Page path reference: <a href="../../guide.html"><code>guide</code></a>.</p><p>Current package reference: <a href="../../guide.html"><code>guide</code></a>.</p><p>Reference to a library that is not a dependency: <a href="../../my_pkg.baz/Baz/index.html#val-z"><code>Baz.z</code></a>.</p>
  _build/default/_doc/_html/my_pkg/index.html:<p>Flat page reference: <a href="guide.html"><code>guide</code></a>.</p><p>Page path reference: <a href="guide.html"><code>guide</code></a>.</p><p>Module path reference: <a href="my_pkg.bar/Bar/index.html"><code>Bar</code></a>.</p><p>Private library reference: <a href="my_pkg.__private__.priv/Priv/index.html"><code>Priv</code></a>.</p>

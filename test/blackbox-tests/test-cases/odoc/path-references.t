odoc 3 path references name a page or module by the tree it belongs to:
{!/pkg/page} for a page of package [pkg], {!/lib/Module} for a module of
library [lib]. odoc resolves them at link time from the page trees (-P) and
module trees (-L), which are rooted at the parent id of each package and
library.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package (name my_pkg))
  > EOF

  $ mkdir foo bar
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
  >     Page path reference: {!/my_pkg/guide}. *)
  > val y : int
  > EOF
  $ cat > bar/bar.ml <<EOF
  > let y = Foo.x
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
  > EOF
  $ cat > guide.mld <<EOF
  > {0 Guide}
  > EOF

Dune does not pass -P and -L to odoc, so path references fail to resolve:

  $ dune build @doc 2>&1 | grep Warning | sort
  Warning: Failed to resolve reference /my_pkg.bar/Bar Path '/my_pkg.bar/Bar' not found
  Warning: Failed to resolve reference /my_pkg.foo/Foo.x Path '/my_pkg.foo/Foo' not found
  Warning: Failed to resolve reference /my_pkg/guide Path '/my_pkg/guide' not found
  Warning: Failed to resolve reference /my_pkg/guide Path '/my_pkg/guide' not found

Flat references are rendered as links, path references as plain code:

  $ grep -o '<p>.*</p>' \
  >   _build/default/_doc/_html/my_pkg/Bar/index.html \
  >   _build/default/_doc/_html/my_pkg/index.html
  _build/default/_doc/_html/my_pkg/Bar/index.html:<p>Flat reference: <a href="../Foo/index.html#val-x"><code>Foo.x</code></a>.</p><p>Module path reference: <code>/my_pkg.foo/Foo.x</code>.</p><p>Page path reference: <code>/my_pkg/guide</code>.</p>
  _build/default/_doc/_html/my_pkg/index.html:<p>Flat page reference: <a href="guide.html"><code>guide</code></a>.</p><p>Page path reference: <code>/my_pkg/guide</code>.</p><p>Module path reference: <code>/my_pkg.bar/Bar</code>.</p>

The files field of the documentation stanza can rename mld pages with [as].
Top-level pages are built under their new name.

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package (name my_pkg) (allow_empty))
  > EOF

  $ mkdir doc
  $ cat > doc/dune <<EOF
  > (env (dev (odoc (warnings fatal))))
  > (documentation
  >  (package my_pkg)
  >  (mld_files)
  >  (files
  >   (main.mld as index.mld)
  >   (draft.mld as guide.mld)
  >   other.mld))
  > EOF
  $ cat > doc/main.mld <<EOF
  > {0 Renamed index}
  > EOF
  $ cat > doc/draft.mld <<EOF
  > {0 Renamed guide}
  > EOF
  $ cat > doc/other.mld <<EOF
  > {0 Other}
  > 
  > See {!page-index} and {!page-guide}.
  > EOF

  $ dune build @doc

Pages are named after their new name, with the content of the renamed file:

  $ ls _build/default/_doc/_html/my_pkg
  db.js
  guide.html
  index.html
  other.html
  $ grep -o '<h1[^>]*>.*</h1>' \
  >   _build/default/_doc/_html/my_pkg/index.html \
  >   _build/default/_doc/_html/my_pkg/guide.html
  _build/default/_doc/_html/my_pkg/index.html:<h1 id="renamed-index"><a href="#renamed-index" class="anchor"></a>Renamed index</h1>
  _build/default/_doc/_html/my_pkg/guide.html:<h1 id="renamed-guide"><a href="#renamed-guide" class="anchor"></a>Renamed guide</h1>

References use the new names:

  $ grep -o '<p>.*</p>' _build/default/_doc/_html/my_pkg/other.html
  <p>See <a href="index.html"><code>index</code></a> and <a href="guide.html"><code>guide</code></a>.</p>

The env stanza of the directory makes odoc warnings fatal. This applies to a
page that is not renamed:

  $ cat >> doc/other.mld <<EOF
  > 
  > [broken
  > EOF
  $ dune build @doc
  File "../../../doc/other.mld", line 6, characters 0-0:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

  $ cat > doc/other.mld <<EOF
  > {0 Other}
  > EOF

And to a renamed page:

  $ cat >> doc/draft.mld <<EOF
  > 
  > [broken
  > EOF
  $ dune build @doc
  File "../../../doc/draft.mld", line 4, characters 0-0:
  Error: End of text is not allowed in '[...]' (code).
  ERROR: Warnings have been generated.
  [1]

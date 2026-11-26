A "mixed" dependency chain [p] -> [q] -> [r] has one edge crossing the
workspace/lockdir boundary. Only the packages listed in a package's
[(depends ...)] are searched for binaries, so a package whose binaries are
needed must be declared explicitly.

[p] (workspace) -> [q] (workspace) -> [r] (lockdir). [p] only lists [q], so
[r-tool] does not resolve from [p]'s stanza.

The lockdir package [r] is a Dune project installing [r-tool], built natively
or by an opaque recipe:

  $ mkdir -p _r native opaque
  $ (cd _r && make_dune_project_with_package 3.25 r)
  $ echo '(install (section bin) (files r-tool))' > _r/dune
  $ printf '#!/bin/sh\necho from r\n' > _r/r-tool
  $ chmod +x _r/r-tool
  $ make_backend_lockdirs r "$PWD/_r"

  $ mkdir -p p q
  $ cat >p/dune <<'EOF'
  > (rule (with-stdout-to r-avail (echo %{bin-available:r-tool})))
  > EOF
  $ cp -R p q native/ && cp -R p q opaque/
  $ check() (
  >   cd "$1"
  >   make_dune_project 3.25
  >   cat >> dune-project <<EOF
  > (package (name p) (allow_empty) (dir p) (depends $2))
  > (package (name q) (allow_empty) (dir q) (depends r))
  > EOF
  >   dune clean
  >   dune build p/r-avail
  >   echo "$(cat _build/default/p/r-avail)"
  > )

[r] is visible through the workspace package [q]:

  $ check native q
  true
  $ check opaque q
  true
  $ cat opaque/$pkg_root/r/_install/opaque-recipe
  opaque
  $ test ! -e native/$pkg_root/r/_install/opaque-recipe

Declaring [r] directly on [p] makes it visible:

  $ check native "q r"
  true
  $ check opaque "q r"
  true

Both blocks print [true]: [p] sees the dependencies of the packages it lists,
whether it reaches [r] through [q] or lists it directly.

[p] (workspace) -> [q] (lockdir) -> [r] (workspace) is valid too. The opaque
recipe for [q] receives [r], and [r-tool] is in the dependency closure of [p].

  $ mkdir lockdir-middle
  $ cd lockdir-middle
  $ make_lockdir
  $ make_lockpkg q <<'EOF'
  > (version 0.0.1)
  > (depends r)
  > (build (system "true"))
  > EOF

  $ mkdir -p p r
  $ cat >r/r-tool.sh <<'EOF'
  > #!/bin/sh
  > echo from r
  > EOF
  $ chmod +x r/r-tool.sh
  $ cat >r/dune <<'EOF'
  > (install (package r) (section bin) (files (r-tool.sh as r-tool)))
  > EOF
  $ cat >p/dune <<'EOF'
  > (rule (with-stdout-to r-avail (echo %{bin-available:r-tool})))
  > EOF

  $ make_dune_project 3.25
  $ cat >> dune-project <<'EOF'
  > (package (name p) (allow_empty) (dir p) (depends q))
  > (package (name r) (allow_empty) (dir r))
  > EOF

  $ dune build p/r-avail
  $ cat _build/default/p/r-avail
  true

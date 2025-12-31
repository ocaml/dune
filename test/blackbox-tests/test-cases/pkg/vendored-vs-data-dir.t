Showcase the difference in Dune package management solving dependencies of
vendored directories and data directories. In both cases, the top level project
doesn't use the vendored packages as a dependency.

We create the necessary packages first

  $ mkrepo
  $ add_mock_repo_if_needed

  $ mkdir -p vendor/a vendor/b

  $ mkpkg lwt
  $ mkpkg eio
  $ mkpkg ocaml 5.2.0

The vendored packages have some dependencies respectively
  $ cat >vendor/a/dune-project << EOF
  > (lang dune 3.21)
  > (name a)
  > (package
  >  (name a)
  > (depends
  >  (ocaml (= 5.2.0))
  >  lwt))
  > EOF

  $ cat >vendor/b/dune-project << EOF
  > (lang dune 3.21)
  > (name b)
  > (package
  >  (name b)
  > (depends
  >  (ocaml (= 5.2.0))
  >  eio))
  > EOF



Now we're adding these as vendored directories.

  $ cat >dune <<EOF
  > (vendored_dirs vendor)
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (package
  > (name hello)
  > (allow_empty)
  > (depends
  >  (ocaml
  >  (= 5.2.0))))
  > EOF

If we inspect the locking, we find that it also pulls in the dependencies in the
vendor/* directories. This is because dune pkg scans all the sub directories for
dependencies.

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - eio.0.0.1
  - lwt.0.0.1
  - ocaml.5.2.0


Now, let's treat the vendor packages as `data_only_dirs`. This means dune won't
scan the vendored directories for its dependencies.

  $ cat >dune <<EOF
  > (data_only_dirs vendor)
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - ocaml.5.2.0

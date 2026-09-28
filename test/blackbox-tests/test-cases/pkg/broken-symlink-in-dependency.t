Test that dune can handle the case where a dependency's source contains a
symlink with a missing destination.

Define a package foo containing a broken symlink.
  $ mkdir foo
  $ touch foo/a.txt
  $ ln -s non_existent foo/b.txt

Define a package bar containing a broken symlink.
  $ mkdir bar
  $ touch bar/a.txt
  $ ln -s non_existent bar/b.txt
  $ tar czf bar.tar.gz bar

Make a directory to contain a test project and change to it.
  $ mkdir project
  $ cd project

Create a lockdir for the project.
  $ make_lockdir

The package "foo" exercises copying package sources from a local directory.
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build (run ls))
  > (source
  >  (fetch
  >   (url
  >    file:///$PWD/../foo)))
  > EOF

The package "bar" exercises extracting a source archive from a local file.
  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (build (run ls))
  > (source
  >  (fetch
  >   (url
  >    file:///$PWD/../bar.tar.gz)))
  > EOF

The package "bar" exercises extracting a source archive from a downloaded file.
  $ make_lockpkg baz <<EOF
  > (version 0.0.1)
  > (build (run ls))
  > (source
  >  (fetch
  >   (url http://0.0.0.0:1)
  >   (checksum md5=$(md5sum $PWD/../bar.tar.gz | cut -f1 -d' '))))
  > EOF

Set up a fake web server to serve the source archive for the package "bar".
  $ echo $PWD/../bar.tar.gz >> fake-curls

Make a project file that depends on all the packages.
  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends foo bar baz))
  > EOF

The recipe working directories contain all files except the broken symlinks:
  $ build_pkg foo
  a.txt
  $ build_pkg bar
  a.txt
  $ build_pkg baz
  a.txt

The immutable source snapshots exclude the broken symlinks too:
  $ find _build/_fetch -name b.txt
  $ find _build/_fetch -name a.txt | wc -l
  3

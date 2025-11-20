Test that dune pkg lock and build generate trace events.

  $ . ./helpers.sh
  $ mkrepo

Make a library to package:

  $ mkdir foo
  $ cd foo
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ cat > foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > dune <<EOF
  > (library (public_name foo))
  > EOF
  $ cd ..
  $ tar cf foo.tar foo
  $ rm -rf foo

Configure fake curl to serve the tarball:

  $ echo foo.tar >> fake-curls
  $ PORT=1

Create package with URL:

  $ mkpkg foo <<EOF
  > build: [
  >   ["dune" "build" "-p" name "-j" jobs]
  > ]
  > url {
  >  src: "http://0.0.0.0:$PORT"
  >  checksum: [
  >   "md5=$(md5sum foo.tar | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Make a simple package without URL:

  $ mkpkg bar

Make a simple library for bar package:

  $ mkdir bar_src
  $ cd bar_src
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name bar))
  > EOF
  $ cat > bar.ml <<EOF
  > let y = "bar"
  > EOF
  $ cat > dune <<EOF
  > (library (public_name bar))
  > EOF
  $ cd ..
  $ tar cf bar.tar bar_src
  $ rm -rf bar_src

Configure fake curl to serve bar tarball:

  $ echo bar.tar >> fake-curls

Update bar package to have URL:

  $ mkpkg bar <<EOF
  > build: [
  >   ["dune" "build" "-p" name "-j" jobs]
  > ]
  > url {
  >  src: "http://0.0.0.0:2"
  >  checksum: [
  >   "md5=$(md5sum bar.tar | cut -f1 -d' ')"
  >  ]
  > }
  > EOF

Create a project that depends on both packages:

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name myproject)
  >  (depends foo bar))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_endline (Foo.x ^ " " ^ Bar.y)
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name main)
  >  (libraries foo bar))
  > EOF

  $ add_mock_repo_if_needed

Lock the project with tracing enabled:
  $ export TRACE_FILE=lock-trace.json

  $ dune pkg lock --trace-file $TRACE_FILE
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - bar.0.0.1
  - foo.0.0.1

Checks how many times string occurs in trace.
  $ test() {
  >   grep -c "$1" "$TRACE_FILE"
  > }

The system should only be polled once.
  $ test '"cat":"sys_poll"'
  > test '"name":"make"'
  1
  1

Loading opam repo + overlays.
  $ test '"cat":"opam_repo"'
  > test '"name":"load_all_versions_by_keys"'
  8
  8

  $ test '"cat":"solver"'
  > test '"name":"repo_candidate"'
  > test '"name":"build_problem"'
  > test '"name":"solve_package_list"'
  20
  8
  4
  4

Writing the lock dir.
  $ test '"cat":"lock_dir"'
  > test '"name":"write_lock_dir"'
  1
  1

  $ rm $TRACE_FILE

Now build with tracing to check fetch and archive events:

  $ dune build --trace-file $TRACE_FILE main.exe

  $ test '"cat":"lock_dir"'
  > test '"name":"load_lock_dir"'
  7
  7

  $ test '"cat":"fetch"'
  > test '"name":"dune-fetch"'
  > test '"name":"extract"'
  4
  2
  2

  $ dune exec ./main.exe
  foo bar

Fetch from more than one source

  $ . ./helpers.sh

  $ make_lockdir
  $ mkdir foo
  $ cat >foo/bar <<EOF
  > this is bar
  > EOF

  $ cat >baz <<EOF
  > this is baz
  > EOF

  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/foo))
  > (extra_sources (mybaz (copy $PWD/baz)))
  > (build
  >  (system "find . | sort -u"))
  > EOF

  $ build_pkg test
  .
  ./bar
  ./mybaz

Make sure extra source patches are downloaded, checksum-verified and applied
when building.

First we need a project that will have the patch applied:

  $ mkdir needs-patch 
  $ cd needs-patch
  $ git init --quiet
  $ cat > dune-project <<EOF
  > (lang dune 3.15)
  > EOF
  $ cat > needs_patch.opam <<EOF
  > opam-version: "2.0"
  > EOF
  $ cat > dune <<EOF
  > (library (public_name needs_patch))
  > EOF
  $ cat > needs_patch.ml <<EOF
  > let msg = "Needs to be patched"
  > EOF
  $ git add -A
  $ git commit -m "Initial" --quiet
  $ cd ..
  $ tar cf needs-patch.tar needs-patch
  $ SRC_MD5=$(md5sum needs-patch.tar | cut -f1 -d' ')
  $ cd needs-patch
  $ cat > needs_patch.ml <<EOF
  > let msg = "Patch successfully applied"
  > EOF
  $ git diff > ../required.patch
  $ cd ..
  $ PATCH_MD5=$(md5sum required.patch | cut -f1 -d' ')

Then we start the oneshot server for both the source and the patch.

  $ webserver_oneshot --content-file needs-patch.tar --port-file tarball-port.txt &
  $ until test -f tarball-port.txt ; do sleep 0.1; done
  $ SRC_PORT=$(cat tarball-port.txt)
  $ webserver_oneshot --content-file required.patch --port-file patch-port.txt &
  $ until test -f patch-port.txt ; do sleep 0.1; done
  $ PATCH_PORT=$(cat patch-port.txt)

We now have the checksums as well as the port numbers, so we can define the
package.

  $ mkrepo
  $ mkpkg needs-patch 0.0.1 <<EOF
  > patches: ["required.patch"]
  > url {
  >   src: "http://localhost:$SRC_PORT"
  >   checksum: "md5=$SRC_MD5"
  > }
  > extra-source "required.patch" {
  >   src: "http://localhost:$PATCH_PORT"
  >   checksum: "md5=$PATCH_MD5"
  > }
  > EOF

Now let's depend on that project with a binary that will display the message
that is supposed to get patched.

  $ cat > dune-project <<EOF
  > (lang dune 3.15)
  > (package (name my) (depends needs-patch))
  > EOF
  $ cat > dune <<EOF
  > (executable (public_name display) (libraries needs_patch))
  > EOF
  $ cat > display.ml <<EOF
  > let () = print_endline Needs_patch.msg
  > EOF

Lock the dependency, it should generate an a lock dir that references both the
url and the extra source.

  $ add_mock_repo_if_needed
  $ dune pkg lock
  Solution for dune.lock:
  - needs-patch.0.0.1

Running the binary should download the tarball & patch, build them and show the
correct, patched, message:

  $ dune exec ./display.exe
  Error: Multiple rules generated for
  _build/_private/default/.pkg/needs-patch/source:
  - dune.lock/needs-patch.pkg:14
  - dune.lock/needs-patch.pkg:8
  [1]

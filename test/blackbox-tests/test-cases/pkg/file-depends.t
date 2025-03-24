Here we test the file-depends field in pkg1.config. When a package has been
installed, the .config file can also be included. This can have a file-depends
field which is a list of external files, together with their checksums, that
the package depends on. We make sure that such a package really does depend on
the files found in files-depend. 

  $ . ./helpers.sh
  $ make_lockdir

  $ foo=$PWD/foo
  > cat > dune.lock/file-depends.pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (system "\| echo Building file-depends
  >          "\| cat > file-depends.config <<EOF
  >          "\| opam-version: "2.0"
  >          "\| file-depends: [ "$foo" "md5=00000000000000000000000000000000" ]
  >          "\| EOF
  >  ))
  > EOF

Word of warning: the opam libraries will quietly discard the file-depends field if the
checksum is not parsable.

Now we make a package depending on file-depends.

  $ cat > dune.lock/dep.pkg <<EOF
  > (version 0.0.1)
  > (depends file-depends)
  > (build
  >  (system "echo Building dep"))
  > EOF

  $ cat > foo <<EOF
  > Hello
  > EOF

Building dep should show both of them as being built.

  $ build_pkg dep
  Building file-depends
  Building dep

Building again causes no rebuild as expected.

  $ build_pkg dep

Changing foo should cause foo to be rebuilt.

  $ cat > foo <<EOF
  > World
  > EOF

CR-someday alizter: This is broken, no rebuild is done.
  $ build_pkg dep

Removing foo should cause an error due to the missing file.

  $ rm foo

CR-someday alizter: This is broken, no rebuild is done.
  $ build_pkg dep

  $ . ../helpers.sh

These opam variables are known as "switch variables" in opam, but since in Dune we don't
have switches, we consider them glboal variables. To keep inline with opam we consider
there to be a single switch named "dune" and all the installation locations should be in
the package building directories.

Note that misc is not supported in dune and a test for it can be found in
opam-var-unsupported.t
  $ mkrepo
  $ mkpkg testpkg <<EOF
  > build: [
  >   [ "echo" switch ]
  >   [ "echo" build ]
  >   [ "echo" prefix ]
  >   [ "echo" lib ]
  >   [ "echo" libexec ]
  >   [ "echo" bin ]
  >   [ "echo" sbin ]
  >   [ "echo" share ]
  >   [ "echo" doc ]
  >   [ "echo" etc ]
  >   [ "echo" man ]
  >   [ "echo" toplevel ]
  >   [ "echo" stublibs ]
  > ]
  > EOF
  > solve testpkg
  Solution for .dune-solution-cache:
  - testpkg.0.0.1
  $ cat ${default_lock_dir}/testpkg.pkg
  (version 0.0.1)
  
  (build
   (progn
    (run echo %{switch})
    (run echo %{build})
    (run echo %{prefix})
    (run echo %{lib})
    (run echo %{libexec})
    (run echo %{bin})
    (run echo %{sbin})
    (run echo %{share})
    (run echo %{doc})
    (run echo %{etc})
    (run echo %{man})
    (run echo %{toplevel})
    (run echo %{stublibs})))

  $ build_pkg testpkg 2>&1 | sed -E 's#.*.sandbox/[^/]+#.sandbox/$SANDBOX#g'
  dune
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/source
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/lib
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/lib
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/bin
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/sbin
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/share
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/doc
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/etc
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/man
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/lib/toplevel
  .sandbox/$SANDBOX/_private/default/.pkg/testpkg.0.0.1-756c784cf1572138b72ad6859ef7f811/target/lib/stublibs

Test that installed binaries are visible in dependent packages

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<EOF
  > (build
  >  (system "\| echo "#!/bin/sh\necho from test package" > foo;
  >          "\| chmod +x foo;
  >          "\| touch libxxx lib_rootxxx;
  >          "\| cat >test.install <<EOF
  >          "\| bin: [ "foo" ]
  >          "\| lib: [ "libxxx" ]
  >          "\| lib_root: [ "lib_rootxxx" ]
  >          "\| share_root: [ "lib_rootxxx" ]
  >          "\| EOF
  >  ))
  > EOF

  $ cat >dune.lock/usetest <<EOF
  > (deps test)
  > (build
  >  (progn
  >   (run foo)
  >   (run mkdir -p %{prefix})))
  > EOF

  $ dune build .pkg/usetest/target/
  from test package

  $ find _build/default/.pkg/test/target | sort
  _build/default/.pkg/test/target
  _build/default/.pkg/test/target/bin
  _build/default/.pkg/test/target/bin/foo
  _build/default/.pkg/test/target/cookie
  _build/default/.pkg/test/target/lib
  _build/default/.pkg/test/target/lib/lib_rootxxx
  _build/default/.pkg/test/target/lib/test
  _build/default/.pkg/test/target/lib/test/libxxx
  _build/default/.pkg/test/target/share
  _build/default/.pkg/test/target/share/lib_rootxxx
  $ dune internal dump _build/default/.pkg/test/target/cookie
  { files =
      map
        { LIB : [ In_build_dir "default/.pkg/test/target/lib/test/libxxx" ]
        ; LIB_ROOT :
            [ In_build_dir "default/.pkg/test/target/lib/lib_rootxxx" ]
        ; BIN : [ In_build_dir "default/.pkg/test/target/bin/foo" ]
        ; SHARE_ROOT :
            [ In_build_dir "default/.pkg/test/target/share/lib_rootxxx" ]
        }
  ; variables = []
  }

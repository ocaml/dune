Testing install actions

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<'EOF'
  > (build (run true))
  > (install (system "echo foobar; mkdir -p %{lib}; touch %{lib}/xxx"))
  > EOF

  $ dune build .pkg/test/target/
  foobar

  $ find _build/default/.pkg/test/target | sort
  _build/default/.pkg/test/target
  _build/default/.pkg/test/target/bin
  _build/default/.pkg/test/target/cookie
  _build/default/.pkg/test/target/doc
  _build/default/.pkg/test/target/doc/test
  _build/default/.pkg/test/target/etc
  _build/default/.pkg/test/target/etc/test
  _build/default/.pkg/test/target/lib
  _build/default/.pkg/test/target/lib/stublibs
  _build/default/.pkg/test/target/lib/test
  _build/default/.pkg/test/target/lib/toplevel
  _build/default/.pkg/test/target/lib/xxx
  _build/default/.pkg/test/target/man
  _build/default/.pkg/test/target/sbin
  _build/default/.pkg/test/target/share
  _build/default/.pkg/test/target/share/test

  $ dune internal dump _build/default/.pkg/test/target/cookie
  { files =
      map
        { LIB_ROOT :
            [ { src = In_build_dir "default/.pkg/test/target/lib/xxx"
              ; kind = File
              ; dst = "xxx"
              ; section = LIB_ROOT
              }
            ]
        }
  ; variables = []
  }

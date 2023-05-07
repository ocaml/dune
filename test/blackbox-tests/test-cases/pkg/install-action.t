Testing install actions

  $ mkdir dune.lock
  $ cat >dune.lock/test <<'EOF'
  > (build (run true))
  > (install (system "echo foobar; mkdir -p %{lib}; touch %{lib}/xxx"))
  > EOF

  $ dune build .pkg/test/target/
  foobar

  $ find _build/default/.pkg/test/target | sort
  _build/default/.pkg/test/target
  _build/default/.pkg/test/target/cookie
  _build/default/.pkg/test/target/lib
  _build/default/.pkg/test/target/lib/xxx

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

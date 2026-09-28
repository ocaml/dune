Testing install actions

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install (system "echo foobar; mkdir -p %{lib}; touch %{lib}/xxx"))
  > EOF

  $ build_pkg test
  foobar

  $ show_pkg_targets test
  
  /bin
  /cookie
  /doc
  /doc/test
  /etc
  /etc/test
  /lib
  /lib/stublibs
  /lib/test
  /lib/toplevel
  /lib/xxx
  /man
  /sbin
  /share
  /share/test

  $ show_pkg_cookie test
  { files =
      [ (LIB_ROOT,
         [ In_build_dir "default/.lockfile/pkg/test/.opam/test/target/lib/xxx"
         ])
      ]
  ; variables = []
  }

An install manifest should take precedence over the install action's scanner.
Currently, executable permissions cause the scanner to classify the same file
as libexec, so the cookie incorrectly records it in both lib and libexec.

  $ make_lockpkg overlap <<'EOF'
  > (version 1.0)
  > (build
  >  (progn
  >   (write-file payload contents)
  >   (write-file overlap.install "lib: [ \"payload\" ]")))
  > (install
  >  (system "cp payload %{lib}/overlap/payload; chmod +x %{lib}/overlap/payload"))
  > EOF
  $ build_pkg overlap
  $ show_pkg_cookie overlap
  { files =
      [ (LIB,
         [ In_build_dir
             "_private/default/.pkg/overlap.1.0-$DIGEST/target/lib/overlap/payload"
         ])
      ; (LIBEXEC, [])
      ]
  ; variables = []
  }

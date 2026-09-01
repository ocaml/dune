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
      [ (LIB_ROOT, [ In_build_dir "_private/default/.pkg/test/target/lib/xxx" ])
      ]
  ; variables = []
  }

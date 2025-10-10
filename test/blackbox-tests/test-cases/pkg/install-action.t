Testing install actions

  $ . ./helpers.sh

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (install (system "echo foobar; mkdir -p %{lib}; touch %{lib}/xxx"))
  > EOF

  $ build_pkg test
  foobar

  $ export BUILD_PATH_PREFIX_MAP="/PKG_ROOT=test/target:$BUILD_PATH_PREFIX_MAP"

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
         [ In_build_dir
             "_private/default/.pkg/test.0.0.1-f6ed2ec1b5272dd0b899919ab4533208/target/lib/xxx"
         ])
      ]
  ; variables = []
  }

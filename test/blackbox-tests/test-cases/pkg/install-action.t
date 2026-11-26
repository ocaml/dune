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
      [ (LIB_ROOT, [ In_build_dir "default/.pkg/test/_install/lib/xxx" ]) ]
  ; variables = []
  }

An install manifest takes precedence over the install action's scanner.
Executable permissions must not cause a file declared in lib to also appear
in libexec.

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
         [ In_build_dir "default/.pkg/overlap/_install/lib/overlap/payload" ])
      ; (LIBEXEC, [])
      ]
  ; variables = []
  }

The same happens within one section: a file the manifest declares in bin and
the install action also copies there is recorded once. A dependency can still
use it.

  $ make_lockpkg twice <<'EOF'
  > (version 1.0)
  > (build
  >  (progn
  >   (write-file tool installed)
  >   (write-file twice.install "bin: [ \"tool\" {\"shared-tool\"} ]")))
  > (install
  >  (progn
  >   (run mkdir -p %{pkg-self:bin})
  >   (run cp tool %{pkg-self:bin}/shared-tool)))
  > EOF
  $ build_pkg twice
  $ show_pkg_cookie twice
  { files =
      [ (BIN, [ In_build_dir "default/.pkg/twice/_install/bin/shared-tool" ]) ]
  ; variables = []
  }
  $ make_lockpkg consumer <<'EOF'
  > (version 1.0)
  > (depends twice)
  > (build (run cat %{pkg:twice:bin}/shared-tool))
  > EOF
  $ build_pkg consumer
  installed

An install action may leave both "tool" and "tool.exe" in bin, as Windows
builds do. Once the extension is stripped for lookup, the two names coincide;
a dependent package's recipe must still be set up.

  $ make_lockpkg both <<'EOF'
  > (version 1.0)
  > (install (system "mkdir -p %{bin}; touch %{bin}/tool %{bin}/tool.exe"))
  > EOF
  $ make_lockpkg both-consumer <<'EOF'
  > (version 1.0)
  > (depends both)
  > (build (run echo ok))
  > EOF
  $ build_pkg both-consumer
  ok

Test that dependency section variables expand to correct relative paths and
that installed artifacts are accessible through those paths.

  $ make_lockdir

Create a dependency package "dep" that installs files into every section.

  $ make_lockpkg dep <<'EOF'
  > (version 0.0.1)
  > (install
  >  (progn
  >   (run mkdir -p %{lib})
  >   (run mkdir -p %{libexec})
  >   (run mkdir -p %{bin})
  >   (run mkdir -p %{sbin})
  >   (run mkdir -p %{share})
  >   (run mkdir -p %{etc})
  >   (run mkdir -p %{doc})
  >   (run mkdir -p %{man})
  >   (run mkdir -p %{toplevel})
  >   (run mkdir -p %{stublibs})
  >   (system
  >    "\| echo lib-data > %{lib}/lib-file
  >    "\| echo libexec-data > %{libexec}/libexec-file
  >    "\| echo bin-data > %{bin}/bin-file
  >    "\| echo sbin-data > %{sbin}/sbin-file
  >    "\| echo share-data > %{share}/share-file
  >    "\| echo etc-data > %{etc}/etc-file
  >    "\| echo doc-data > %{doc}/doc-file
  >    "\| echo man-data > %{man}/man-file
  >    "\| echo toplevel-data > %{toplevel}/toplevel-file
  >    "\| echo stublibs-data > %{stublibs}/stublibs-file
  >   )))
  > EOF

Section variables expanded via (system) produce relative paths:

  $ make_lockpkg system-consumer <<'EOF'
  > (version 0.0.1)
  > (depends dep)
  > (build
  >  (progn
  >   (system
  >    "\| echo %{pkg:dep:lib}
  >    "\| echo %{pkg:dep:libexec}
  >    "\| echo %{pkg:dep:bin}
  >    "\| echo %{pkg:dep:sbin}
  >    "\| echo %{pkg:dep:share}
  >    "\| echo %{pkg:dep:etc}
  >    "\| echo %{pkg:dep:doc}
  >    "\| echo %{pkg:dep:man}
  >    "\| echo %{pkg:dep:toplevel}
  >    "\| echo %{pkg:dep:stublibs}
  >   )
  >   (system
  >    "\| cat %{pkg:dep:lib}/lib-file
  >    "\| cat %{pkg:dep:bin}/bin-file
  >    "\| cat %{pkg:dep:sbin}/sbin-file
  >    "\| cat %{pkg:dep:share}/share-file
  >    "\| cat %{pkg:dep:etc}/etc-file
  >    "\| cat %{pkg:dep:doc}/doc-file
  >    "\| cat %{pkg:dep:man}/man-file
  >    "\| cat %{pkg:dep:toplevel}/toplevel-file
  >    "\| cat %{pkg:dep:stublibs}/stublibs-file
  >   )))
  > EOF

  $ build_pkg system-consumer 2>&1 | censor
  ../../dep.0.0.1-$DIGEST/target/lib/dep
  ../../dep.0.0.1-$DIGEST/target/lib/dep
  ../../dep.0.0.1-$DIGEST/target/bin
  ../../dep.0.0.1-$DIGEST/target/sbin
  ../../dep.0.0.1-$DIGEST/target/share/dep
  ../../dep.0.0.1-$DIGEST/target/etc/dep
  ../../dep.0.0.1-$DIGEST/target/doc/dep
  ../../dep.0.0.1-$DIGEST/target/man
  ../../dep.0.0.1-$DIGEST/target/lib/toplevel
  ../../dep.0.0.1-$DIGEST/target/lib/stublibs
  bin-data
  sbin-data
  man-data
  toplevel-data
  stublibs-data
  cat: ../../dep.0.0.1-$DIGEST/target/lib/dep/lib-file: No such file or directory
  cat: ../../dep.0.0.1-$DIGEST/target/share/dep/share-file: No such file or directory
  cat: ../../dep.0.0.1-$DIGEST/target/etc/dep/etc-file: No such file or directory
  cat: ../../dep.0.0.1-$DIGEST/target/doc/dep/doc-file: No such file or directory

Section variables used as standalone pform args in (run) actions, as produced
by opam translation of [ "echo" dep:lib ] to (run echo %{pkg:dep:lib}).
These currently expand to absolute paths instead of relative ones:

  $ make_lockpkg run-consumer <<'EOF'
  > (version 0.0.1)
  > (depends dep)
  > (build
  >  (progn
  >   (run echo %{pkg:dep:lib})
  >   (run echo %{pkg:dep:libexec})
  >   (run echo %{pkg:dep:bin})
  >   (run echo %{pkg:dep:sbin})
  >   (run echo %{pkg:dep:share})
  >   (run echo %{pkg:dep:etc})
  >   (run echo %{pkg:dep:doc})
  >   (run echo %{pkg:dep:man})
  >   (run echo %{pkg:dep:toplevel})
  >   (run echo %{pkg:dep:stublibs})
  >   (run cat %{pkg:dep:lib}/lib-file)
  >   (run cat %{pkg:dep:bin}/bin-file)
  >   (run cat %{pkg:dep:sbin}/sbin-file)
  >   (run cat %{pkg:dep:share}/share-file)
  >   (run cat %{pkg:dep:etc}/etc-file)
  >   (run cat %{pkg:dep:doc}/doc-file)
  >   (run cat %{pkg:dep:man}/man-file)
  >   (run cat %{pkg:dep:toplevel}/toplevel-file)
  >   (run cat %{pkg:dep:stublibs}/stublibs-file)))
  > EOF

  $ build_pkg run-consumer 2>&1 | strip_sandbox | censor | dune_cmd subst '/[^ ]*/cat:' 'cat:'
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/lib/dep
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/lib/dep
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/bin
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/sbin
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/share/dep
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/etc/dep
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/doc/dep
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/man
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/lib/toplevel
  $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/lib/stublibs
  File "dune.lock/run-consumer.pkg", line 15, characters 7-10:
  15 |   (run cat %{pkg:dep:lib}/lib-file)
              ^^^
  Error: Logs for package run-consumer
  cat: $SANDBOX/_private/default/.pkg/dep.0.0.1-$DIGEST/target/lib/dep/lib-file: No such file or directory
  
  [1]

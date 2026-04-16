Test that dependency section variables expand to correct relative paths and
that installed artifacts are accessible through those paths.

Per the opam spec, lib, libexec, share, etc, doc append the package name
(e.g. lib -> <prefix>/lib/<pkg>), while bin, sbin, man, toplevel, stublibs
do not.

  $ make_lockdir

Create a dependency package "dep" that installs files into every section.

  $ make_lockpkg dep <<'EOF'
  > (version 0.0.1)
  > (install
  >  (progn
  >   (run mkdir -p %{pkg-self:lib})
  >   (run mkdir -p %{pkg-self:libexec})
  >   (run mkdir -p %{pkg-self:bin})
  >   (run mkdir -p %{pkg-self:sbin})
  >   (run mkdir -p %{pkg-self:share})
  >   (run mkdir -p %{pkg-self:etc})
  >   (run mkdir -p %{pkg-self:doc})
  >   (run mkdir -p %{pkg-self:man})
  >   (run mkdir -p %{pkg-self:toplevel})
  >   (run mkdir -p %{pkg-self:stublibs})
  >   (system
  >    "\| echo lib-data > %{pkg-self:lib}/lib-file
  >    "\| echo libexec-data > %{pkg-self:libexec}/libexec-file
  >    "\| echo bin-data > %{pkg-self:bin}/bin-file
  >    "\| echo sbin-data > %{pkg-self:sbin}/sbin-file
  >    "\| echo share-data > %{pkg-self:share}/share-file
  >    "\| echo etc-data > %{pkg-self:etc}/etc-file
  >    "\| echo doc-data > %{pkg-self:doc}/doc-file
  >    "\| echo man-data > %{pkg-self:man}/man-file
  >    "\| echo toplevel-data > %{pkg-self:toplevel}/toplevel-file
  >    "\| echo stublibs-data > %{pkg-self:stublibs}/stublibs-file
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
  >    "\| cat %{pkg:dep:libexec}/libexec-file
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
  lib-data
  libexec-data
  bin-data
  sbin-data
  share-data
  etc-data
  doc-data
  man-data
  toplevel-data
  stublibs-data

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
  >   (run cat %{pkg:dep:libexec}/libexec-file)
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
  lib-data
  libexec-data
  bin-data
  sbin-data
  share-data
  etc-data
  doc-data
  man-data
  toplevel-data
  stublibs-data

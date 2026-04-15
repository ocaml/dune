Test that %{pkg:foo:...} section variables expand to the correct install
paths for a local workspace package.

Per the opam convention, lib, libexec, share, etc, doc append the package
name as a subdirectory, while bin, sbin, man, toplevel, stublibs do not.

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (package (name foo))
  > EOF

  $ mkdir foo

  $ cat >foo/dune <<EOF
  > (install
  >  (section share)
  >  (package foo)
  >  (files (data.txt as data.txt)))
  > EOF

  $ cat >foo/data.txt <<EOF
  > some data
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (alias test-paths)
  >  (action
  >   (echo
  >    "\| lib:          %{pkg:foo:lib}
  >    "\| lib_root:     %{pkg:foo:lib_root}
  >    "\| libexec:      %{pkg:foo:libexec}
  >    "\| libexec_root: %{pkg:foo:libexec_root}
  >    "\| bin:          %{pkg:foo:bin}
  >    "\| sbin:         %{pkg:foo:sbin}
  >    "\| share:        %{pkg:foo:share}
  >    "\| share_root:   %{pkg:foo:share_root}
  >    "\| etc:          %{pkg:foo:etc}
  >    "\| doc:          %{pkg:foo:doc}
  >    "\| man:          %{pkg:foo:man}
  >    "\| toplevel:     %{pkg:foo:toplevel}
  >    "\| stublibs:     %{pkg:foo:stublibs}
  >   )))
  > EOF

  $ dune build @test-paths 2>&1
  lib:          ../install/default/lib/foo
  lib_root:     ../install/default/lib
  libexec:      ../install/default/lib/foo
  libexec_root: ../install/default/lib
  bin:          ../install/default/bin
  sbin:         ../install/default/sbin
  share:        ../install/default/share/foo
  share_root:   ../install/default/share
  etc:          ../install/default/etc/foo
  doc:          ../install/default/doc/foo
  man:          ../install/default/man
  toplevel:     ../install/default/lib/toplevel
  stublibs:     ../install/default/lib/stublibs

The expansion registers a dependency on the package install alias:

  $ cat >dune <<EOF
  > (rule
  >  (target pkg-test-output)
  >  (action (with-stdout-to %{target} (echo %{pkg:foo:share}))))
  > EOF

  $ dune rules --deps _build/default/pkg-test-output 2>&1 | grep -i alias
  ((Alias ((dir (In_build_dir _build/default)) (name .foo-files))))

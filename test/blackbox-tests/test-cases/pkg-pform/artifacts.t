Test that %{pkg:...} resolves through install renames across all sections
and tracks source files as dependencies.

  $ make_dune_project_with_package 3.24 foo

  $ mkdir foo

  $ for section in lib lib_root libexec libexec_root bin sbin toplevel share share_root etc doc stublibs man; do
  >   cat >>foo/dune <<EOF
  > (install (package foo) (section $section) (files (${section}_src as ${section}_dst)))
  > EOF
  >   echo "$section content" > foo/${section}_src
  > done

Each section resolves the install name back to the source file:

  $ cat >dune <<'EOF'
  > (rule
  >  (target out)
  >  (action
  >   (with-stdout-to %{target}
  >    (progn
  >     (cat %{pkg:foo:lib:lib_dst})
  >     (cat %{pkg:foo:lib_root:lib_root_dst})
  >     (cat %{pkg:foo:libexec:libexec_dst})
  >     (cat %{pkg:foo:libexec_root:libexec_root_dst})
  >     (cat %{pkg:foo:bin:bin_dst})
  >     (cat %{pkg:foo:sbin:sbin_dst})
  >     (cat %{pkg:foo:toplevel:toplevel_dst})
  >     (cat %{pkg:foo:share:share_dst})
  >     (cat %{pkg:foo:share_root:share_root_dst})
  >     (cat %{pkg:foo:etc:etc_dst})
  >     (cat %{pkg:foo:doc:doc_dst})
  >     (cat %{pkg:foo:stublibs:stublibs_dst})
  >     (cat %{pkg:foo:man:man_dst})))))
  > EOF

  $ dune build out 2>&1
  $ cat _build/default/out
  lib content
  lib_root content
  libexec content
  libexec_root content
  bin content
  sbin content
  toplevel content
  share content
  share_root content
  etc content
  doc content
  stublibs content
  man content

All source files appear as dependencies (not install staging paths):

  $ dune rules --format=json _build/default/out 2>&1 | jq_dune '.[] | ruleDepFilePaths' | sort
  "_build/default/foo/bin_src"
  "_build/default/foo/doc_src"
  "_build/default/foo/etc_src"
  "_build/default/foo/lib_root_src"
  "_build/default/foo/lib_src"
  "_build/default/foo/libexec_root_src"
  "_build/default/foo/libexec_src"
  "_build/default/foo/man_src"
  "_build/default/foo/sbin_src"
  "_build/default/foo/share_root_src"
  "_build/default/foo/share_src"
  "_build/default/foo/stublibs_src"
  "_build/default/foo/toplevel_src"

Library-file pforms use the installed names of a public sublibrary's files.
The libexec section shares its library directory and is reachable through
both lib and libexec pforms. Reuse one provider and consumer through workspace,
native and opaque installations.

  $ mkdir foo/support _provider native opaque
  $ cat > foo/support/dune <<'EOF'
  > (library (name support) (public_name foo.support))
  > (install (section lib) (files (data as support/renamed.txt)))
  > (install (section libexec) (files (data as support/executable-data)))
  > EOF
  $ echo 'let x = ()' > foo/support/support.ml
  $ echo installed-name > foo/support/data
  $ cat > library-rules <<'EOF'
  > (rule
  >  (target renamed)
  >  (action (copy %{lib:foo.support:renamed.txt} %{target})))
  > (rule
  >  (target libexec-data)
  >  (action (copy %{libexec:foo.support:executable-data} %{target})))
  > (rule
  >  (target lib-data)
  >  (action (copy %{lib:foo.support:executable-data} %{target})))
  > EOF
  $ cat library-rules >> dune
  $ dune build renamed libexec-data lib-data
  $ cat _build/default/renamed _build/default/libexec-data \
  >   _build/default/lib-data
  installed-name
  installed-name
  installed-name

Use the complete producer and the same pforms with both locked backends.
The contents agree, but the native rule depends on the mounted source
while the opaque rule depends on the installed file.

  $ cp dune-project _provider/dune-project
  $ cp -R foo _provider/
  $ (cd native && make_named_package_project consumer 3.24 foo)
  $ (cd opaque && make_named_package_project consumer 3.24 foo)
  $ cp dune native/dune
  $ cp dune opaque/dune
  $ make_backend_lockdirs foo "$PWD/_provider"
  $ (cd native && dune build out renamed libexec-data lib-data)
  $ cmp _build/default/out native/_build/default/out
  $ cat native/_build/default/renamed native/_build/default/libexec-data \
  >   native/_build/default/lib-data
  installed-name
  installed-name
  installed-name
  $ test ! -e native/_build/default/.pkg/foo/_install/cookie
  $ (cd native && dune rules --format=json _build/default/out) \
  >   | jq_dune '.[] | ruleDepFilePaths | select(test("/share_(src|dst)$"))'
  "_build/default/.pkg/foo/foo/share_src"
  $ (cd opaque && dune build out renamed libexec-data lib-data)
  $ cmp _build/default/out opaque/_build/default/out
  $ cat opaque/_build/default/renamed opaque/_build/default/libexec-data \
  >   opaque/_build/default/lib-data
  installed-name
  installed-name
  installed-name
  $ cat opaque/_build/default/.pkg/foo/_install/opaque-recipe
  opaque
  $ test -f opaque/_build/default/.pkg/foo/_install/cookie
  $ (cd opaque && dune rules --format=json _build/default/out) \
  >   | jq_dune '.[] | ruleDepFilePaths | select(test("/share_(src|dst)$"))'
  "_build/default/.pkg/foo/_install/share/foo/share_dst"

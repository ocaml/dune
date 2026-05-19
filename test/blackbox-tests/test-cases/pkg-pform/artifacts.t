Test that %{pkg:...} resolves through install renames across all sections
and tracks source files as dependencies.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name foo))
  > EOF

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

  $ dune rules --format=json _build/default/out 2>&1 | jq 'include "dune"; .[] | ruleDepFilePaths' | sort
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

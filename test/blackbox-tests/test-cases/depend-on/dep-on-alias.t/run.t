
  $ mkdir a
  $ cd a

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > EOF

  $ echo old-contents > x

  $ cat >dune <<EOF
  > (alias
  >   (name a)
  >   (deps x)
  > )
  > (rule
  >   (alias b)
  >   (deps (alias a))
  >   (action (bash "echo -n \"running b: \"; cat x"))
  > )
  > (rule
  >   (deps (alias a))
  >   (action (progn (bash "echo -n \"running b: \"; cat x") (with-stdout-to b (bash "cat x"))))
  > )
  > EOF

  $ dune build @b
          bash alias b
  running b: old-contents
  $ dune build @b
  $ echo new-contents > x
  $ dune build @b

^ Bug: dune does not re-run the action even though
its declared dependencies changed.

Nor does the path appear in the sandbox:

  $ dune build @b --sandbox copy |& grep -v 'cd _build/.sandbox'
          bash alias b (exit 1)
  running b: cat: x: No such file or directory

In fact none of the alias stamp files even change
when this dependency changes, so alias stamp files are useless here,
even if we depended on them:

  $ echo old-contents > x
  $ dune build @b &> /dev/null
  $ md5sum _build/.aliases/default/* > stamp-files-old
  $ rm -r _build 

  $ echo new-contents > x
  $ dune build @b &> /dev/null
  $ md5sum _build/.aliases/default/* > stamp-files-new
  $ rm -r _build

  $ cat stamp-files-old
  ba4d257a2d76880811986a1120e3d1ea  _build/.aliases/default/a-00000000000000000000000000000000
  d41d8cd98f00b204e9800998ecf8427e  _build/.aliases/default/a-ff67d857b9b4f3d2e7bb31b302aa5bc4
  9425f8e768d73fcee0cf05928f737277  _build/.aliases/default/b-00000000000000000000000000000000
  d41d8cd98f00b204e9800998ecf8427e  _build/.aliases/default/b-de1ad9f2f2db598914453ffb73cfb3a2

  $ diff stamp-files-old stamp-files-new

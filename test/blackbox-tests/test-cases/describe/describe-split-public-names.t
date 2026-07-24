The `--split-public-names` flag makes `dune describe` report private
names in the `name`/`names` fields and public names in a separate
`public_name`/`public_names` field, rather than reporting the public name
(falling back to the private name) in `name`/`names`.
============================================================================

  $ make_dune_project_with_package 3.21 mypkg

  $ mkdir bin lib helper priv

Executables with a mix of public and private names, depending on a public and
a private library:

  $ cat >bin/dune <<EOF
  > (executables
  >  (names a b)
  >  (public_names x -)
  >  (package mypkg)
  >  (libraries mypkg.mylib privlib))
  > EOF
  $ touch bin/a.ml bin/b.ml

A public library, depending on another public library:

  $ cat >lib/dune <<EOF
  > (library
  >  (name mylib)
  >  (public_name mypkg.mylib)
  >  (libraries mypkg.helper))
  > EOF
  $ touch lib/mylib.ml

  $ cat >helper/dune <<EOF
  > (library
  >  (name helper)
  >  (public_name mypkg.helper))
  > EOF
  $ touch helper/helper.ml

A private library (no public name):

  $ cat >priv/dune <<EOF
  > (library
  >  (name privlib))
  > EOF
  $ touch priv/privlib.ml

`dune describe workspace`
-------------------------

With the flag, private names are reported in `names`/`name` and public names in
`public_names`/`public_name`. Executables without a public name (here `b`) get a
`()` entry, and a private library (here `privlib`) gets an empty `public_name`:

  $ dune describe workspace --split-public-names --sanitize-for-tests | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (executables
    ((names
      (a b))
     (public_names
      ((x) ()))
     (requires
      ($DIGEST1 $DIGEST2))
     (modules
      (((name B)
        (impl (_build/default/bin/b.ml))
        (intf ())
        (cmt (_build/default/bin/.a.eobjs/byte/dune__exe__B.cmt))
        (cmti ()))
       ((name A)
        (impl (_build/default/bin/a.ml))
        (intf ())
        (cmt (_build/default/bin/.a.eobjs/byte/dune__exe__A.cmt))
        (cmti ()))
       ((name Dune__exe)
        (impl (_build/default/bin/.a.eobjs/dune__exe.ml-gen))
        (intf ())
        (cmt (_build/default/bin/.a.eobjs/byte/dune__exe.cmt))
        (cmti ()))))
     (include_dirs (_build/default/bin/.a.eobjs/byte))))
   (library
    ((name helper)
     (public_name (mypkg.helper))
     (uid $DIGEST3)
     (local true)
     (requires ())
     (source_dir _build/default/helper)
     (modules
      (((name Helper)
        (impl (_build/default/helper/helper.ml))
        (intf ())
        (cmt (_build/default/helper/.helper.objs/byte/helper.cmt))
        (cmti ()))))
     (include_dirs (_build/default/helper/.helper.objs/byte))))
   (library
    ((name mylib)
     (public_name (mypkg.mylib))
     (uid $DIGEST1)
     (local true)
     (requires ($DIGEST3))
     (source_dir _build/default/lib)
     (modules
      (((name Mylib)
        (impl (_build/default/lib/mylib.ml))
        (intf ())
        (cmt (_build/default/lib/.mylib.objs/byte/mylib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/lib/.mylib.objs/byte))))
   (library
    ((name privlib)
     (public_name ())
     (uid $DIGEST2)
     (local true)
     (requires ())
     (source_dir _build/default/priv)
     (modules
      (((name Privlib)
        (impl (_build/default/priv/privlib.ml))
        (intf ())
        (cmt (_build/default/priv/.privlib.objs/byte/privlib.cmt))
        (cmti ()))))
     (include_dirs (_build/default/priv/.privlib.objs/byte)))))

`dune describe external-lib-deps`
---------------------------------

Without the flag, the public name (falling back to the private name) is used in
`names`:

  $ dune describe external-lib-deps
  (default
   ((library
     ((names (mypkg.mylib))
      (extensions ())
      (package (mypkg))
      (source_dir lib)
      (external_deps ())
      (internal_deps ((mypkg.helper required)))))
    (executables
     ((names
       (x b))
      (extensions (.exe))
      (package (mypkg))
      (source_dir bin)
      (external_deps ())
      (internal_deps
       ((mypkg.mylib required)
        (privlib required)))))))

With the flag, private names are reported in `names` and public names in
`public_names`:

  $ dune describe external-lib-deps --split-public-names
  (default
   ((library
     ((names (mylib))
      (public_names ((mypkg.mylib)))
      (extensions ())
      (package (mypkg))
      (source_dir lib)
      (external_deps ())
      (internal_deps ((mypkg.helper required)))))
    (executables
     ((names
       (a b))
      (public_names
       ((x) ()))
      (extensions (.exe))
      (package (mypkg))
      (source_dir bin)
      (external_deps ())
      (internal_deps
       ((mypkg.mylib required)
        (privlib required)))))))

Test embedding of build information
-----------------------------------

  $ mkdir -p a b c

  $ for i in a b c; do
  >   mkdir -p $i
  >   cat >$i/dune-project <<EOF
  > (lang dune 1.11)
  > (name $i)
  > (package (name $i))
  > EOF
  >   (cd $i;
  >    git init -q;
  >    git add .;
  >    git commit -q -m _;
  >    git tag -a 1.0+$i -m _)
  > done

  $ for i in a b; do
  >   cat >$i/dune <<EOF
  > (library
  >  (public_name $i))
  > EOF
  > done

  $ cat >c/dune <<EOF
  > (executable
  >  (public_name c)
  >  (libraries a b dune.build-info))
  > EOF

  $ cat >c/c.ml <<EOF
  > module D = Dune_build_info.V1
  > let pr fmt = Printf.printf (fmt ^^ "\n")
  > let () =
  >   pr "%s" D.version;
  >   let process_lib lib =
  >     pr "lib %s: %s" (D.Statically_linked_library.name lib)
  >       (D.Statically_linked_library.version lib)
  >   in
  >   List.iter process_lib D.statically_linked_libraries
  > EOF

  $ dune build
  $ dune install --prefix _install
  Installing _install/lib/a/META
  Installing _install/lib/a/a$ext_lib
  Installing _install/lib/a/a.cma
  Installing _install/lib/a/a.cmi
  Installing _install/lib/a/a.cmt
  Installing _install/lib/a/a.cmx
  Installing _install/lib/a/a.cmxa
  Installing _install/lib/a/a.cmxs
  Installing _install/lib/a/a.ml
  Installing _install/lib/a/dune-package
  Installing _install/lib/b/META
  Installing _install/lib/b/b$ext_lib
  Installing _install/lib/b/b.cma
  Installing _install/lib/b/b.cmi
  Installing _install/lib/b/b.cmt
  Installing _install/lib/b/b.cmx
  Installing _install/lib/b/b.cmxa
  Installing _install/lib/b/b.cmxs
  Installing _install/lib/b/b.ml
  Installing _install/lib/b/dune-package
  Installing _install/lib/c/META
  Installing _install/lib/c/dune-package
  Installing _install/bin/c

Inside _build, we have no version information:

  $ _build/default/c/c.exe | sed -E 's/(dune.build-info: ).*/\1XXX/'
  n/a
  lib a: n/a
  lib b: n/a
  lib dune.build-info: XXX

  $ grep version _build/install/default/lib/a/dune-package
  [1]

  $ grep version _build/install/default/lib/a/META
  [1]

Once installed, we have the version information:

  $ _install/bin/c | sed -E 's/(dune.build-info: ).*/\1XXX/'
  1.0+c
  lib a: 1.0+a
  lib b: 1.0+b
  lib dune.build-info: XXX

  $ grep version _install/lib/a/dune-package
  (version 1.0+a)

  $ grep version _install/lib/a/META
  version = "1.0+a"

Check what the generated build info module looks like:

  $ cat _build/default/c/.c.eobjs/dune_build_info_data.ml | \
  >    sed -E 's/("dune.build-info", ).*/\1XXX/'
  let p1 = ref "%%DUNE_PLACEHOLDER:64:vcs-describe:1:a%%%%%%%%%%%%%%%%%%%%%%%%%%"
  let p2 = ref "%%DUNE_PLACEHOLDER:64:vcs-describe:1:b%%%%%%%%%%%%%%%%%%%%%%%%%%"
  let p0 = ref "%%DUNE_PLACEHOLDER:64:vcs-describe:1:c%%%%%%%%%%%%%%%%%%%%%%%%%%"
  
  let placeholders =
    [ p1
    ; p2
    ; p0
    ]
  type value =
    | Unset
    | Direct of string
    | Placeholder of string ref
  
  let version = Placeholder p0
  
  let statically_linked_libraries =
    [ "a", Placeholder p1
    ; "b", Placeholder p2
    ; "dune.build-info", XXX
    ]

Test substitution when promoting
--------------------------------

  $ cat >> c/dune <<EOF
  > (rule
  >  (targets d.exe)
  >  (mode promote)
  >  (action (copy c.exe d.exe)))
  > EOF

  $ dune build c/d.exe

  $ c/d.exe | sed -E 's/(dune.build-info: ).*/\1XXX/'
  1.0+c
  lib a: 1.0+a
  lib b: 1.0+b
  lib dune.build-info: XXX

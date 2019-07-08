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
  >  (promote (until-clean))
  >  (libraries a b dune-build-info))
  > EOF

  $ cat >c/c.ml <<EOF
  > module B = Build_info.V1
  > let pr fmt = Printf.printf (fmt ^^ "\n")
  > let get_version = function
  >   | Some v -> B.Version.to_string v
  >   | None -> "n/a"
  > let () =
  >   pr "%s" (get_version (B.version ()));
  >   let process_lib lib =
  >     let name = B.Statically_linked_library.name lib in
  >     let version = B.Statically_linked_library.version lib in
  >     pr "lib %s: %s" name (get_version version)
  >   in
  >   List.iter process_lib (B.Statically_linked_libraries.to_list ())
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

  $ _build/default/c/c.exe | sed 's/build-info: .*/build-info: XXX/'
  n/a
  lib a: n/a
  lib b: n/a
  lib dune-build-info: XXX

  $ grep version _build/install/default/lib/a/dune-package
  [1]

  $ grep version _build/install/default/lib/a/META
  [1]

Once installed, we have the version information:

  $ _install/bin/c | sed 's/build-info: .*/build-info: XXX/'
  1.0+c
  lib a: 1.0+a
  lib b: 1.0+b
  lib dune-build-info: XXX

  $ grep version _install/lib/a/dune-package
  (version 1.0+a)

  $ grep version _install/lib/a/META
  version = "1.0+a"

Check what the generated build info module looks like:

  $ cat _build/default/c/.c.eobjs/build_info_data.ml-gen \
  >   | sed 's/"dune-build-info".*/"dune-build-info", Some "XXX"/'
  let eval s =
    let len = String.length s in
    if s.[0] = '=' then
      let colon_pos = String.index_from s 1 ':' in
      let vlen = int_of_string (String.sub s 1 (colon_pos - 1)) in
      (* This [min] is because the value might have been truncated
         if it was too large *)
      let vlen = min vlen (len - colon_pos - 1) in
      Some (String.sub s (colon_pos + 1) vlen)
    else
      None
  [@@inline never]
  
  let p1 = eval "%%DUNE_PLACEHOLDER:64:vcs-describe:1:a%%%%%%%%%%%%%%%%%%%%%%%%%%"
  let p2 = eval "%%DUNE_PLACEHOLDER:64:vcs-describe:1:b%%%%%%%%%%%%%%%%%%%%%%%%%%"
  let p0 = eval "%%DUNE_PLACEHOLDER:64:vcs-describe:1:c%%%%%%%%%%%%%%%%%%%%%%%%%%"
  
  let version = p0
  
  let statically_linked_libraries =
    [ "a", p1
    ; "b", p2
    ; "dune-build-info", Some "XXX"
    ]

Test substitution when promoting
--------------------------------

  $ c/c.exe | sed 's/build-info: .*/build-info: XXX/'
  1.0+c
  lib a: 1.0+a
  lib b: 1.0+b
  lib dune-build-info: XXX

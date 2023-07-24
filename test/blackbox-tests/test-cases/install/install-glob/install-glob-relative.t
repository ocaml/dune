Install a glob pattern that uses a relative path.

When we use a pattern such as ../foo/*, the relative path leaks into the
installation destination and can escape the root of the installed package.

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

  $ mkdir -p stanza stuff/xy
  $ touch stuff/foo.txt stuff/xy/bar.txt

normal install stanza in the share directory of the package:

  $ cat >dune <<EOF
  > (install
  >  (section share)
  >  (files stuff/foo.txt))
  > EOF

faulty stanza that install things outside the package:

  $ cat >stanza/dune <<EOF
  > (install
  >  (files (glob_files_rec ../stuff/*.txt))
  >  (section share))
  > EOF

  $ dune build foo.install

Note that the "stuff" paths from  are now going to be installed outside the
package.

  $ grep txt _build/default/foo.install
    "_build/install/default/share/stuff/foo.txt" {"../stuff/foo.txt"}
    "_build/install/default/share/stuff/xy/bar.txt" {"../stuff/xy/bar.txt"}
    "_build/install/default/share/foo/foo.txt"

  $ dune install foo --prefix _foo
  $ find _foo | sort | grep txt
  _foo/share/foo/foo.txt
  _foo/share/stuff/foo.txt
  _foo/share/stuff/xy/bar.txt

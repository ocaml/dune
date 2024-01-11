Test for the case where the destination of an install stanza entry would place a
file outside of the directories associated with the package. This behaviour was
deprecated in 3.11.

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo))
  > EOF

Create a file to install
  $ mkdir a
  $ touch a/b.txt

Test that we get a warning if `(files ...)` has a dst starting with "..":
  $ cat >dune <<EOF
  > (install
  >  (section etc)
  >  (files (a/b.txt as ../b)))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 3, characters 20-24:
  3 |  (files (a/b.txt as ../b)))
                          ^^^^
  Warning: The destination path ../b begins with .. which will become an error
  in a future version of Dune. Destinations of files in install stanzas
  beginning with .. will be disallowed to prevent a package's installed files
  from escaping that package's install directories.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (escaping_paths_in_install_stanza disabled))
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  etc: [
    "_build/install/default/etc/b" {"../b"}
  ]

Test that we get a warning if `(dirs ...)` has a dst starting with "..":
  $ cat >dune <<EOF
  > (rule
  >  (target (dir bar))
  >  (action (progn (run mkdir bar) (run touch bar/baz.txt))))
  > 
  > (install
  >  (section etc)
  >  (dirs (bar as ../baz)))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 7, characters 15-21:
  7 |  (dirs (bar as ../baz)))
                     ^^^^^^
  Warning: The destination path ../baz begins with .. which will become an
  error in a future version of Dune. Destinations of files in install stanzas
  beginning with .. will be disallowed to prevent a package's installed files
  from escaping that package's install directories.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (escaping_paths_in_install_stanza disabled))
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  etc: [
    "_build/install/default/etc/baz/baz.txt" {"../baz/baz.txt"}
  ]

Test that we get a warning if `(dirs ...)` has a dst that is exactly "..":
  $ cat >dune <<EOF
  > (rule
  >  (target (dir bar))
  >  (action (progn (run mkdir bar) (run touch bar/baz.txt))))
  > 
  > (install
  >  (section etc)
  >  (dirs (bar as ..)))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 7, characters 15-17:
  7 |  (dirs (bar as ..)))
                     ^^
  Warning: The destination path .. begins with .. which will become an error in
  a future version of Dune. Destinations of files in install stanzas beginning
  with .. will be disallowed to prevent a package's installed files from
  escaping that package's install directories.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (escaping_paths_in_install_stanza disabled))
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  etc: [
    "_build/install/default/etc/baz.txt" {"../baz.txt"}
  ]

Test that we get get a warning if the ".." is the result of variable expansion:
  $ printf ".." > prefix.txt
  $ cat >dune <<EOF
  > (install
  >  (section etc)
  >  (files (a/b.txt as %{read:prefix.txt}/b)))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 3, characters 20-40:
  3 |  (files (a/b.txt as %{read:prefix.txt}/b)))
                          ^^^^^^^^^^^^^^^^^^^^
  Warning: The destination path ../b begins with .. which will become an error
  in a future version of Dune. Destinations of files in install stanzas
  beginning with .. will be disallowed to prevent a package's installed files
  from escaping that package's install directories.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (escaping_paths_in_install_stanza disabled))
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  etc: [
    "_build/install/default/etc/b" {"../b"}
  ]

Test that we get an error if `(source_tree ...)` has a dst starting with "..".
This is an error rather than a warning as installing source trees is added in
the same version of dune as starting a dest with ".." was deprecated.
  $ cat >dune <<EOF
  > (install
  >  (section etc)
  >  (source_trees (a as ../b)))
  > EOF
  $ dune build foo.install
  File "dune", line 3, characters 21-25:
  3 |  (source_trees (a as ../b)))
                           ^^^^
  Error: The destination path ../b begins with .. which is not allowed.
  Destinations in install stanzas may not begin with .. to prevent a package's
  installed files from escaping that package's install directories.
  [1]

Test that we get an error if `(source_tree ...)` has a dst that is exactly "..":
  $ cat >dune <<EOF
  > (install
  >  (section etc)
  >  (source_trees (a as ..)))
  > EOF
  $ dune build foo.install
  File "dune", line 3, characters 21-23:
  3 |  (source_trees (a as ..)))
                           ^^
  Error: The destination path .. begins with .. which is not allowed.
  Destinations in install stanzas may not begin with .. to prevent a package's
  installed files from escaping that package's install directories.
  [1]

Test that we get a warning if the ".." comes from the prefix of a glob:
  $ cat >dune <<EOF
  > (install
  >  (section etc)
  >  (files (glob_files_rec (a/*.txt with_prefix ../baz))))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 3, characters 45-51:
  3 |  (files (glob_files_rec (a/*.txt with_prefix ../baz))))
                                                   ^^^^^^
  Warning: The destination path ../baz/b.txt begins with .. which will become
  an error in a future version of Dune. Destinations of files in install
  stanzas beginning with .. will be disallowed to prevent a package's installed
  files from escaping that package's install directories.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (escaping_paths_in_install_stanza disabled))
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  etc: [
    "_build/install/default/etc/baz/b.txt" {"../baz/b.txt"}
  ]

Test that on older versions of dune we don't get warnings in this case:
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo))
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (section etc)
  >  (files (a/b.txt as ../b)))
  > 
  > (rule
  >  (target (dir bar))
  >  (action (progn (run mkdir bar) (run touch bar/baz.txt))))
  > 
  > (install
  >  (section etc)
  >  (dirs (bar as ../baz)))
  > 
  > (install
  >  (section share)
  >  (dirs (bar as ..)))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 15, characters 15-17:
  15 |  (dirs (bar as ..)))
                      ^^
  Warning: The destination path .. begins with .. which will become an error in
  a future version of Dune. Destinations of files in install stanzas beginning
  with .. will be disallowed to prevent a package's installed files from
  escaping that package's install directories.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (escaping_paths_in_install_stanza disabled))
  File "dune", line 11, characters 15-21:
  11 |  (dirs (bar as ../baz)))
                      ^^^^^^
  Warning: The destination path ../baz begins with .. which will become an
  error in a future version of Dune. Destinations of files in install stanzas
  beginning with .. will be disallowed to prevent a package's installed files
  from escaping that package's install directories.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (escaping_paths_in_install_stanza disabled))
  File "dune", line 3, characters 20-24:
  3 |  (files (a/b.txt as ../b)))
                          ^^^^
  Warning: The destination path ../b begins with .. which will become an error
  in a future version of Dune. Destinations of files in install stanzas
  beginning with .. will be disallowed to prevent a package's installed files
  from escaping that package's install directories.
  Hint: To disable this warning, add the following to your dune-project file:
  (warnings (escaping_paths_in_install_stanza disabled))
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/baz.txt" {"../baz.txt"}
  ]
  etc: [
    "_build/install/default/etc/b" {"../b"}
    "_build/install/default/etc/baz/b.txt" {"../baz/b.txt"}
  ]

Test that we don't get the warning if a vendored project starts an install dst
with "..". This is so that if a project vendors another project which would
produce warnings, the first project doesn't get spammed with those warnings.

  $ mkdir -p vendor/bar

  $ cat >vendor/dune <<EOF
  > (vendored_dirs *)
  > EOF

  $ cat >vendor/bar/dune-project <<EOF
  > (lang dune 3.10)
  > (package
  >  (name bar))
  > EOF

  $ touch vendor/bar/a.txt

  $ cat >vendor/bar/bar.ml <<EOF
  > let bar = "bar"
  > EOF

  $ cat >vendor/bar/dune <<EOF
  > (library
  >  (public_name bar))
  > 
  > (install
  >  (section etc)
  >  (files (a.txt as ../a.txt)))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (public_name foo)
  >  (libraries bar))
  > EOF

  $ cat >foo.ml <<EOF
  > let () = print_endline "hi"
  > EOF

  $ dune build foo.install && cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  bin: [
    "_build/install/default/bin/foo"
  ]


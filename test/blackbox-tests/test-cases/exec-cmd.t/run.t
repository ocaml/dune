  $ dune clean
  $ dune exec --no-build ./foo.exe
  Error: Program "./foo.exe" isn't built yet. You need to build it first or
  remove the --no-build option.
  [1]
  $ dune exec ./foo.exe
  Foo
  $ dune exec --profile release ./foo.exe
  Foo
  $ dune exec dunetestbar --no-build
  Error: Program "dunetestbar" isn't built yet. You need to build it first or
  remove the --no-build option.
  [1]
  $ dune exec dunetestbar
  Bar

On Macos there is an additional message about saving the DB.
  $ INSIDE_EMACS=yes dune exec --display=progress dunetestbar 2>&1 | sed -e "s/ *Saving digest db... *//g" -e "s//\n/g"
  Done: 0% (0/0, 0 left) (jobs: 0)
                                  
  Scanned 0 directories
                       
  Done: 0% (0/0, 0 left) (jobs: 0)
                                  
  Done: 0% (0/0, 0 left) (jobs: 1)
                                  
  Bar

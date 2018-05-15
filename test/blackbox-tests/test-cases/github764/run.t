  $ mkdir -p c1 && cd c1 && ln -s . x && ln -s . y && jbuilder build
  Path . has already been scanned. Cannot scan it again through symlink x
  [1]
  $ mkdir -p c2 && cd c2 && ln -s x ../../ && jbuilder build
  $ mkdir -p c3 && cd c3 && ln -s x y && ln -s y x && jbuilder build

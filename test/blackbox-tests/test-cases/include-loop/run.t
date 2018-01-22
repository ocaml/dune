  $ $JBUILDER build --root . -j 1
  File "jbuild", line 2, characters 0-15:
  Error: Recursive inclusion of jbuild files detected:
  File a.inc is included from c.inc:2
  --> included from b.inc:2
  --> included from a.inc:2
  --> included from jbuild:2
  [1]

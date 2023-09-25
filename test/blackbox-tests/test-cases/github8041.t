  $ cat > dune-project << EOF
  > (lang dune 2.4)
  > (package
  >  (name p))
  > EOF

  $ cat > dune << EOF
  > (rule (copy data.txt data2.txt))
  > 
  > (install
  >  (files data.txt data2.txt data3.txt)
  >  (section share))
  > EOF

  $ echo contents > data.txt
  $ head -c 100000 /dev/zero > data3.txt

If sendfile fails, we should fallback to the portable implementation.

  $ strace -e inject=sendfile:error=EINVAL -o /dev/null dune build @install

#8210: data2.txt is copied from readonly-file data.txt (#3092), so it needs to
be adequately unlinked before starting the fallback.

#8284: the buffer needs to be flushed, or there will be incomplete data in
larger files.

  $ if cmp -s data3.txt _build/default/data3.txt ; then
  >   echo "File copied correctly"
  > else
  >   echo "File copied incorrectly"
  > fi
  File copied correctly

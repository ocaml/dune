Test symlinks between subdirectories within the same directory target.

This tests a legitimate use case where symlinks reference files in sibling
directories within the same directory target. This should be allowed but is
currently incorrectly rejected due to validation against individual subdirectories
rather than the overall directory target boundary.

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (using directory-targets 0.1)
  > EOF

Test 1: Simple cross-subdirectory symlink within the same directory target

  $ cat > dune << EOF
  > (rule
  >  (target (dir shared_output))
  >  (action
  >   (progn
  >    (run mkdir -p shared_output/lib shared_output/bin)
  >    (system "echo 'library code' > shared_output/lib/core.txt")
  >    (chdir shared_output/bin
  >     (run ln -s ../lib/core.txt core_link.txt)))))
  > EOF

  $ dune build shared_output

Test 2: More complex nested cross-references

  $ cat > dune << EOF
  > (rule
  >  (target (dir complex_target))
  >  (action
  >   (progn
  >    (run mkdir -p complex_target/src/utils complex_target/tests complex_target/docs)
  >    (system "echo 'utility function' > complex_target/src/utils/helper.txt")
  >    (system "echo 'documentation' > complex_target/docs/readme.txt")
  >    (chdir complex_target/tests
  >     (progn
  >      (run ln -s ../src/utils/helper.txt test_helper.txt)
  >      (run ln -s ../docs/readme.txt test_docs.txt))))))
  > EOF

  $ dune build complex_target

Test 3: Deep nesting with relative path traversal

  $ cat > dune << EOF
  > (rule
  >  (target (dir deep_target))
  >  (action
  >   (progn
  >    (run mkdir -p deep_target/a/b/c deep_target/x/y/z)
  >    (system "echo 'deep file' > deep_target/a/b/c/file.txt")
  >    (chdir deep_target/x/y/z
  >     (run ln -s ../../../a/b/c/file.txt linked_file.txt)))))
  > EOF

  $ dune build deep_target

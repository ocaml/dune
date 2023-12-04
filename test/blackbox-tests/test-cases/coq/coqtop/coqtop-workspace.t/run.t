Running "dune coq top" from different directories in a workspace. The setup is
a workspace whose root is the root of a first project, and a second project is
contained under directory "project".

  $ unset INSIDE_DUNE
  $ . ./util.sh

Calling "dune coq top" from the workspace root.

  $ coqtop_test file1.v
  $ coqtop_test coq_dir1/file2.v
  $ coqtop_test coq_dir1/dir1/file3.v
  $ coqtop_test project/file4.v
  $ coqtop_test project/coq_dir2/file5.v
  $ coqtop_test project/coq_dir2/dir2/file6.v

Calling "dune coq top" from the "coq_dir1" directory.

  $ cd coq_dir1

  $ coqtop_test ../file1.v
  $ coqtop_test ../file1.v
  $ coqtop_test ../coq_dir1/file2.v
  $ coqtop_test ../coq_dir1/dir1/file3.v
  $ coqtop_test ../project/file4.v
  $ coqtop_test ../project/coq_dir2/file5.v
  $ coqtop_test ../project/coq_dir2/dir2/file6.v

  $ coqtop_test file2.v
  $ coqtop_test dir1/file3.v

  $ cd ..

Calling "dune coq top" from the "coq_dir1/dir1" directory.

  $ cd coq_dir1/dir1

  $ coqtop_test ../../file1.v
  $ coqtop_test ../../file1.v
  $ coqtop_test ../../coq_dir1/file2.v
  $ coqtop_test ../../coq_dir1/dir1/file3.v
  $ coqtop_test ../../project/file4.v
  $ coqtop_test ../../project/coq_dir2/file5.v
  $ coqtop_test ../../project/coq_dir2/dir2/file6.v

  $ coqtop_test ../file2.v
  $ coqtop_test ../dir1/file3.v
  $ coqtop_test file3.v

  $ cd ../..

Calling "dune coq top" from the "project" directory.

  $ cd project

  $ coqtop_test ../file1.v
  $ coqtop_test ../coq_dir1/file2.v
  $ coqtop_test ../coq_dir1/dir1/file3.v
  $ coqtop_test ../project/file4.v
  $ coqtop_test ../project/coq_dir2/file5.v
  $ coqtop_test ../project/coq_dir2/dir2/file6.v

  $ coqtop_test file4.v
  $ coqtop_test coq_dir2/file5.v
  $ coqtop_test coq_dir2/dir2/file6.v

  $ cd ..

Calling "dune coq top" from the "project/coq_dir2" directory.

  $ cd project/coq_dir2

  $ coqtop_test ../../file1.v
  $ coqtop_test ../../file1.v
  $ coqtop_test ../../coq_dir1/file2.v
  $ coqtop_test ../../coq_dir1/dir1/file3.v
  $ coqtop_test ../../project/file4.v
  $ coqtop_test ../../project/coq_dir2/file5.v
  $ coqtop_test ../../project/coq_dir2/dir2/file6.v

  $ coqtop_test ../file4.v
  $ coqtop_test ../coq_dir2/file5.v
  $ coqtop_test ../coq_dir2/dir2/file6.v
  $ coqtop_test file5.v
  $ coqtop_test dir2/file6.v

  $ cd ../..

Calling "dune coq top" from the "project/coq_dir2/dir2" directory.

  $ cd project/coq_dir2/dir2

  $ coqtop_test ../../../file1.v
  $ coqtop_test ../../../file1.v
  $ coqtop_test ../../../coq_dir1/file2.v
  $ coqtop_test ../../../coq_dir1/dir1/file3.v
  $ coqtop_test ../../../project/file4.v
  $ coqtop_test ../../../project/coq_dir2/file5.v
  $ coqtop_test ../../../project/coq_dir2/dir2/file6.v

  $ coqtop_test ../../file4.v
  $ coqtop_test ../../coq_dir2/file5.v
  $ coqtop_test ../../coq_dir2/dir2/file6.v
  $ coqtop_test ../file5.v
  $ coqtop_test ../dir2/file6.v
  $ coqtop_test file6.v

  $ cd ../../..

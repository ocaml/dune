Test that package builds do not rebuild when PATH changes after initial build.
This ensures builds are properly cached based on package content, not environment.

  $ . ./helpers.sh

Create a program that will be used during the build:

  $ mkdir tool_v1
  $ cat > tool_v1/mytool <<EOF
  > #!/usr/bin/env sh
  > echo "version1"
  > EOF
  $ chmod +x tool_v1/mytool

Set up initial PATH and create a simple package:

  $ export PATH="$PWD/tool_v1:$PATH"

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "mytool > output.txt")
  >   (system "cat output.txt")))
  > (install
  >  (system "mkdir -p %{lib}/foo && cp output.txt %{lib}/foo/"))
  > EOF

Build the package for the first time:

  $ build_pkg foo
  version1

Verify the installed file has the correct content:

  $ prefix=$(get_build_pkg_dir foo)
  $ cat "$prefix/target/lib/foo/output.txt"
  version1

Now create a different version of the tool:

  $ mkdir tool_v2
  $ cat > tool_v2/mytool <<EOF
  > #!/usr/bin/env sh
  > echo "version2"
  > EOF
  $ chmod +x tool_v2/mytool

Change PATH to use the new tool:

  $ export PATH="$PWD/tool_v2:$PATH"

Verify the new tool is now in PATH:

  $ which mytool
  $TESTCASE_ROOT/tool_v2/mytool
  $ mytool
  version2

Rebuild the package - it should use the cached build and NOT rebuild,
so the output should still be version1:

  $ build_pkg foo

The installed file should still have the original version (not rebuilt):

  $ cat "$prefix/target/lib/foo/output.txt"
  version1

  $ dune clean
  $ build_pkg foo
  version2

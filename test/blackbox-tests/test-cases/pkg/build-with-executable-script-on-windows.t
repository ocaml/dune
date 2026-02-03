Shows what happens when Dune tries to build a package which runs run a "broken"
executable script or a script with unsupported shebang line on Windows. The
tests only run on Windows since some of the errors are limitations of our
shebang parsing or the behavior on Linux and MacOS doesn't match (for instance
the file with just one line test).

For additional success cases see build-with-executable-script.t

Setup dune-project, dune-workspace, etc.

  $ make_lockdir

  $ mkdir scripts/

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ pkg() {
  > make_lockpkg $1 <<EOF
  > (build (run $2))
  > (version dev)
  > EOF
  > local files="${source_lock_dir}/$1.files"
  > local exec_path="${source_lock_dir}/$1.files/$2"
  > mkdir -p "${files}"
  > touch "${exec_path}"
  > chmod a+x "${exec_path}"
  > cat > "${exec_path}"
  > }

Running script with CRLF characters

  $ pkg foo ./executable.sh <<EOF
  > $(printf '#!/usr/bin/env sh \r\necho "Executable with CRLF chars ran successfully!"\r\n')
  > EOF
  $ dune build @pkg-install
  Executable with CRLF chars ran successfully!

Empty file

  $ pkg foo ./empty.sh
  $ dune build @pkg-install 2>&1 | grep Error
  Error: CreateProcess(): Exec format error
  [1]

File with just one line

  $ pkg foo ./one.sh
  $ exec="${source_lock_dir}/foo.files/one.sh"
  $ echo -n "#!/bin/sh" >"$exec"
  $ dune_cmd count-lines "$exec"
  1
  $ dune build @pkg-install

File with just "#!" in the first line

  $ pkg foo ./one.sh
  $ exec="${source_lock_dir}/foo.files/one.sh"
  $ echo -n "#!" >"$exec"
  $ dune_cmd count-lines "$exec"
  1
  $ dune build @pkg-install
  Error: CreateProcess(): Exec format error
  -> required by
     _build/_private/default/.pkg/foo.dev-5f224c017f3cf1ab04bdf8e60e90d898/target
  -> required by alias pkg-install
  [1]

Script doesn't exist

  $ pkg foo ./missing.sh
  $ rm "${source_lock_dir}/foo.files/missing.sh"
  $ dune build @pkg-install 2>&1 | grep Error
  Error: CreateProcess(): No such file or directory
  [1]

Executable couldn't be found

  $ pkg foo ./fake.sh <<EOF
  > #!/bin/fake
  > echo "no"
  > EOF
  $ dune build @pkg-install 2>&1 | grep Error
  Error: fake.sh: No such file or directory
  [1]

File with just shebang characters, but no executable

  $ pkg foo ./whitespace.sh <<EOF
  > #!    
  > EOF
  $ dune build @pkg-install 2>&1 | sanitize_pkg_digest foo.dev
  Error: Dune could not parse the shebang line in whitespace.sh:
    #!    
  Dune currently only supports the following forms of shebang lines:
    #!/path/to/executable <argument>
    #!/path/to/env <executable>
    #!/path/to/env -S <executable> <arg1> <arg2> ...
  If this is a valid shebang line that should be parsed by Dune, please report
  upstream including the contents of the shebang line.
  -> required by
     _build/_private/default/.pkg/foo.dev-DIGEST_HASH/target
  -> required by alias pkg-install
  [1]

Running env with arguments other than -S

  $ pkg foo ./executable.sh <<EOF
  > #!/usr/bin/env -vS sh
  > echo "Not supported"
  > EOF
  $ dune build @pkg-install 2>&1 | sanitize_pkg_digest foo.dev
  Error: Dune could not parse the shebang line in executable.sh:
    #!/usr/bin/env -vS sh
  Dune currently only supports the following forms of shebang lines:
    #!/path/to/executable <argument>
    #!/path/to/env <executable>
    #!/path/to/env -S <executable> <arg1> <arg2> ...
  If this is a valid shebang line that should be parsed by Dune, please report
  upstream including the contents of the shebang line.
  -> required by
     _build/_private/default/.pkg/foo.dev-DIGEST_HASH/target
  -> required by alias pkg-install
  [1]

Running env with extra arguments after -S

  $ pkg foo ./executable.sh <<EOF
  > #!/usr/bin/env -S -u DUNE_FOO sh
  > echo "Not supported"
  > EOF
  $ dune build @pkg-install 2>&1 | sanitize_pkg_digest foo.dev
  Error: Dune could not parse the shebang line in executable.sh:
    #!/usr/bin/env -S -u DUNE_FOO sh
  Dune currently only supports the following forms of shebang lines:
    #!/path/to/executable <argument>
    #!/path/to/env <executable>
    #!/path/to/env -S <executable> <arg1> <arg2> ...
  If this is a valid shebang line that should be parsed by Dune, please report
  upstream including the contents of the shebang line.
  -> required by
     _build/_private/default/.pkg/foo.dev-DIGEST_HASH/target
  -> required by alias pkg-install
  [1]

Test the "version" variable for packages not in the solution. In opam, absent
packages have version="" in a string interpolation context and undefined in
filter context (which evaluates to false).

  $ mkrepo

Test the variable in string interpolation context (command argument) and filter
context:

  $ mkpkg "test-version-var" <<'EOF'
  > build: [
  >   [ "echo" absent:version ]
  >   [ "echo" "this is a long line to force breaking %{absent:version}%" ]
  >   [ "echo" "%{absent:version?yes:no}%" ]
  >   [ "echo" "old version" ] {absent:version < "1.0"}
  >   [ "echo" "new version" ] {absent:version >= "1.0"}
  >   [ "echo" "has version" ] {absent:version}
  >   [ "echo" "no version" ] {!absent:version}
  > ]
  > EOF

  $ solve test-version-var
  Solution for dune.lock:
  - test-version-var.0.0.1

The variable resolves to empty string in string interpolation and false in
filter context at solve time. Commands with always-false conditions are removed,
and commands with always-true conditions have their filters removed:

  $ cat dune.lock/test-version-var.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo "")
       (run echo "this is a long line to force breaking ")
       (run echo no)
       (run echo "no version"))))))

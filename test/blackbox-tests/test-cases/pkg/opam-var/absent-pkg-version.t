Test the "version" variable for packages not in the solution. In opam, absent
packages have version="" in a string interpolation context and an error
otherwise.

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

String interpolation contexts resolve to empty string at solve time. Truthy and
filter contexts are left for build time evaluation where they will be undefined
and thus falsey:

  $ cat dune.lock/test-version-var.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo %{pkg:absent:version})
       (run echo "this is a long line to force breaking %{pkg:absent:version}")
       (run echo (if (catch_undefined_var %{pkg:absent:version} false) yes no))
       (when (< %{pkg:absent:version} 1.0) (run echo "old version"))
       (when (>= %{pkg:absent:version} 1.0) (run echo "new version"))
       (when %{pkg:absent:version} (run echo "has version"))
       (when (not %{pkg:absent:version}) (run echo "no version")))))))


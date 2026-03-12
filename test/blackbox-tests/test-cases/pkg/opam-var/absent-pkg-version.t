Test the "version" variable for packages not in the solution. In opam, absent
packages have version="" in a string interpolation context and an error
otherwise.

  $ mkrepo

Test the variable in string interpolation context (command argument) and filter
context:

  $ mkpkg "test-version-var" <<'EOF'
  > build: [
  >   [ "echo" absent:version ]
  >   [ "echo" "%{absent:version}%" ]
  >   [ "echo" "%{absent:version?yes:no}%" ]
  >   [ "echo" "has version" ] {not-in-lock:version}
  >   [ "echo" "no version" ] {!not-in-lock:version}
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
       (run echo "")
       (run echo "")
       (run echo (if (catch_undefined_var %{pkg:absent:version} false) yes no))
       (when %{pkg:not-in-lock:version} (run echo "has version"))
       (when (not %{pkg:not-in-lock:version}) (run echo "no version")))))))


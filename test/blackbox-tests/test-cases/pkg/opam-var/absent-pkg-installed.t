Test the "installed" variable for packages not in the solution. In opam,
absent packages have installed=false. We test different syntactic contexts:
bare ident, string interpolation, and filter context.

  $ mkrepo

  $ mkpkg "test-installed-var" <<'EOF'
  > build: [
  >   [ "echo" absent:installed ]
  >   [ "echo" "%{absent:installed}%" ]
  >   [ "echo" "%{absent:installed?yes:no}%" ]
  >   [ "echo" "installed" ] {absent:installed}
  >   [ "echo" "not installed" ] {!absent:installed}
  > ]
  > EOF

  $ solve test-installed-var
  Solution for dune.lock:
  - test-installed-var.0.0.1

Currently the installed variable is left as a pform. It should resolve to
"false" at solve time (matching opam semantics):

  $ cat dune.lock/test-installed-var.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo %{pkg:absent:installed})
       (run echo %{pkg:absent:installed})
       (run
        echo
        (if (catch_undefined_var %{pkg:absent:installed} false) yes no))
       (when %{pkg:absent:installed} (run echo installed))
       (when (not %{pkg:absent:installed}) (run echo "not installed")))))))


The "enable" variable is desugared to "installed?enable:disable". Currently the
variable is left as a conditional. Since installed is false for absent packages,
enable should resolve to "disable" at solve time:

  $ mkpkg "test-enable-var" <<'EOF'
  > build: [
  >   [ "echo" absent:enable ]
  >   [ "echo" "%{absent:enable}%" ]
  >   [ "echo" "%{absent:enable?yes:no}%" ]
  > ]
  > EOF

  $ solve test-enable-var
  Solution for dune.lock:
  - test-enable-var.0.0.1

  $ cat dune.lock/test-enable-var.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run
        echo
        (if (catch_undefined_var %{pkg:absent:installed} false) enable disable))
       (run
        echo
        (if (catch_undefined_var %{pkg:absent:installed} false) enable disable))
       (run echo (if (catch_undefined_var %{pkg:absent:enable} false) yes no)))))))


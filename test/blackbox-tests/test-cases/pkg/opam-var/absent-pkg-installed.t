Test the "installed" variable for packages not in the solution in different
contexts: string interpolation vs truthy/filter context.

  $ mkrepo

First, test the variable in string interpolation context (command argument):

  $ mkpkg "string-context" <<'EOF'
  > build: [
  >   [ "echo" "%{not-in-lock:installed}%" ]
  > ]
  > EOF

  $ solve string-context
  Solution for dune.lock:
  - string-context.0.0.1

The variable resolves to "false" at solve time:

  $ cat dune.lock/string-context.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (run echo false)))))




Now test the variable in truthy/filter context (conditional on command):

  $ mkpkg "truthy-context" <<'EOF'
  > build: [
  >   [ "echo" "yes" ] {not-in-lock:installed}
  >   [ "echo" "no" ] {!not-in-lock:installed}
  > ]
  > EOF

  $ solve truthy-context
  Solution for dune.lock:
  - truthy-context.0.0.1

In truthy context, the variable is left for build time evaluation where it will
be undefined and thus falsey:

  $ cat dune.lock/truthy-context.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (when %{pkg:not-in-lock:installed} (run echo yes))
       (when (not %{pkg:not-in-lock:installed}) (run echo no)))))))




The "enable" variable is desugared to "installed?enable:disable". The
"?then:else" syntax uses catch_undefined_var to handle undefined:

  $ mkpkg "enable-context" <<'EOF'
  > build: [
  >   [ "echo" "%{not-in-lock:enable}%" ]
  > ]
  > EOF

  $ solve enable-context
  Solution for dune.lock:
  - enable-context.0.0.1

The conditional evaluates to "disable" at build time when the variable is
undefined:

  $ cat dune.lock/enable-context.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (run
       echo
       (if
        (catch_undefined_var %{pkg:not-in-lock:installed} false)
        enable
        disable))))))




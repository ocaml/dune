Test the "version" variable for packages not in the solution in different
contexts: string interpolation vs truthy/filter context.

  $ mkrepo

First, test the variable in string interpolation context (command argument):

  $ mkpkg "string-context" <<'EOF'
  > build: [
  >   [ "echo" "version=%{not-in-lock:version}%" ]
  > ]
  > EOF

  $ solve string-context
  Solution for dune.lock:
  - string-context.0.0.1

The variable resolves to empty string at solve time:

  $ cat dune.lock/string-context.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms ((action (run echo "version=")))))


Now test the variable in truthy/filter context (conditional on command):

  $ mkpkg "truthy-context" <<'EOF'
  > build: [
  >   [ "echo" "has version" ] {not-in-lock:version}
  >   [ "echo" "no version" ] {!not-in-lock:version}
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
       (when %{pkg:not-in-lock:version} (run echo "has version"))
       (when (not %{pkg:not-in-lock:version}) (run echo "no version")))))))






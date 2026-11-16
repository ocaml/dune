A package dependency should expose the files it installs to another
package's sites without also installing that package's own files.

  $ unset DUNE_DIR_LOCATIONS
  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > (using dune_site 0.1)
  > (using directory-targets 0.1)
  > (package (name owner)
  >  (sites (share data) (share_root root_data) (libexec tools)))
  > (package (name installer))
  > EOF
  $ cat > dune <<'EOF'
  > (executable (name main) (libraries dune-site))
  > (generate_sites_module (module sites) (sites owner))
  > (install (package owner) (section (site (owner data))) (files owner.txt))
  > (install
  >  (package installer)
  >  (section share_root)
  >  (files (nested.txt as owner/data/nested.txt)))
  > (install
  >  (package installer)
  >  (section (site (owner root_data)))
  >  (files root.txt))
  > (install
  >  (package installer)
  >  (section libexec_root)
  >  (dirs (payload as owner)))
  > (rule
  >  (target (dir payload))
  >  (action
  >   (progn
  >    (run mkdir -p payload/tools)
  >    (system "echo tool > payload/tools/tool"))))
  > (rule (alias runtest) (deps (package installer)) (action (run ./main.exe)))
  > EOF
  $ echo owner > owner.txt
  $ echo nested > nested.txt
  $ echo root > root.txt
  $ write_site_test_reader
  $ cat > main.ml <<'EOF'
  > let () =
  >   let open Sites.Sites in
  >   let nested = Site_reader.print data "nested.txt" in
  >   let root = Site_reader.print root_data "root.txt" in
  >   let tool = Site_reader.print tools "tool" in
  >   let owner = Site_reader.print data "owner.txt" in
  >   if not (nested && root && tool && not owner) then exit 1
  > EOF
  $ dune runtest > output 2>&1
  [1]
  $ grep -E '^(nested\.txt|root\.txt|tool|owner\.txt):' output
  nested.txt: missing
  root.txt: missing
  tool: missing
  owner.txt: missing

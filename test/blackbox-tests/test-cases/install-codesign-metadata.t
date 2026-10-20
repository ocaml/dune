Installing an executable that needs artifact substitution re-signs it on
macOS.

  $ git init --quiet
  $ git -c user.name=Test -c user.email=test@example.com commit --allow-empty --quiet -m init
  $ git -c user.name=Test -c user.email=test@example.com tag -a v1.2.3 -m v1.2.3
  $ make_dune_project_with_package 3.25 foo
  $ cat >dune <<'EOF'
  > (executable
  >  (name main)
  >  (public_name main)
  >  (libraries dune-build-info))
  > EOF
  $ cat >main.ml <<'EOF'
  > let () =
  >   match Build_info.V1.version () with
  >   | None -> print_endline "no version"
  >   | Some v -> print_endline (Build_info.V1.Version.to_string v)
  > EOF
  $ dune build @install
  $ cat >entitlements.plist <<'EOF'
  > <?xml version="1.0" encoding="UTF-8"?>
  > <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  > <plist version="1.0"><dict>
  > <key>com.apple.security.virtualization</key><true/>
  > </dict></plist>
  > EOF
  $ chmod u+w _build/default/main.exe
  $ codesign --force --sign - --identifier org.example.main --options runtime --entitlements entitlements.plist _build/default/main.exe 2>/dev/null
  $ dune install --prefix prefix --sections bin
  $ prefix/bin/main
  v1.2.3
  $ codesign --verify --strict prefix/bin/main
  $ codesign --display --verbose=2 prefix/bin/main 2>&1 | sed -n -e '/^Identifier=/p' -e 's/.*flags=0x[0-9a-f]*(\([^)]*\)).*/flags=\1/p'
  Identifier=.#main
  flags=adhoc
  $ codesign --display --xml --entitlements - prefix/bin/main 2>/dev/null | grep -o com.apple.security.virtualization
  [1]

Basic usage of portable lockdirs.

  $ mkrepo
  $ add_mock_repo_if_needed

Create a package that writes a different value to some files depending on the os and arch.
  $ mkpkg foo <<EOF
  > build: [
  >   ["mkdir" "-p" share "%{lib}%/%{name}%"]
  >   ["touch" "%{lib}%/%{name}%/META"] # needed for dune to recognize this as a library
  >   ["sh" "-c" "echo Darwin > %{share}%/kernel"] { os = "macos" }
  >   ["sh" "-c" "echo Linux > %{share}%/kernel"] { os = "linux" }
  >   ["sh" "-c" "echo x86_64 > %{share}%/machine"] { arch = "x86_64" }
  >   ["sh" "-c" "echo arm64 > %{share}%/machine"] { arch = "arm64" }
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > (package
  >  (name x)
  >  (depends foo))
  > EOF

  $ cat > x.ml <<EOF
  > let () = print_endline "Hello, World!"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries foo))
  > EOF

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - foo.0.0.1

  $ cat ${default_lock_dir}/lock.dune
  (lang package 0.1)
  
  (dependency_hash 36e640fbcda71963e7e2f689f6c96c3e)
  
  (repositories
   (complete false)
   (used))
  
  (solved_for_platforms
   ((arch x86_64)
    (os linux))
   ((arch arm64)
    (os linux))
   ((arch x86_64)
    (os macos))
   ((arch arm64)
    (os macos)))

  $ cat ${default_lock_dir}/foo.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (choice
    ((((arch x86_64) (os linux)))
     ((action
       (progn
        (run mkdir -p %{share} %{lib}/%{pkg-self:name})
        (run touch %{lib}/%{pkg-self:name}/META)
        (run sh -c "echo Linux > %{share}/kernel")
        (run sh -c "echo x86_64 > %{share}/machine")))))
    ((((arch arm64) (os linux)))
     ((action
       (progn
        (run mkdir -p %{share} %{lib}/%{pkg-self:name})
        (run touch %{lib}/%{pkg-self:name}/META)
        (run sh -c "echo Linux > %{share}/kernel")
        (run sh -c "echo arm64 > %{share}/machine")))))
    ((((arch x86_64) (os macos)))
     ((action
       (progn
        (run mkdir -p %{share} %{lib}/%{pkg-self:name})
        (run touch %{lib}/%{pkg-self:name}/META)
        (run sh -c "echo Darwin > %{share}/kernel")
        (run sh -c "echo x86_64 > %{share}/machine")))))
    ((((arch arm64) (os macos)))
     ((action
       (progn
        (run mkdir -p %{share} %{lib}/%{pkg-self:name})
        (run touch %{lib}/%{pkg-self:name}/META)
        (run sh -c "echo Darwin > %{share}/kernel")
        (run sh -c "echo arm64 > %{share}/machine")))))))

  $ DUNE_CONFIG__ARCH=arm64 dune build
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/kernel
  Linux
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/machine
  arm64

  $ DUNE_CONFIG__OS=macos DUNE_CONFIG__ARCH=x86_64 DUNE_CONFIG__OS_FAMILY=homebrew DUNE_CONFIG__OS_DISTRIBUTION=homebrew DUNE_CONFIG__OS_VERSION=15.3.1 dune build
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/kernel
  Darwin
  $ cat $pkg_root/$(dune pkg print-digest foo)/target/share/machine
  x86_64

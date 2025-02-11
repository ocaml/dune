Demonstration of portable lockdirs.

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

A package that writes some info about machine where it's built to the share directory.
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
  > (lang dune 3.17)
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

  $ DUNE_CONFIG__PORTABLE_LOCK_DIR=enabled dune pkg lock
  Solution for dune.lock:
  - foo.0.0.1

  $ cat dune.lock/foo.pkg
  (version 0.0.1)
  
  (build
   (((arch x86_64)
     (os linux)
     (os-distribution alpine))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo Linux > %{share}/kernel")
       (run sh -c "echo x86_64 > %{share}/machine")))))
   (((arch x86_64)
     (os linux))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo Linux > %{share}/kernel")
       (run sh -c "echo x86_64 > %{share}/machine")))))
   (((arch arm64)
     (os linux)
     (os-distribution alpine))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo Linux > %{share}/kernel")
       (run sh -c "echo arm64 > %{share}/machine")))))
   (((arch arm64)
     (os linux))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo Linux > %{share}/kernel")
       (run sh -c "echo arm64 > %{share}/machine")))))
   (((arch x86_64)
     (os macos))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo Darwin > %{share}/kernel")
       (run sh -c "echo x86_64 > %{share}/machine")))))
   (((arch arm64)
     (os macos))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo Darwin > %{share}/kernel")
       (run sh -c "echo arm64 > %{share}/machine")))))
   (((arch x86_64)
     (os win32)
     (os-distribution cygwin))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo x86_64 > %{share}/machine")))))
   (((arch x86_64)
     (os win32))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo x86_64 > %{share}/machine")))))
   (((arch arm64)
     (os win32)
     (os-distribution cygwin))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo arm64 > %{share}/machine")))))
   (((arch arm64)
     (os win32))
    ((action
      (progn
       (run mkdir -p %{share} %{lib}/%{pkg-self:name})
       (run touch %{lib}/%{pkg-self:name}/META)
       (run sh -c "echo arm64 > %{share}/machine"))))))
  
  (depends
   (((arch x86_64)
     (os linux)
     (os-distribution alpine))
    ())
   (((arch x86_64)
     (os linux))
    ())
   (((arch arm64)
     (os linux)
     (os-distribution alpine))
    ())
   (((arch arm64)
     (os linux))
    ())
   (((arch x86_64)
     (os macos))
    ())
   (((arch arm64)
     (os macos))
    ())
   (((arch x86_64)
     (os win32)
     (os-distribution cygwin))
    ())
   (((arch x86_64)
     (os win32))
    ())
   (((arch arm64)
     (os win32)
     (os-distribution cygwin))
    ())
   (((arch arm64)
     (os win32))
    ()))

  $ dune build

  $ [ $(cat _build/_private/default/.pkg/foo/target/share/kernel) = $(uname -s) ]

  $ [ $(cat _build/_private/default/.pkg/foo/target/share/machine) = $(uname -m) ]

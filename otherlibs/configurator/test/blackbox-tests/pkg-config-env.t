Homebrew's additional search path must preserve the rest of the environment.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF
  $ cat > dune <<'EOF'
  > (executable
  >  (name config_test)
  >  (libraries dune-configurator))
  > EOF
  $ cat > config_test.ml <<'EOF'
  > open Configurator.V1
  > let () = main ~name:"pkg-config-env" (fun c ->
  >   let pc = Option.get (Pkg_config.get c) in
  >   List.iter (fun package ->
  >     (match Pkg_config.query pc ~package with
  >      | None -> print_endline "package missing"
  >      | Some { libs; cflags } ->
  >        print_endline (String.concat " " (cflags @ libs)));
  >     print_endline (Option.get
  >       (Pkg_config.query_variable pc ~package ~variable:"pc_path")))
  >     ["with-brew"; "without-brew"])
  > EOF

  $ mkdir -p bin brew/opt/with-brew/lib/pkgconfig
  $ cat > bin/brew <<'EOF'
  > #!/bin/sh
  > printf '%s\n' "$BREW_PREFIX"
  > EOF
  $ cat > bin/pkg-config <<'EOF'
  > #!/bin/sh
  > case "$1" in
  >   --print-errors) test "$PKG_CONFIG_LIBDIR" = /custom/libdir ;;
  >   --cflags) printf '%s\n' "-I$PKG_CONFIG_SYSROOT_DIR/include" ;;
  >   --libs) printf '%s\n' "-L$PKG_CONFIG_LIBDIR" ;;
  >   --variable=pc_path) printf '%s\n' "$PKG_CONFIG_PATH" ;;
  > esac
  > EOF
  $ chmod +x bin/brew bin/pkg-config
  $ export PATH="$PWD/bin:$PATH"
  $ export PKG_CONFIG="$PWD/bin/pkg-config"
  $ export BREW_PREFIX="$PWD/brew"
  $ export PKG_CONFIG_PATH=/custom/pkgconfig
  $ export PKG_CONFIG_LIBDIR=/custom/libdir
  $ export PKG_CONFIG_SYSROOT_DIR=/custom/sysroot
  $ unset PKG_CONFIG_ARGN

The existing search path keeps precedence over Homebrew. Packages without a
Homebrew directory still receive the original environment.

  $ dune exec ./config_test.exe
  -I/custom/sysroot/include -L/custom/libdir
  /custom/pkgconfig:$TESTCASE_ROOT/brew/opt/with-brew/lib/pkgconfig
  -I/custom/sysroot/include -L/custom/libdir
  /custom/pkgconfig

{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    melange = {
      url = "github:melange-re/melange/refs/tags/5.1.0-53";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ocaml-overlays = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      melange,
      ocaml-overlays,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
          ocaml-overlays.overlays.default
          (self: super: {
            ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (
              oself: osuper: {
                mdx = osuper.mdx.override {
                  logs = oself.logs;
                };
                utop = osuper.utop.overrideAttrs {
                  dontGzipMan = true;
                };
              }
            );
          })
          melange.overlays.default
          (self: super: {
            coq_8_16_native = super.coq_8_16.overrideAttrs (a: {
              configureFlags = [
                "-native-compiler"
                "yes"
              ];
            });
          })
        ];
        dune-static-overlay = self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (
            oself: osuper: {
              dune_3 = osuper.dune_3.overrideAttrs (a: {
                src = ./.;
                preBuild = "ocaml boot/bootstrap.ml --static";
              });
            }
          );
        };
        pkgs-static = nixpkgs.legacyPackages.${system}.appendOverlays [
          ocaml-overlays.overlays.default
          dune-static-overlay
        ];

        add-experimental-configure-flags =
          pkg:
          pkg.overrideAttrs {
            configureFlags = [
              "--pkg-build-progress"
              "enable"
              "--lock-dev-tool"
              "enable"
              "--portable-lock-dir"
              "enable"
            ];
          };

        ocamlformat =
          let
            ocamlformat_version =
              let
                lists = pkgs.lib.lists;
                strings = pkgs.lib.strings;
                ocamlformat_config = strings.splitString "\n" (builtins.readFile ./.ocamlformat);
                prefix = "version=";
                ocamlformat_version_pred = line: strings.hasPrefix prefix line;
                version_line = lists.findFirst ocamlformat_version_pred "not_found" ocamlformat_config;
                version = strings.removePrefix prefix version_line;
              in
              builtins.replaceStrings [ "." ] [ "_" ] version;
          in
          builtins.getAttr ("ocamlformat_" + ocamlformat_version) pkgs;

        testBuildInputs =
          with pkgs;
          [
            file
            mercurial
            unzip
          ]
          ++ lib.optionals stdenv.isLinux [ strace ];
        testNativeBuildInputs =
          pkgs: with pkgs; [
            nodejs-slim
            pkg-config
            opam
            ocamlformat
          ];

        docInputs = with pkgs.python3.pkgs; [
          sphinx-autobuild
          furo
          sphinx-copybutton
          sphinx-design
          myst-parser
        ];
      in
      {
        formatter = pkgs.nixpkgs-fmt;

        packages = {
          default =
            with pkgs;
            stdenv.mkDerivation {
              pname = "dune";
              version = "3.x-n/a";
              src =
                let
                  fs = lib.fileset;
                in
                fs.toSource {
                  root = ./.;
                  fileset = fs.unions [
                    ./bin
                    ./boot
                    ./configure
                    ./dune-project
                    ./dune-file
                    ./src
                    ./plugin
                    ./vendor
                    ./otherlibs
                    ./Makefile
                    (fs.fileFilter (file: file.hasExt "opam" || file.hasExt "template") ./.)
                  ];
                };
              nativeBuildInputs = with ocamlPackages; [
                ocaml
                findlib
              ];
              buildInputs = lib.optionals stdenv.isDarwin [
                darwin.apple_sdk.frameworks.CoreServices
              ];
              strictDeps = true;
              buildFlags = [ "release" ];
              dontAddPrefix = true;
              dontAddStaticConfigureFlags = true;
              configurePlatforms = [ ];
              installFlags = [
                "PREFIX=${placeholder "out"}"
                "LIBDIR=$(OCAMLFIND_DESTDIR)"
              ];
            };
          dune = self.packages.${system}.default;
          dune-static = pkgs-static.pkgsCross.musl64.ocamlPackages.dune;
          dune-experimental = add-experimental-configure-flags self.packages.${system}.dune;
          dune-static-experimental = add-experimental-configure-flags self.packages.${system}.dune-static;
        };

        devShells =
          let
            INSIDE_NIX = "true";
            makeDuneDevShell =
              {
                extraBuildInputs ? (pkgs: [ ]),
                meta ? null,
                duneFromScope ? false,
              }:
              let
                pkgs' =
                  if duneFromScope then
                    pkgs.extend (
                      pself: psuper: {
                        ocamlPackages = psuper.ocamlPackages.overrideScope (
                          oself: osuper: {
                            dune_3 = self.packages.${system}.default;
                          }
                        );
                      }
                    )
                  else
                    pkgs;

                inherit (pkgs') writeScriptBin stdenv;

                duneScript = writeScriptBin "dune" ''
                  #!${stdenv.shell}
                  "$DUNE_SOURCE_ROOT"/_boot/dune.exe $@
                '';
              in

              pkgs'.mkShell {
                shellHook = ''
                  export DUNE_SOURCE_ROOT=$PWD
                '';
                inherit meta;
                nativeBuildInputs = (testNativeBuildInputs pkgs') ++ docInputs ++ [ duneScript ];
                inputsFrom = [ pkgs'.ocamlPackages.dune_3 ];
                buildInputs =
                  testBuildInputs
                  ++ (
                    with pkgs'.ocamlPackages;
                    [
                      ctypes
                      cinaps
                      integers
                      lwt
                      mdx
                      menhir
                      merlin
                      ocaml-index
                      ocaml-lsp
                      odoc
                      patdiff
                      ppx_expect
                      re
                      spawn
                      uutf
                    ]
                    ++ (extraBuildInputs pkgs')
                  );
                inherit INSIDE_NIX;
              };
          in
          {
            doc = pkgs.mkShell {
              inherit INSIDE_NIX;
              buildInputs = docInputs;
              meta.description = ''
                Provides a shell environment suitable for building the Dune
                documentation website (e.g. `make doc`).
              '';
            };

            fmt = pkgs.mkShell {
              inherit INSIDE_NIX;
              nativeBuildInputs = [ ocamlformat ];
              # re shouldn't be needed. this is an issue with the fmt rules
              inputsFrom = [
                pkgs.dune_3
              ];
              buildInputs = with pkgs.ocamlPackages; [
                re
                spawn
                uutf
                findlib
              ];
              meta.description = ''
                Provides a shell environment suitable for formatting the Dune
                codebase source code (e.g. with `make fmt`).
              '';
            };

            slim = makeDuneDevShell {
              meta.description = ''
                Provides a minimal shell environment built purely from nixpkgs
                that can run the testsuite (except the coq / melange tests).
              '';
            };
            slim-melange = makeDuneDevShell {
              extraBuildInputs = pkgs: [
                pkgs.ocamlPackages.melange
              ];
              meta.description = ''
                Provides a minimal shell environment built purely from nixpkgs
                that can run the testsuite (except the coq tests).
              '';
            };
            slim-opam =
              with pkgs;
              mkShell {
                inherit INSIDE_NIX;
                nativeBuildInputs = lib.remove pkgs.ocamlformat (testNativeBuildInputs pkgs);
                buildInputs = lib.optionals stdenv.isDarwin [
                  darwin.apple_sdk.frameworks.CoreServices
                ];
                meta.description = ''
                  provides a shell with just `opam` and minimal (external)
                  dependencies to run the testsuite.";
                '';
              };

            coq = pkgs.mkShell {
              inherit INSIDE_NIX;
              nativeBuildInputs = (testNativeBuildInputs pkgs);
              # Coq requires OCaml 4.x
              inputsFrom = [
                pkgs.ocaml-ng.ocamlPackages_4_14.dune_3
              ];
              buildInputs = with pkgs; [
                ocaml-ng.ocamlPackages_4_14.re
                ocaml-ng.ocamlPackages_4_14.spawn
                ocaml-ng.ocamlPackages_4_14.uutf
                coq_8_16_native
                coq_8_16_native.ocamlPackages.findlib
              ];
              meta.description = ''
                Provides a minimal shell environment built purely from nixpkgs
                that can build Dune and the Coq testsuite.
              '';
            };

            bootstrap-check = pkgs.mkShell {
              inherit INSIDE_NIX;
              buildInputs = with pkgs; [
                gnumake
                ocaml-ng.ocamlPackages_4_02.ocaml
              ];
              meta.description = ''
                Provides a minimal shell environment with OCaml 4.02 in order
                to test the bootstrapping script.
              '';
            };

            bootstrap-check_4_08 = pkgs.mkShell {
              inherit INSIDE_NIX;
              buildInputs = with pkgs; [
                gnumake
                ocaml-ng.ocamlPackages_4_08.ocaml
              ];
              meta.description = ''
                Provides a minimal shell environment with OCaml 4.08 in order
                to test the bootstrapping script.
              '';
            };

            microbench = makeDuneDevShell {
              extraBuildInputs = pkgs: [
                pkgs.ocamlPackages.core_bench
              ];
              meta.description = ''
                Provides a minimal shell environment that can build the
                microbenchmarks.
              '';
            };

            scope = makeDuneDevShell {
              duneFromScope = true;
              meta.description = ''
                Provides a minimal shell environment built purely from nixpkgs
                that replaces the Dune executable in the `ocamlPackages` scope by
                the Dune binary built by from the repo.
              '';
            };
            default = makeDuneDevShell {
              extraBuildInputs =
                pkgs:
                (with pkgs; [
                  # dev tools
                  ccls
                  # test dependencies
                  binaryen
                  curl
                  git
                  procps
                  which
                ])
                ++ (with pkgs.ocamlPackages; [
                  core_bench
                  js_of_ocaml
                  js_of_ocaml-compiler
                  ocaml-lsp
                  pkgs.ocamlPackages.melange
                  utop
                  wasm_of_ocaml-compiler
                ]);
              meta.description = ''
                Provides a shell environment where `dune` is provided and built
                using the source code in this repo.
              '';
            };
          };
      }
    );
}

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
    oxcaml = {
      url = "github:oxcaml/oxcaml";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    oxcaml-opam-repository = {
      url = "github:oxcaml/opam-repository";
      flake = false;
    };
  };
  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      melange,
      ocaml-overlays,
      oxcaml,
      oxcaml-opam-repository,
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

        applyOxcamlPatches = import ./nix/ox-patches.nix {
          inherit pkgs;
          lib = pkgs.lib;
          oxcamlOpamRepo = oxcaml-opam-repository;
        };

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
            perl
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
        formatter = pkgs.nixfmt;

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
        };

        devShells =
          let
            INSIDE_NIX = "true";
            makeDuneDevShell =
              {
                extraBuildInputs ? (pkgs: [ ]),
                meta ? null,
                duneFromScope ? false,
                includeTestDeps ? true,
                packageOverrides ? (oself: osuper: { }),
              }:
              let
                hasOcamlOverride = (packageOverrides { } { ocaml = null; }) ? ocaml;

                pkgs' =
                  if hasOcamlOverride then
                    pkgs.extend (
                      pself: psuper: {
                        ocamlPackages = psuper.ocamlPackages.overrideScope (
                          oself: osuper:
                          (pkgs.lib.mapAttrs (
                            name: pkg:
                            if pkgs.lib.isDerivation pkg && pkg ? overrideAttrs then
                              pkg.overrideAttrs (old: {
                                doCheck = false;
                              })
                            else
                              pkg
                          ) osuper)
                          // (packageOverrides oself osuper)
                        );
                      }
                    )
                  else if duneFromScope then
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

                baseInputs = if includeTestDeps then (testNativeBuildInputs pkgs') ++ docInputs else [ ];

                ocamlLibs =
                  if includeTestDeps then
                    (with pkgs'.ocamlPackages; [
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
                      pp
                      ppx_expect
                      re
                      spawn
                      uutf
                    ])
                  else
                    [ ];
              in

              pkgs'.mkShell {
                shellHook = ''
                  export DUNE_SOURCE_ROOT=$PWD
                '';
                inherit meta;
                nativeBuildInputs =
                  baseInputs ++ [ duneScript ] ++ (if hasOcamlOverride then [ pkgs'.ocamlPackages.ocaml ] else [ ]);
                inputsFrom = if hasOcamlOverride then [ ] else [ pkgs'.ocamlPackages.dune_3 ];
                buildInputs =
                  (if includeTestDeps then testBuildInputs else [ ])
                  ++ ocamlLibs
                  ++ (extraBuildInputs pkgs')
                  ++ (if hasOcamlOverride then [ pkgs'.ocamlPackages.findlib ] else [ ]);
                inherit INSIDE_NIX;
                dontDetectOcamlConflicts = hasOcamlOverride;
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
                csexp
                pp
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
                ocaml-ng.ocamlPackages_4_14.csexp
                ocaml-ng.ocamlPackages_4_14.pp
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

            bootstrap-ox = pkgs.mkShell {
              inherit INSIDE_NIX;
              buildInputs = [
                pkgs.gnumake
                oxcaml.packages.${system}.default
              ];
              meta.description = ''
                Provides a minimal shell environment with OxCaml in order to
                test the bootstrapping script.
              '';
            };

            ox-minimal = makeDuneDevShell {
              includeTestDeps = false;
              packageOverrides =
                oself: osuper:
                (applyOxcamlPatches oself osuper)
                // {
                  # dune_3 = self.packages.${system}.default;
                  ocaml = oxcaml.packages.${system}.default.overrideAttrs (old: {
                    passthru = (old.passthru or { }) // pkgs.ocamlPackages.ocaml.passthru;
                    meta = (old.meta or { }) // pkgs.ocamlPackages.ocaml.meta;
                  });
                  spawn = osuper.spawn.overrideAttrs (old: {
                    doCheck = false;
                  });
                  csexp = osuper.csexp.overrideAttrs (old: {
                    doCheck = false;
                  });
                  pp = osuper.pp.overrideAttrs (old: {
                    doCheck = false;
                  });
                };
              extraBuildInputs =
                pkgs: with pkgs.ocamlPackages; [
                  csexp
                  pp
                  re
                  spawn
                  uutf
                  findlib
                ];
              meta.description = ''
                Provides a minimal shell environment with OxCaml in order to
                run the OxCaml tests.
              '';
            };

            ox = makeDuneDevShell {
              packageOverrides =
                oself: osuper:
                (applyOxcamlPatches oself osuper)
                // {
                  dune_3 = self.packages.${system}.default;
                  ocaml = oxcaml.packages.${system}.default.overrideAttrs (old: {
                    passthru = (old.passthru or { }) // pkgs.ocamlPackages.ocaml.passthru;
                    meta = (old.meta or { }) // pkgs.ocamlPackages.ocaml.meta;
                  });
                };
              meta.description = ''
                Provides a full shell environment with the OxCaml compiler to
                develop with Dune. Warning: does not work.
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

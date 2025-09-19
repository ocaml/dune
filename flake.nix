{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
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
      nixpkgs,
      melange,
      ocaml-overlays,
    }:
    let
      inherit (nixpkgs) lib;
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      eachSystem = lib.genAttrs systems;
      pkgsFor = eachSystem (
        system:
        import nixpkgs {
          localSystem.system = system;
          overlays = [
            ocaml-overlays.overlays.default
            melange.overlays.default
            (_: super: {
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
            (_: super: {
              coq_8_16_native = super.coq_8_16.overrideAttrs (_: {
                configureFlags = [
                  "-native-compiler"
                  "yes"
                ];
              });
            })
          ];
        }
      );
      dune-static-overlay = _: super: {
        ocamlPackages = super.ocaml-ng.ocamlPackages_5_3.overrideScope (
          _: osuper: {
            dune_3 = osuper.dune_3.overrideAttrs (a: {
              src = ./.;
              preBuild = "ocaml boot/bootstrap.ml --static";
            });
          }
        );
      };

      pkgsForStatic = eachSystem (
        system:
        import nixpkgs {
          localSystem.system = system;
          overlays = [
            ocaml-overlays.overlays.default
            dune-static-overlay
          ];
        }
      );
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
      ocamlformat_version =
        let
          ocamlformat_config = lib.splitString "\n" (builtins.readFile ./.ocamlformat);
          prefix = "version=";
          ocamlformat_version_pred = line: lib.hasPrefix prefix line;
          version_line = lib.findFirst ocamlformat_version_pred "not_found" ocamlformat_config;
          version = lib.removePrefix prefix version_line;
        in
        lib.replaceString "." "_" version;
    in
    {
      formatter = eachSystem (system: pkgsFor.${system}.nixfmt);

      packages = eachSystem (
        system:
        let
          pkgs = pkgsFor.${system};
        in
        {
          default = self.packages.${system}.dune;

          dune = pkgs.stdenv.mkDerivation {
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

            nativeBuildInputs = with pkgs.ocamlPackages; [
              ocaml
              findlib
            ];

            buildInputs = lib.optionals pkgs.stdenv.isDarwin [
              pkgs.darwin.apple_sdk.frameworks.CoreServices
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

          dune-static = pkgsForStatic.${system}.pkgsCross.musl64.ocamlPackages.dune;
          dune-experimental = add-experimental-configure-flags self.packages.${system}.dune;
          dune-static-experimental = add-experimental-configure-flags self.packages.${system}.dune-static;
        }
      );

      devShells = eachSystem (
        system:
        let
          pkgs = pkgsFor.${system};
          INSIDE_NIX = "true";

          ocamlformat = builtins.getAttr ("ocamlformat_" + ocamlformat_version) pkgs;

          testNativeBuildInputs =
            p:
            [ ocamlformat ]
            ++ (with p; [
              nodejs-slim
              pkg-config
              opam
            ]);

          makeDuneDevShell =
            {
              extraBuildInputs ? (_: [ ]),
              meta ? null,
              duneFromScope ? false,
            }:
            let
              pkgs' =
                if duneFromScope then
                  pkgs.extend (
                    _: psuper: {
                      ocamlPackages = psuper.ocamlPackages.overrideScope (
                        _: _: {
                          dune_3 = self.packages.${system}.default;
                        }
                      );
                    }
                  )
                else
                  pkgs;

              duneScript = pkgs.writeScriptBin "dune" ''
                #!${pkgs.stdenv.shell}
                "$DUNE_SOURCE_ROOT"/_boot/dune.exe $@
              '';
            in

            pkgs.mkShell {
              shellHook = ''
                export DUNE_SOURCE_ROOT=$PWD
              '';
              inherit INSIDE_NIX;
              inherit meta;
              nativeBuildInputs = [ duneScript ] ++ (testNativeBuildInputs pkgs');
              inputsFrom = [
                pkgs'.ocamlPackages.dune_3
                self.devShells.${system}.doc
              ];
              buildInputs =
                (with pkgs; [
                  file
                  mercurial
                  unzip
                ])
                ++ (with pkgs'.ocamlPackages; [
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
                ])
                ++ (extraBuildInputs pkgs')
                ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.strace ];
            };
        in
        {
          doc = pkgs.mkShell {
            inherit INSIDE_NIX;
            packages = with pkgs.python3Packages; [
              sphinx-autobuild
              furo
              sphinx-copybutton
              sphinx-design
              myst-parser
            ];
            meta.description = ''
              Provides a shell environment suitable for building the Dune
              documentation website (e.g. `make doc`).
            '';
          };

          fmt = pkgs.mkShell {
            inherit INSIDE_NIX;
            packages = [
              ocamlformat
            ]
            ++ (with pkgs.ocamlPackages; [
              re
              spawn
              uutf
              findlib
            ]);

            # re shouldn't be needed. this is an issue with the fmt rules
            inputsFrom = [
              pkgs.dune_3
            ];

            meta.description = ''
              Provides a shell environment suitable for formatting the Dune
              codebase source code (e.g. with `make fmt`).
            '';
          };

          slim-opam = pkgs.mkShell {
            inherit INSIDE_NIX;
            nativeBuildInputs = lib.remove ocamlformat (testNativeBuildInputs pkgs);
            buildInputs = lib.optionals pkgs.stdenv.isDarwin [
              pkgs.darwin.apple_sdk.frameworks.CoreServices
            ];
            meta.description = ''
              provides a shell with just `opam` and minimal (external)
              dependencies to run the testsuite.";
            '';
          };

          slim = makeDuneDevShell {
            meta.description = ''
              Provides a minimal shell environment built purely from nixpkgs
              that can run the testsuite (except the coq / melange tests).
            '';
          };

          slim-melange = makeDuneDevShell {
            extraBuildInputs = p: [ p.ocamlPackages.melange ];
            meta.description = ''
              Provides a minimal shell environment built purely from nixpkgs
              that can run the testsuite (except the coq tests).
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
            extraBuildInputs = p: [
              p.ocamlPackages.core_bench
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
        }
      );

    };
}

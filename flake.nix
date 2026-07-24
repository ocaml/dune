{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    melange = {
      url = "github:melange-re/melange/v7-54";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ocaml-overlays = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    revdeps-dune = {
      url = "github:ocaml/dune";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.ocaml-overlays.follows = "ocaml-overlays";
      inputs.melange.follows = "melange";
      inputs.revdeps-dune.follows = "revdeps-dune";
    };
  };

  nixConfig = {
    extra-substituters = "https://anmonteiro.nix-cache.workers.dev";
    extra-trusted-public-keys = "ocaml.nix-cache.com-1:/xI2h2+56rwFfKyyFVbkJSeGqSIYMC/Je+7XXqGKDIY=";
  };

  outputs =
    {
      self,
      nixpkgs,
      melange,
      ocaml-overlays,
      revdeps-dune,
    }:
    let
      forAllSystems =
        f:
        nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed (
          system:
          let
            # Vanilla nixpkgs scope used to source packages we want to take
            # ahead of what `ocaml-overlays` ships (e.g. odoc 3.2.1).
            nixpkgsOcaml = nixpkgs.legacyPackages.${system}.ocaml-ng.ocamlPackages_5_4;
            pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
              ocaml-overlays.overlays.default
              (self: super: {
                ocamlPackages = super.ocaml-ng.ocamlPackages_5_4.overrideScope (
                  oself: osuper: {
                    ocaml = osuper.ocaml.override {
                      flambdaSupport = false;
                      framePointerSupport = true;
                    };
                    utop = osuper.utop.overrideAttrs {
                      dontGzipMan = true;
                    };
                    odoc-parser = osuper.odoc-parser.overrideAttrs (old: {
                      inherit (nixpkgsOcaml.odoc-parser) version src;
                      doCheck = false;
                    });
                    odoc = osuper.odoc.overrideAttrs (old: {
                      inherit (nixpkgsOcaml.odoc) version src;
                      doCheck = false;
                    });
                    # Templates the cross-compiled dune binary used by
                    # `windows-static`. Lives at the top-level scope so
                    # `nix-overlays`' cross-overlay sees it during scope
                    # construction and `fixOCamlPackage` can pair the cross
                    # derivation with this native template via
                    # `findNativePackage`.
                    dune_target = oself.callPackage ./nix/dune-target.nix { };
                  }
                );
                # Keep `ocaml-ng.ocamlPackages_5_4` in sync with the override
                # above. nix-overlays' `cross/ocaml.nix:46` looks up the native
                # OCaml via `buildPackages.ocaml-ng."ocamlPackages_5_4"`, so
                # without this the cross-target side uses our overridden ocaml
                # while the native side (used to build findlib's `nativeBuild-
                # Inputs` etc.) uses the unoverridden ocaml+flambda — the
                # mismatch shows up as cross `ocamlopt` rejecting native
                # `topdirs.cmx` ("not a compilation unit description").
                ocaml-ng = super.ocaml-ng // {
                  ocamlPackages_5_4 = self.ocamlPackages;
                };
              })
              melange.overlays.default
            ];
          in
          f pkgs
        );
    in
    {
      formatter = forAllSystems (pkgs: pkgs.nixfmt);

      # ocamlPackages with dune_3 replaced by specified source
      #
      # IMPORTANT: revdeps-dune input must be overridden, the default is likely stale
      #
      # Usage:
      #
      # Build lwt with current version of dune
      # $ nix build .#revdeps.x86_64-linux.lwt --override-input revdeps-dune path:.
      #
      # Build base and core with dune 3.20.2
      # $ nix build .#revdeps.x86_64-linux.{base,core} --override-input revdeps-dune github:ocaml/dune/3.20.2
      #
      # Build lwt with a specific commit (useful for bisection)
      # $ nix build .#revdeps.x86_64-linux.lwt --override-input revdeps-dune github:ocaml/dune/a1b2c3d
      revdeps = forAllSystems (
        pkgs:
        import ./nix/revdeps.nix {
          inherit
            nixpkgs
            ocaml-overlays
            revdeps-dune
            pkgs
            ;
        }
      );

      packages =
        let
          # Nixpkgs 26.11 dropped x86_64-darwin. Keep the Intel macOS binary
          # on the last supported release.
          nixpkgsDarwin = builtins.getFlake (
            "github:NixOS/nixpkgs/fca2dbd4c00c3063235e56bb91758e24fc67b7b8"
            + "?narHash=sha256-uH9LkreZXkpZXD0QOXBkQWnAHhlVuT0wUABFw7AN9BU%3D"
          );
          dune =
            (import ./nix/dune-package.nix {
              nixpkgs = nixpkgsDarwin;
              inherit ocaml-overlays;
              pkgs = nixpkgsDarwin.legacyPackages.x86_64-darwin;
              src = ./.;
            }).default;
        in
        forAllSystems (
          pkgs:
          let
            dune-package = import ./nix/dune-package.nix {
              inherit nixpkgs ocaml-overlays pkgs;
              src = ./.;
            };
          in
          rec {
            inherit (dune-package)
              default
              musl-static
              windows-static
              ;
            dune = default;
            dune-static = musl-static;
          }
        )
        // {
          x86_64-darwin = {
            inherit dune;
            default = dune;
          };
        };

      devShells = forAllSystems (
        pkgs:
        let
          INSIDE_NIX = "true";
          ocamlformat =
            let
              lists = pkgs.lib.lists;
              strings = pkgs.lib.strings;
              ocamlformat_config = strings.splitString "\n" (builtins.readFile ./.ocamlformat);
              prefix = "version=";
              ocamlformat_version_pred = line: strings.hasPrefix prefix line;
              version_line = lists.findFirst ocamlformat_version_pred "not_found" ocamlformat_config;
              version_string = strings.removePrefix prefix version_line;
              ocamlformat_attr = builtins.replaceStrings [ "." ] [ "_" ] version_string;
            in
            builtins.getAttr (
              "ocamlformat_" + ocamlformat_attr
            ) nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system};
          testBuildInputs =
            with pkgs;
            [
              jq
              ripgrep
              shellcheck
              mercurial
              unzip
              coreutils
              bashInteractive
              curl
              git
              binaryen
              procps
              which
            ]
            ++ lib.optionals (lib.meta.availableOn stdenv.hostPlatform fish) [
              # Fish is not installed on CI runners, and its completion backend
              # is maintained in Dune rather than generated by Cmdliner.
              fish
            ]
            ++ lib.optionals stdenv.isLinux [ strace ];
          testNativeBuildInputs =
            pkgs:
            with pkgs;
            [
              nodejs-slim
              pkg-config
              opam
              ocamlformat
            ]
            ++ lib.optionals stdenv.isLinux [ bubblewrap ];

          docInputs = with pkgs.python3.pkgs; [
            sphinx-autobuild
            furo
            sphinx-copybutton
            sphinx-design
            myst-parser
          ];
          makeDuneDevShell = import ./nix/dev-shell.nix {
            inherit
              pkgs
              INSIDE_NIX
              testBuildInputs
              testNativeBuildInputs
              docInputs
              ;
            sourceDune = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
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
            inputsFrom = [
              pkgs.dune_3
            ];
            buildInputs = with pkgs.ocamlPackages; [
              csexp
              pp
              # Some additional dependencies are needed because formatting
              # promoted files in boot/ requires building them first
              ppx_expect
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
              that can run the testsuite (except the melange tests).
            '';
          };
          slim-melange = makeDuneDevShell {
            extraBuildInputs = pkgs: [
              pkgs.ocamlPackages.melange
            ];
            meta.description = ''
              Provides a minimal shell environment built purely from nixpkgs
              that can run the test suite
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

          inherit (import ./nix/devShells/rocq.nix { inherit pkgs makeDuneDevShell; })
            rocq
            rocq-native
            ;

          bootstrap-check =
            let
              # Older nixpkgs needed only to source OCaml 4.02.
              pkgs_4_02 = import (builtins.fetchTree {
                type = "github";
                owner = "nixos";
                repo = "nixpkgs";
                rev = "7f50d4b33363d3948543f6a02b90a2c66852a453";
              }) { inherit (pkgs.stdenv.hostPlatform) system; };
            in
            pkgs.mkShell {
              inherit INSIDE_NIX;
              buildInputs = [
                pkgs.gnumake
                pkgs_4_02.ocaml-ng.ocamlPackages_4_02.ocaml
              ];
              meta.description = ''
                Provides a minimal shell environment with OCaml 4.02 in order
                to test the bootstrapping script.
              '';
            };

          bootstrap-check_4_14 = pkgs.mkShell {
            inherit INSIDE_NIX;
            buildInputs = [
              pkgs.gnumake
              pkgs.ocaml-ng.ocamlPackages_4_14.ocaml
            ];
            meta.description = ''
              Provides a minimal shell environment with OCaml 4.14 in order
              to test the bootstrapping script.
            '';
          };

          inherit (import ./nix/devShells/ox.nix { inherit pkgs makeDuneDevShell INSIDE_NIX; })
            bootstrap-ox
            ox-minimal
            ox-minimal-trunk
            ox
            ;

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

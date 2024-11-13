{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocamllsp = {
      url = "git+https://github.com/ocaml/ocaml-lsp?submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    melange = {
      url = "github:melange-re/melange/refs/tags/4.0.0-51";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    ocaml-overlays = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };
  outputs =
    { self
    , flake-utils
    , nixpkgs
    , ocamllsp
    , melange
    , ocaml-overlays
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
        (self: super: {
          ocamlPackages = super.ocaml-ng.ocamlPackages_5_1.overrideScope (oself: osuper: {
            mdx = osuper.mdx.override {
              logs = oself.logs;
            };
            utop = osuper.utop.overrideAttrs {
              dontGzipMan = true;
            };
          });
        })
        melange.overlays.default
        ocamllsp.overlays.default
        (self: super: {
          coq_8_16_native = super.coq_8_16.overrideAttrs (a: {
            configureFlags = [ "-native-compiler" "yes" ];
          });
        })
      ];
      dune-static-overlay = self: super: {
        ocamlPackages = super.ocaml-ng.ocamlPackages_5_1.overrideScope (oself: osuper: {
          dune_3 = osuper.dune_3.overrideAttrs (a: {
            src = ./.;
            preBuild = "ocaml boot/bootstrap.ml --static";
          });
        });
      };
      pkgs-static = nixpkgs.legacyPackages.${system}.appendOverlays [
        ocaml-overlays.overlays.default
        dune-static-overlay
      ];

      add-experimental-configure-flags = pkg: pkg.overrideAttrs {
        configureFlags =
          [
            "--pkg-build-progress" "enable"
            "--lock-dev-tool" "enable"
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

      testBuildInputs = with pkgs;
        [ file mercurial ]
        ++ lib.optionals stdenv.isLinux [ strace ];
      testNativeBuildInputs = with pkgs; [ nodejs-slim pkg-config opam ocamlformat ];
    in
    {
      formatter = pkgs.nixpkgs-fmt;

      packages = {
        default = with pkgs; stdenv.mkDerivation {
          pname = "dune";
          version = "n/a";
          src = ./.;
          nativeBuildInputs = with ocamlPackages; [ ocaml findlib ];
          buildInputs = lib.optionals stdenv.isDarwin [
            darwin.apple_sdk.frameworks.CoreServices
          ];
          strictDeps = true;
          buildFlags = [ "release" ];
          dontAddPrefix = true;
          dontAddStaticConfigureFlags = true;
          configurePlatforms = [ ];
          installFlags = [ "PREFIX=${placeholder "out"}" "LIBDIR=$(OCAMLFIND_DESTDIR)" ];
        };
        dune = self.packages.${system}.default;
        dune-static = pkgs-static.pkgsCross.musl64.ocamlPackages.dune;
        dune-experimental = add-experimental-configure-flags self.packages.${system}.dune;
        dune-static-experimental = add-experimental-configure-flags self.packages.${system}.dune-static;
      };

      devShells =
        let
          makeDuneDevShell =
            { extraBuildInputs ? [ ]
            , meta ? null
            , duneFromScope ? false
            }:
            let
              pkgs' =
                if duneFromScope then
                  pkgs.extend
                    (pself: psuper: {
                      ocamlPackages = psuper.ocamlPackages.overrideScope (oself: osuper: {
                        dune_3 = self.packages.${system}.default;
                      });
                    })
                else pkgs;

              inherit (pkgs') writeScriptBin stdenv lib;

              docInputs = with pkgs'.python3.pkgs; [
                sphinx-autobuild
                furo
                sphinx-copybutton
                sphinx-design
                myst-parser
              ];
              duneScript =
                writeScriptBin "dune" ''
                  #!${stdenv.shell}
                  "$DUNE_SOURCE_ROOT"/_boot/dune.exe $@
                '';
            in

            pkgs'.mkShell {
              shellHook = ''
                export DUNE_SOURCE_ROOT=$PWD
              '';
              inherit meta;
              nativeBuildInputs = testNativeBuildInputs
                ++ docInputs
                ++ [ duneScript ];
              inputsFrom = [ pkgs'.ocamlPackages.dune_3 ];
              buildInputs = testBuildInputs ++ (with pkgs'.ocamlPackages; [
                ocaml-lsp
                merlin
                ppx_expect
                ctypes
                integers
                mdx
                cinaps
                menhir
                odoc
                lwt
                patdiff
              ] ++ extraBuildInputs);
            };
        in
        {
          doc =
            pkgs.mkShell {
              buildInputs = (with pkgs;
                [
                  sphinx
                  sphinx-autobuild
                  python310Packages.sphinx-copybutton
                  python310Packages.furo
                  python310Packages.sphinx-design
                  python310Packages.myst-parser
                ]
              );
              meta.description = ''
                Provides a shell environment suitable for building the Dune
                documentation website (e.g. `make doc`).
              '';
            };

          fmt =
            pkgs.mkShell {
              nativeBuildInputs = [ ocamlformat ];
              inputsFrom = [ pkgs.dune_3 ];
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
            extraBuildInputs = [
              pkgs.ocamlPackages.melange
            ];
            meta.description = ''
              Provides a minimal shell environment built purely from nixpkgs
              that can run the testsuite (except the coq tests).
            '';
          };
          slim-opam = with pkgs; mkShell {
            nativeBuildInputs = lib.remove pkgs.ocamlformat testNativeBuildInputs;
            buildInputs = lib.optionals stdenv.isDarwin [
              darwin.apple_sdk.frameworks.CoreServices
            ];
            meta.description = ''
              provides a shell with just `opam` and minimal (external)
              dependencies to run the testsuite.";
            '';
          };

          coq =
            pkgs.mkShell {
              nativeBuildInputs = testNativeBuildInputs;
              # Coq requires OCaml 4.x
              inputsFrom = [ pkgs.ocaml-ng.ocamlPackages_4_14.dune_3 ];
              buildInputs = with pkgs; [
                coq_8_16_native
                coq_8_16_native.ocamlPackages.findlib
              ];
              meta.description = ''
                Provides a minimal shell environment built purely from nixpkgs
                that can build Dune and the Coq testsuite.
              '';
            };
          microbench = makeDuneDevShell {
            extraBuildInputs = with pkgs.ocamlPackages; [
              core_bench
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
          default =
            makeDuneDevShell {
              extraBuildInputs = (with pkgs; [
                # dev tools
                ccls
              ]) ++ (with pkgs.ocamlPackages; [
                pkgs.ocamlPackages.ocaml-lsp
                pkgs.ocamlPackages.melange
                js_of_ocaml-compiler
                js_of_ocaml
                utop
                core_bench
              ]);
              meta.description = ''
                Provides a shell environment where `dune` is provided and built
                using the source code in this repo.
              '';
            };
        };
    });
}

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
      url = "github:melange-re/melange";
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
    }@inputs:
    let package = "dune";
    in flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
        (self: super: {
          ocamlPackages = self.ocaml-ng.ocamlPackages_4_14;
        })
        melange.overlays.default
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

      testBuildInputs = with pkgs;
        [ file mercurial ]
        ++ lib.optionals stdenv.isLinux [ strace ];
      testNativeBuildInputs = with pkgs; [ nodejs-slim pkg-config opam ocamlformat ];
    in
    rec {
      formatter = pkgs.nixpkgs-fmt;

      packages = rec {
        default = with pkgs; stdenv.mkDerivation {
          pname = package;
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
        dune = default;
      };

      devShells =
        let
          pkgs = nixpkgs.legacyPackages.${system}.appendOverlays [
            (self: super: {
              ocamlPackages = self.ocaml-ng.ocamlPackages_4_14;
            })
            melange.overlays.default
          ];
          makeDuneDevShell =
            { extraBuildInputs ? [ ]
            , meta ? null
            , duneFromScope ? false
            }:
            let
              slimPkgs =
                if duneFromScope then
                  pkgs.extend
                    (self: super: {
                      ocamlPackages = super.ocamlPackages.overrideScope' (oself: osuper: {
                        dune_3 = packages.default;
                      });
                    })
                else pkgs;
            in
            slimPkgs.mkShell {
              inherit meta;
              nativeBuildInputs = testNativeBuildInputs;
              inputsFrom = [ slimPkgs.ocamlPackages.dune_3 ];
              buildInputs = testBuildInputs ++ (with slimPkgs.ocamlPackages; [
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
                  python310Packages.sphinx-rtd-theme
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
              pkgs.ocamlPackages.rescript-syntax
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
              inputsFrom = [ pkgs.dune_3 ];
              buildInputs = with pkgs; [
                coq_8_16
                coq_8_16.ocamlPackages.findlib
              ];
              meta.description = ''
                Provides a minimal shell environment built purely from nixpkgs
                that can build Dune and the Coq testsuite.
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
                patdiff
                ccls
              ]) ++ (with pkgs.ocamlPackages; [
                ocamllsp.outputs.packages.${system}.default
                pkgs.ocamlPackages.melange
                rescript-syntax
                js_of_ocaml-compiler
                js_of_ocaml
                utop
                core_bench
                mdx
                odoc
                ppx_expect
                ctypes
              ]);
              meta.description = ''
                Provides a shell environment where `dune` is provided and built
                using the source code in this repo.
              '';
            };
        };
    });
}

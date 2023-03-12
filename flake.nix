{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-overlays = {
      url = "github:nix-ocaml/nix-overlays";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils.url = "github:numtide/flake-utils";
    ocamllsp = {
      url = "git+https://www.github.com/ocaml/ocaml-lsp?submodules=1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.opam-repository.follows = "opam-repository";
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
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
    , opam-nix
    , nixpkgs
    , ocamllsp
    , opam-repository
    , melange
    , nix-overlays
    }@inputs:
    let package = "dune";
    in flake-utils.lib.eachDefaultSystem (system:
    let
      devPackages = {
        menhir = "*";
        lwt = "*";
        csexp = "*";
        core_bench = "*";
        js_of_ocaml = "*";
        js_of_ocaml-compiler = "*";
        mdx = "*";
        odoc = "*";
        ppx_expect = "*";
        ppxlib = "*";
        ctypes = "*";
        utop = "*";
        cinaps = "*";
        ocamlfind = "1.9.2";
      };
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
      scope =
        opam-nix.lib.${system}.buildOpamProject'
          {
            inherit pkgs;
            repos = [ opam-repository ];
          } ./.
          (devPackages // {
            ocaml-base-compiler = "4.14.0";
          });
      testBuildInputs = with pkgs;
        [ file mercurial ]
        ++ lib.optionals stdenv.isLinux [ strace ];
      testNativeBuildInputs = with pkgs; [ nodejs-slim pkg-config opam ocamlformat ];
    in
    {
      formatter = pkgs.nixpkgs-fmt;

      packages = {
        dune = scope.dune;
        default = with pkgs; stdenv.mkDerivation rec {
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
      };

      devShells =
        let
          pkgs = nix-overlays.legacyPackages.${system}.appendOverlays [
            (self: super: {
              ocamlPackages = self.ocaml-ng.ocamlPackages_4_14;
            })
            melange.overlays.default
          ];
          mkSlim = { extraBuildInputs ? [ ] }:
            pkgs.mkShell {
              nativeBuildInputs = testNativeBuildInputs;
              inputsFrom = [ pkgs.ocamlPackages.dune_3 ];
              buildInputs = testBuildInputs ++ (with pkgs.ocamlPackages; [
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
            };

          fmt =
            pkgs.mkShell {
              nativeBuildInputs = [ ocamlformat ];
              inputsFrom = [ pkgs.dune_3 ];
            };

          slim = mkSlim { };
          slim-melange = mkSlim {
            extraBuildInputs = [
              pkgs.ocamlPackages.melange
              pkgs.ocamlPackages.mel
            ];
          };

          coq =
            pkgs.mkShell {
              nativeBuildInputs = testNativeBuildInputs;
              inputsFrom = [ pkgs.dune_3 ];
              buildInputs = with pkgs; [
                coq_8_16
                coq_8_16.ocamlPackages.findlib
              ];
            };

          default =
            pkgs.mkShell {
              nativeBuildInputs = testNativeBuildInputs;
              buildInputs = testBuildInputs ++ (with pkgs;
                [
                  # dev tools
                  patdiff
                  ccls
                ])
                ++ [
                ocamllsp.outputs.packages.${system}.ocaml-lsp-server
                pkgs.ocamlPackages.melange
                pkgs.ocamlPackages.mel
              ] ++ nixpkgs.lib.attrsets.attrVals (builtins.attrNames devPackages) scope;
              inputsFrom = [ self.packages.${system}.dune ];
            };
        };
    });
}

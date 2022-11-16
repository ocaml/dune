{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ocamllsp.url = "git+https://www.github.com/ocaml/ocaml-lsp?submodules=1";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    melange.url = "github:melange-re/melange";
    melange-compiler-libs.url = "github:melange-re/melange-compiler-libs";
  };
  outputs =
    { self
    , flake-utils
    , opam-nix
    , nixpkgs
    , ocamllsp
    , opam-repository
    , melange
    , melange-compiler-libs
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
        melange-compiler-libs.overlays.default
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
    in
    {
      packages.default = scope.dune;

      devShells.doc =
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

      devShells.fmt =
        pkgs.mkShell {
          inputsFrom = [ pkgs.dune_3 ];
          buildInputs = [ ocamlformat ];
        };

      devShells.slim = with pkgs.ocamlPackages; pkgs.mkShell {
        inputsFrom = [ dune_3 ];
        nativeBuildInputs = with pkgs; [ pkg-config nodejs-slim ];
        buildInputs = [
          merlin
          ocamlformat
          ppx_expect
          ctypes
          integers
          mdx
          cinaps
          menhir
          odoc
          lwt
        ];
      };

      devShells.coq =
        pkgs.mkShell {
          inputsFrom = [ pkgs.dune_3 ];
          buildInputs = with pkgs; [
            coq_8_16
            coq_8_16.ocamlPackages.findlib
          ];
        };

      devShells.default =
        pkgs.mkShell {
          nativeBuildInputs = [ pkgs.opam ];
          buildInputs = (with pkgs;
            [
              # dev tools
              ocamlformat
              coq_8_16
              nodejs-slim
              pkg-config
              file
              ccls
              mercurial
            ] ++ (if stdenv.isLinux then [ strace ] else [ ]))
          ++ [
            ocamllsp.outputs.packages.${system}.ocaml-lsp-server
            pkgs.ocamlPackages.melange
          ]
          ++ nixpkgs.lib.attrsets.attrVals (builtins.attrNames devPackages) scope;
          inputsFrom = [ self.packages.${system}.default ];
        };
    });
}

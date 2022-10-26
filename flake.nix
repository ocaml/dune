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
    coq = {
      url = "github:coq/coq/V8.16.0";
      flake = false;
    };
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, ocamllsp, opam-repository, melange, coq }@inputs:
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
        coqPackages = {
          coq = "dev";
          coq-native = "*";
        };
        pkgs = nixpkgs.legacyPackages.${system};
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
        scope-coq =
          let
            scope =
              let
                overlay = final: prev: {
                  ${package} = prev.${package}.overrideAttrs (_: {
                    # Do not add share/nix-support, so that dependencies from
                    # the scope don't leak into dependent derivations
                    doNixSupport = false;
                  });
                };
                scope = with opam-nix.lib.${system}; buildOpamProject'
                  {
                    inherit pkgs;
                    repos =
                      [
                        (makeOpamRepo coq)
                        opam-repository
                      ];
                  } ./.
                  (coqPackages // {
                    ocaml-base-compiler = "4.14.0";
                  });
              in
              scope.overrideScope' overlay;
          in
          scope // { default = self.packages.${system}.${package}; };
      in
      {
        packages.default = scope.dune;

        devShells.doc =
          pkgs.mkShell {
            buildInputs = (with pkgs;
              [
                sphinx
                sphinx-autobuild
                python310Packages.sphinx-rtd-theme
              ]
            );
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

        devShells.coq = with pkgs.ocamlPackages; pkgs.mkShell {
          inputsFrom = [ dune_3 ];
          nativeBuildInputs = [ ];
          buildInputs = (with pkgs;
            [
              ocamlformat
            ])
          ++ [
            ocamllsp.outputs.packages.${system}.ocaml-lsp-server
          ]
          ++ nixpkgs.lib.attrsets.attrVals (builtins.attrNames coqPackages) scope-coq;
        };

        devShells.default =
          pkgs.mkShell {
            nativeBuildInputs = [ pkgs.opam ];
            buildInputs = (with pkgs;
              [
                # dev tools
                ocamlformat
                nodejs-slim
                pkg-config
                file
                ccls
                mercurial
              ] ++ (if stdenv.isLinux then [ strace ] else [ ]))
            ++ [
              ocamllsp.outputs.packages.${system}.ocaml-lsp-server
              melange.outputs.packages.${system}.default
            ]
            ++ nixpkgs.lib.attrsets.attrVals (builtins.attrNames devPackages) scope;
            inputsFrom = [ self.packages.${system}.default ];
          };
      });
}

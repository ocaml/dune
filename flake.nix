{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    ocamllsp.url = "git+https://www.github.com/ocaml/ocaml-lsp?submodules=1";
    opam-nix.follows = "ocamllsp/opam-nix";
    nixpkgs.follows = "ocamllsp/nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, ocamllsp }@inputs:
    let package = "dune";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        devPackages = {
          menhir = null;
          lwt = null;
          csexp = null;
          core_bench = null;
          bisect_ppx = null;
          js_of_ocaml = null;
          js_of_ocaml-compiler = null;
          mdx = null;
          merlin = null;
          odoc = null;
          ppx_expect = null;
          ppxlib = null;
          ctypes = null;
          utop = null;
          cinaps = null;
          ocamlfind = "1.9.2";
        };
        on = opam-nix.lib.${system};
      in {
        legacyPackages = let
          scope = on.buildOpamProject { } package ./. devPackages;
          overlay = self: super: { };
        in scope.overrideScope' overlay;

        defaultPackage = self.legacyPackages.${system}.${package};

        devShell = pkgs.mkShell {
          nativeBuildInputs = let scope = self.legacyPackages.${system};
          in with pkgs;
          [
            ocamllsp.outputs.defaultPackage.${system}
            # dev tools
            ocamlformat_0_21_0
            opam
            nodejs-slim
            pkg-config
          ] ++ (builtins.map (s: builtins.getAttr s scope)
            (builtins.attrNames devPackages));
          inputsFrom = [ self.defaultPackage.${system} ];
        };
      });
}

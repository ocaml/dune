{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    ocamllsp.url = "git+https://www.github.com/ocaml/ocaml-lsp?submodules=1";
    ocamllsp.inputs.opam-nix.follows = "opam-nix";
    ocamllsp.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, flake-utils, opam-nix, nixpkgs, ocamllsp }@inputs:
    let package = "dune";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        devPackages = {
          menhir = "*";
          lwt = "*";
          csexp = "*";
          core_bench = "*";
          bisect_ppx = "*";
          js_of_ocaml = "*";
          js_of_ocaml-compiler = "*";
          mdx = "*";
          merlin = "*";
          odoc = "*";
          ppx_expect = "*";
          ppxlib = "*";
          ctypes = "*";
          utop = "*";
          cinaps = "*";
          ocamlfind = "1.9.2";
        };
      in
      {
        packages =
          let
            scope = opam-nix.lib.${system}.buildOpamProject' { } ./.
              (devPackages // { ocaml-base-compiler = "4.14.0"; });
          in
          scope // { default = self.packages.${system}.${package}; };

        devShell =
          let
            pkgs = nixpkgs.legacyPackages.${system};
          in
          pkgs.mkShell {
            nativeBuildInputs = [ pkgs.opam ];
            buildInputs = (with pkgs;
              [
                # dev tools
                ocamlformat_0_21_0
                coq_8_16
                nodejs-slim
                pkg-config
                ccls
              ]) ++ [ ocamllsp.outputs.packages.${system}.ocaml-lsp-server ]
            ++ (builtins.map (s: builtins.getAttr s self.packages.${system})
              (builtins.attrNames devPackages));
            inputsFrom = [ self.packages.${system}.default ];
          };
      });
}

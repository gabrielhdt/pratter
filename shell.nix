{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  packages = builtins.attrValues { inherit (pkgs.ocamlPackages) 
    ocaml 
    ocaml-lsp
    ocamlformat_0_26_2
    dune_3 findlib camlp-streams alcotest qcheck qcheck-alcotest; };
}

{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  packages = builtins.attrValues {
    inherit (pkgs.ocamlPackages)
      ocaml
      ocaml-lsp
      ocamlformat_0_26_2
      utop
      dune-release
      dune_3
      findlib
      alcotest
      qcheck
      qcheck-alcotest
      ;
  };
}

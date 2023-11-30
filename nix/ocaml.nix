{ ... }:

{
  perSystem = { pkgs, ... }: {
    devShells.ocaml = pkgs.mkShell {
      nativeBuildInputs = with pkgs.ocamlPackages; [
        angstrom
        base
        batteries
        core
        dune_3
        findlib
        ocaml
        ocaml-lsp
        pkgs.ocamlformat
        odoc
        ounit
        utop
      ];
    };

    treefmt = {
      programs = {
        ocamlformat.enable = true;
      };
    };
  };
}

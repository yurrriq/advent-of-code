{ ... }:

{
  flake = {
    overlays.emacs = _final: prev: {
      myEmacs = prev.emacsWithPackagesFromUsePackage {
        alwaysEnsure = true;
        config = ../emacs.el;
      };
    };
  };

  perSystem = { pkgs, ... }: {
    devShells.emacs = pkgs.mkShell {
      nativeBuildInputs = with pkgs; [
        myEmacs
        nixd
      ];
    };
  };
}

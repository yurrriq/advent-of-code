{ ... }:

{
  flake = {
    overlays.noweb = final: prev: {
      noweb = prev.noweb.override {
        icon-lang = prev.icon-lang.override {
          withGraphics = false;
        };
      };

      xelatex-noweb = prev.texlive.combine {
        inherit (prev.texlive) scheme-small;
        inherit (final) noweb;
        # Packages for Noweb
        inherit (prev.texlive)
          braket
          catchfile
          datatool
          datetime
          # dirtytalk
          fmtcount
          framed
          fvextra
          # glossaries
          # glossaries-extra
          hardwrap
          ifplatform
          latexmk
          mfirstuc
          minted
          substr
          titlesec
          # tkz-base
          todonotes
          tufte-latex
          xetex
          xindy
          xfor
          xstring
          ;
        # Packages for GAP
        inherit (prev.texlive)
          helvetic
          enumitem
          ;
      };
    };
  };

  perSystem = { pkgs, ... }: {
    devShells.noweb = pkgs.mkShell {
      nativeBuildInputs = with pkgs; [
        noweb
        python3Packages.pygments
        xelatex-noweb
      ];
    };
  };
}

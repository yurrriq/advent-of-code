{ pkgs ? import ./nix }:
let
  project = import ./. { inherit pkgs; };
in
pkgs.mkShell {
  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = [ pkgs.iosevka ];
  };
  buildInputs = with pkgs; (
    [
      cabal-install
      gap-full
      (
        idrisPackages.with-packages
          (
            with idrisPackages; [
              contrib
              effects
              lightyear
            ]
          )
      )
      nixpkgs-fmt
      niv
      noweb
      picat
      python36
      which
      xelatex-noweb
    ] ++ (
      with haskellPackages;
      [
        hoogle
        ormolu
        pointfree
      ]
    ) ++ (
      with python3Packages;
      [
        pre-commit
        pygments
      ]
    )
  )
  ++ project.env.nativeBuildInputs;
}

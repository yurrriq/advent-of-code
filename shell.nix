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
      gap-full
      (
        idrisPackages.with-packages
          (
            with idrisPackages; [
              effects
            ]
          )
      )
      nixpkgs-fmt
      noweb
      python36
      which
      xelatex-noweb
    ] ++ (
      with haskellPackages;
      [
        ormolu
        pointfree
      ]
    ) ++ (
      with python36Packages;
      [
        pre-commit
        pygments
      ]
    )
  )
  ++ project.env.nativeBuildInputs;
}

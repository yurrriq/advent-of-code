{ pkgs ? import ./nix {} }:

let
  project = import ./release.nix { inherit pkgs; };
in

pkgs.mkShell {
  FONTCONFIG_FILE = pkgs.makeFontsConf {
    fontDirectories = [ pkgs.iosevka ];
  };
  buildInputs = project.env.nativeBuildInputs ++ (with pkgs; [
    gap-full
    (idrisPackages.with-packages (with idrisPackages; [
      effects
    ]))
    noweb
    python36Packages.pygments
    which
    xelatex-noweb
  ]);
}

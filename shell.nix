{ pkgs ? import ./nix {} }:

let
  project = import ./release.nix { inherit pkgs; };
in

pkgs.mkShell {
  buildInputs = project.env.nativeBuildInputs;
}

{ sources ? import ./sources.nix }:

import sources.nixpkgs {
  overlays = [
    # (import sources.nur {}).repos.yurrriq.overlays.nur
    (import ./overlay.nix)
  ];
}

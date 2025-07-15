{ sources ? import ./nix/sources.nix
} :
let
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  myHaskellPackages = pkgs.haskellPackages;
  src = pkgs.lib.cleanSourceWith {
    filter = name: type: !(pkgs.lib.hasSuffix ".cabal" name);
    src = ./.;
  };

in
myHaskellPackages.callCabal2nix "HaskellNixCabalStarter" (src) {}

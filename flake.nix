{
  description = "hdev - a convenient Haskell development tool that wraps ghcid";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hdev = pkgs.haskellPackages.callCabal2nix "hdev" ./. { };
    in
    {
      packages.${system} = {
        hdev = hdev;
        default = hdev;
      };

      checks.${system} = {
        hdev = self.packages.${system}.hdev;
        devShell = self.devShells.${system}.default;
      };

      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        packages = _: [ hdev ];
        nativeBuildInputs = with pkgs; [
          cabal-install
          haskellPackages.ghcid
          haskellPackages.hpack
        ];
      };
    };
}

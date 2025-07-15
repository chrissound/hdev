let
  sources = import ./nix/sources.nix;
  niv = import sources.nixpkgs {
    overlays = [
      (_ : _ : { niv = import sources.niv {}; })
    ] ;
    config = {};
  };
  pkgs = niv.pkgs;
  project = import ./default.nix {};
  pythonWithPackages = pkgs.python3.withPackages (ps: with ps; [
    pyyaml
  ]);
in
  project.env.overrideAttrs (old: {
    buildInputs = old.buildInputs ++ [ 
      pkgs.haskellPackages.ghcid 
      pythonWithPackages
    ];
  })

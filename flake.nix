{
  inputs = {
    utils.url = "github:yatima-inc/nix-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.05";
  };

  outputs =
    { self
    , utils
    , nixpkgs
    }:
    utils.inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      lib = utils.lib.${system};
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (lib) buildRustProject testRustProject rustDefault filterRustProject;
      rust = rustDefault;
      crateName = "radiya";
      root = ./.;
    in
    {
      packages.${crateName} = buildRustProject { inherit root; };

      checks.${crateName} = testRustProject { doCheck = true; inherit root; };

      defaultPackage = self.packages.${system}.${crateName};

      # `nix develop`
      devShell = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.packages.${system};
        nativeBuildInputs = [ rust ];
        buildInputs = with pkgs; [
          rust-analyzer
          clippy
          rustfmt
        ];
      };
    });
}

{
  inputs = {
    utils.url = "github:yatima-inc/nix-utils";
  };

  outputs =
    { self
    , utils
    }:
    utils.inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      lib = utils.lib.${system};
      pkgs = utils.nixpkgs.${system};
      inherit (lib) buildRustProject testRustProject rustDefault filterRustProject;
      rust = rustDefault;
      crateName = "my-crate";
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

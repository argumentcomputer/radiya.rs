{
  inputs = {
    utils.url = "github:yatima-inc/nix-utils";
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.05;
    lean = {
      url = github:yatima-inc/lean4/acs/add-nix-ability-for-native-libs;
    };
    lean-blake3.url = github:yatima-inc/lean-blake3;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , utils
    , nixpkgs
    , flake-utils
    , lean
    , lean-blake3
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      lib = utils.lib.${system};
      pkgs = nixpkgs.legacyPackages.${system};
      leanPkgs = lean.packages.${system};
      Radiya = leanPkgs.buildLeanPackage {
        src = ./.;
        name = "Radiya";
        deps = [ lean-blake3.project.${system} ];
      };
      inherit (lib) buildRustProject testRustProject rustDefault filterRustProject;
      rust = rustDefault;
      crateName = "radiya";
      root = ./.;
    in
    {
      inherit Radiya;
      packages = {
        "Radiya" = Radiya.executable;
        ${crateName} = buildRustProject { inherit root; };
      };

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

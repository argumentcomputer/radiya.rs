{
  inputs = {
    lean = {
      # url = github:leanprover/lean4;
      url = github:yatima-inc/lean4/acs/fix-duplicate-dep-root;
    };
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.05;
    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # A lean dependency
    lean-ipld = {
      url = github:yatima-inc/lean-ipld;
      inputs.lean.follows = "lean";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "utils";
    };
    utils.url = "github:yatima-inc/nix-utils";
  };

  outputs =
    { self
    , utils
    , lean
    , flake-utils
    , nixpkgs
    , lean-ipld
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      lib = utils.lib.${system};
      leanPkgs = lean.packages.${system};
      pkgs = nixpkgs.legacyPackages.${system};
      Ipld = lean-ipld.project.${system};
      # Lean
      Radiya = leanPkgs.buildLeanPackage {
        name = "Radiya";
        deps = with leanPkgs; [ Init Lean Ipld ];
        src = ./lean;
      };
      # Rust
      inherit (lib) buildRustProject testRustProject rustDefault filterRustProject;
      rust = rustDefault;
      crateName = "radiya";
      root = ./.;
      joinDepsDerivationns = getSubDrv: lib.concatStringsSep ":" (map (d: "${getSubDrv d}") ([ ] ++ Radiya.allExternalDeps));  
    in
    {
      packages = {
        Radiya = Radiya.executable;
        ${crateName} = buildRustProject { inherit root; };
      };

      checks.${crateName} = testRustProject { doCheck = true; inherit root; };

      defaultPackage = self.packages.${system}.${crateName};

      # `nix develop`
      devShell = pkgs.mkShell {
        inputsFrom = builtins.attrValues self.packages.${system};
        nativeBuildInputs = [ rust ];
        buildInputs = with pkgs; [
          leanPkgs.lean
          rust-analyzer
          clippy
          rustfmt
        ];
        LEAN_PATH = joinDepsDerivationns (d: d.modRoot);
        LEAN_SRC_PATH = joinDepsDerivationns (d: d.src);    
      };
    });
}

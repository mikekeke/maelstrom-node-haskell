{
  description = "Gossip Glomers env";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    }:

    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [
          cabal-install
          haskell-language-server
          nixpkgs-fmt
          haskellPackages.cabal-fmt
          haskellPackages.fourmolu
          haskellPackages.implicit-hie
          haskellPackages.pointfree
        ];

        LANG = "C.UTF-8";

        # shellHook = ''
        #   echo "test"
        # '';
      };
    });
}

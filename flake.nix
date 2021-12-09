{
  description = "xmonad-config's description";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/2df15ba83d0510a56f2583fd3481723835acb5a1";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [ ];
        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };
        # https://github.com/NixOS/nixpkgs/issues/140774#issuecomment-976899227
        m1MacHsBuildTools =
          pkgs.haskellPackages.override {
            overrides = self: super:
              let
                workaround140774 = hpkg: with pkgs.haskell.lib;
                  overrideCabal hpkg (drv: {
                    enableSeparateBinOutput = false;
                  });
              in
              {
                ghcid = workaround140774 super.ghcid;
              };
          };
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "xmonad-config";
            root = ./.;
            withHoogle = false;
            overrides = self: super: with pkgs.haskell.lib; {
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/k3vxjd8meko7
              xmonad = self.callHackage "xmonad" "0.17.0" { };
              xmonad-contrib = self.callHackage "xmonad-contrib" "0.17.0" { };
              xmonad-extras = self.callHackage "xmonad-extras" "0.17.0" { };
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv
                (with pkgs.haskellPackages; [
                  # Specify your build/dev dependencies here.
                  cabal-fmt
                  cabal-install
                  ghcid
                  haskell-language-server
                  brittany
                  pkgs.nixpkgs-fmt
                ]);
          };
      in
      {
        # Used by `nix build` & `nix run` (prod exe)
        defaultPackage = project false;

        # Used by `nix develop` (dev shell)
        devShell = project true;
      });
}

{
  description = "A sandbox for tinkering with Vulkan in Haskell.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
#      hs = pkgs.haskell.packages.ghc948;
#      hs = pkgs.haskell.packages.ghc90;
      hs = pkgs.haskell.packages.ghc98;
      VulkanSandbox = hs.callCabal2nix "VulkanSandbox" ./VulkanSandbox {};
    in {
      inherit pkgs;

      packages.x86_64-linux = { inherit VulkanSandbox; };

      packages.x86_64-linux.default = hs.shellFor {
        packages = p: [ ];
        nativeBuildInputs =
          (with hs; [
            cabal-install
#            haskell-language-server
          ]) ++
          (with pkgs; [
            libGL
            vulkan-headers
            vulkan-loader
            vulkan-tools
            vulkan-validation-layers
            xorg.libX11
            xorg.libXcursor
            xorg.libXi
            xorg.libXinerama
            xorg.libXrandr
            xorg.libXxf86vm
          ]);
      };

    };
}

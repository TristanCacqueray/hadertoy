# Unfortunately, this doesn't work, nix-shell fails with:
#
# <command line>: cannot satisfy -package-id dear-imgui-1.0.0-inplace:
#    dear-imgui-1.0.0-inplace is unusable due to missing dependencies:
#      GLFW-b-3.3.0.0-LREaDWUCOMdKGiBiHUNZy1 ...
#
# Instead use this custom shell:
# deps="base sdl2 vulkan vulkan-utils gl containers inline-c inline-c-cpp StateVar managed cabal-install OpenGL OpenGLRaw GLFW-b vector doctest"
# nix-shell -p "haskellPackages.ghcWithPackages (p: with p; [${deps}])" -p libGL -p SDL2 -p glfw3 -p pkgconfig -p gcc
{ nixpkgs ? import <nixpkgs> { } }:
let
  name = "hadertoy";
  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      dear-imgui =
        self.callCabal2nix "dear-imgui" ../../haskell-game/dear-imgui.hs { };
    };
  };
  drv = haskellPackages.callCabal2nix name ./. { };
  shellDrv = haskellPackages.shellFor {
    withHoogle = false;
    packages = p: [ drv ];
    buildInputs = with haskellPackages; [ hlint cabal-install ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv

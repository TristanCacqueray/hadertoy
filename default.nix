{ nixpkgs ? import (fetchTarball
  "https://github.com/NixOS/nixpkgs/archive/18cc4df312e74e6d47ea8d1c26f19a3d8f524ed7.tar.gz")
  { } }:
let
  name = "hadertoy";
  haskellPackages = nixpkgs.haskellPackages.extend (self: super: {
    dear-imgui = let
      drv = self.callCabal2nixWithOptions "dear-imgui"
        (nixpkgs.fetchFromGitHub {
          owner = "haskell-game";
          repo = "dear-imgui.hs";
          rev = "b5c310429f265066921ed2fba13901068b5b34bc";
          sha256 = "1kzazlnc4ldcbj7qpgciz5qwy5qf97qd1s1yljarrk1p9f28c7hv";
          fetchSubmodules = true;
        }) "--flag=sdl --flag=glfw --flag=opengl --flag=vulkan" { };
    in nixpkgs.haskell.lib.overrideCabal drv (old: {
      # isExecutable doesn't seems to work, thus vulkan needs to be enable to avoid exe:vulkan build failure
      # Otherwise, vulkan and sdl flags could be dropped since we only need glfw here.
      isExecutable = false;
      buildDepends = (old.buildDepends or [ ])
        ++ [ nixpkgs.vulkan-loader nixpkgs.vulkan-headers ];
    });
  });
  drv = haskellPackages.callCabal2nix name ./. { };
  shellDrv = haskellPackages.shellFor {
    withHoogle = false;
    packages = p: [ drv ];
    buildInputs = with haskellPackages; [
      haskell-language-server
      hlint
      cabal-install
    ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv

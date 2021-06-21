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
          rev = "d42eb672a1d1a708041933f0e1128f9e94af253c";
          sha256 = "1c2x3p5lnz05x32zcr8l29mgyqwsblr5svjn35kp1g7bvc0i1n2g";
          fetchSubmodules = true;
        }) "--flag=-sdl --flag=+glfw --flag=+opengl2 --flag=-vulkan" { };
    in nixpkgs.haskell.lib.overrideCabal drv (old: {
      # isExecutable doesn't seems to work, thus vulkan needs to be enable to avoid exe:vulkan build failure
      # Otherwise, vulkan and sdl flags could be dropped since we only need glfw here.
      isExecutable = false;
      buildDepends = (old.buildDepends or [ ]) ++ [
        nixpkgs.vulkan-loader
        nixpkgs.vulkan-headers
        nixpkgs.pkg-config
        nixpkgs.glew
        nixpkgs.xorg.libX11
        nixpkgs.glfw3
        nixpkgs.haskellPackages.GLFW-b
        nixpkgs.haskellPackages.gl
      ];
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
      nixpkgs.pkg-config
      nixpkgs.glew
      nixpkgs.xorg.libX11
      nixpkgs.glfw3
    ];
  };
in if nixpkgs.lib.inNixShell then shellDrv else drv

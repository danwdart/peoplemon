{ 
  nixpkgs ? import <unstable> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {},
  compiler ? "ghc922"
} :
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      sdl2 = lib.doJailbreak super.sdl2;
      peoplemon = lib.dontHaddock (self.callCabal2nix "peoplemon" (gitignore ./.) {});
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.peoplemon
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f | grep -v dist-newstyle); do krank $i; done
      doctest src lib
      cabal update
    '';
    buildInputs = tools.defaultBuildTools;
    withHoogle = false;
  };
  exe = nixpkgs.haskell.lib.justStaticExecutables (myHaskellPackages.peoplemon);
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
  peoplemon = myHaskellPackages.peoplemon;
}


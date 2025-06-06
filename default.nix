{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc9122"
}:

let
  nix = import ./nix;
  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };

  inherit (pkgs) lib;

  hsPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  sources = [
    "^Data.*$"
    "^tests.*$"
    "^internal.*$"
    "^benchmarks.$"
    "^examples.*$"
    "^.*\\.cabal$"
    "^trace-embrace.yaml$"
    "^LICENSE$"
    "^README.md$"
    "^changelog.md$"
  ];

  baseHaskellPkgs = pkgs.haskell.packages.${ghc};

  base = (hsPkgs.callCabal2nix "attoparsec-isotropic" (lib.sourceByRegex ./. sources) { });
  attoparsec-overlay = _hf: _hp: {
    attoparsec-isotropic = base;
  };

  hsOverlays = [ hsPkgSetOverlay attoparsec-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });

  hls = pkgs.haskell.lib.overrideCabal baseHaskellPkgs.haskell-language-server
    (_: { enableSharedExecutables = true; });

  shell = hsPkgs.shellFor {
    packages = p: [ p.attoparsec-isotropic ];
    nativeBuildInputs = (with pkgs; [
      cabal-install
      ghcid
      hlint
      niv
      pandoc
      git
    ]) ++ [ hls hsPkgs.upload-doc-to-hackage ];
    shellHook = ''
      export PS1='$ '
      echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
    '';
  };

  attoparsec-isotropic = hsPkgs.attoparsec-isotropic;
in {
  inherit hsPkgs;
  inherit ghc;
  inherit pkgs;
  inherit shell;
  inherit attoparsec-isotropic;
}

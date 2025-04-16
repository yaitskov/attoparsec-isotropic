rec {
  sources = import ./sources.nix;
  defaultNixpkgs = import sources.nixpkgs;
  foo = self: super:
    let
      cv = "3.14.1.1";
      oc = self.haskell.lib.overrideCabal;
    in rec
      {
        # Cabal = oc super.Cabal_3_14_1_0 (old: {
        #   version = cv;
        #   revision = null;
        #   editedCabalFile = null; # "0";
        #   sha256 = "1lmnmp1ag9lwaxks66ba26mi4q10afnl0a82nj1fv27bnjzz8hkk";
        # });

        # cabal-install = (oc super.cabal-install (old: {
        #   version = cv;
        #   revision = null;
        #   editedCabalFile = null; # "0";
        #   sha256 = "18cdb2wvic41d6259na9c7ivlgzy89ci8q6d87ri0j0n4x0xkfjc";
        # })).override {
        #   inherit Cabal;
        # };
      };

  pkgSetForSystem =
    system: args: defaultNixpkgs (args // { inherit system; overlays = [ foo ]; });
  pkgSet = pkgSetForSystem builtins.currentSystem;
}

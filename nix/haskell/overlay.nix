{ haskell, lib, sources }:

let
  inherit (haskell.lib) doJailbreak dontCheck doHaddock;

  # 'fakeSha256' is helpful when adding new packages
  #
  # Set 'sha256 = fakeSha256', then replace the SHA with the one reported by
  # Nix when the build fails with a SHA mismatch error.
  inherit (lib) fakeSha256 nameValuePair listToAttrs;

in hfinal: hprev:

(listToAttrs (map (a:
  nameValuePair a.name
    (dontCheck (hfinal.callCabal2nix a.name a.source { }))) [
      { name = "trace-embrace";  source = sources.trace-embrace; }
    ])) // {
      "upload-doc-to-hackage" = hfinal.callPackage sources.upload-doc-to-hackage {};
      "haddock-use-refs" = hfinal.callHackageDirect
        { pkg = "haddock-use-refs";
          ver = "1.0.1";
          sha256 = "sha256-fxrfMQ4CUthzNwYVjwV5kJmmPgimVpbnVhxnoYi1GrE=";
        } {};
    }

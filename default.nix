{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "123a6f487ca954fd983f6d4cd6b2a69d4c463d10";
    sha256 = "121rmnkx8nwiy96ipfyyv6vrgysv0zpr2br46y70zf4d0y1h1lz5";
    })
}:
(import reflex-platform {}).project ({ pkgs, ... }:{
  useWarp = true;
  withHoogle = false;

  packages = {
    cardanomixer-frontend-nix = ../Cardano-Mixer-Frontend;
};
  overrides = let
    servantReflexSrc = builtins.fetchGit {
      url = "https://github.com/s9gf4ult/servant-reflex.git" ;
      rev = "20e2621cc2eca5fe38f8a01c7a159b0b9be524ea" ;
    } ;
    reflexDomContribSrc = builtins.fetchGit {
      url = "https://github.com/reflex-frp/reflex-dom-contrib.git";
      rev = "11db20865fd275362be9ea099ef88ded425789e7";
    };
    cardanoMixerLibSrc = pkgs.fetchFromGitHub {
      owner = "cardmix";
      repo  = "Cardano-Mixer-Lib";
      rev = "77e2fb6ff0ade9834699dc176400426c02dd8be4";
      sha256 = "15b5gj448cbzd651ilf748pwr8a76mshs3dgfz3g1pss4nm67804";
    };
    
  in self: super: with pkgs.haskell.lib; {
    servant-openapi3 = super.callHackage "servant-openapi3" "2.0.1.1" {};
    deriving-aeson = super.callHackage "deriving-aeson" "0.2.3" {};
    openapi3 = doJailbreak super.openapi3;
    aeson-pretty = doJailbreak super.aeson-pretty;
    attoparsec-iso8601 = doJailbreak super.attoparsec-iso8601;
    base-compat = super.callHackage "base-compat" "0.11.1" {};
    base-compat-batteries = super.callHackage "base-compat-batteries" "0.11.1" {};
    generics-sop = super.callHackage "generics-sop" "0.5.1.0" {};
    gi-glib = dontCheck super.gi-glib;
    gi-javascriptcore = dontCheck super.gi-javascriptcore;
    indexed-profunctors = super.callHackage "indexed-profunctors" "0.1" {};
    insert-ordered-containers = super.callHackage "insert-ordered-containers" "0.2.3" {};
    optics-core = super.callHackage "optics-core" "0.2" {};
    optics-extra = super.callHackage "optics-extra" "0.2" {};
    optics-th = super.callHackage "optics-th" "0.2" {};
    quickcheck-instances = doJailbreak super.quickcheck-instances;
    reflex-dom-core = dontCheck super.reflex-dom-core;
    servant = doJailbreak (super.callHackage "servant" "0.18.2" {});
    servant-reflex = doJailbreak (super.callCabal2nix "servant-reflex" servantReflexSrc {});
    sop-core = super.callHackage "sop-core" "0.5.0.1" {};
    uri-bytestring = doJailbreak super.uri-bytestring;
  } // pkgs.lib.optionalAttrs (!(super.ghc.isGhcjs or false)) {
    aeson = dontCheck (doJailbreak (super.callHackage "aeson" "1.4.7.1" {}));
  };

  shellToolOverrides = ghc: super: {
    closure-compiler = null;
    haskell-ide-engine = null;
    hdevtools = null;
    hlint = null;
    stylish-haskell = null;
  };

  shells = {
    ghc = ["cardanomixer-frontend-nix"];
    ghcjs = ["cardanomixer-frontend-nix"];
  };
})


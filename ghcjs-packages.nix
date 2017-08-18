{ reflex-platform, ... }:
let inherit (reflex-platform) nixpkgs;
in reflex-platform.ghcjs.override {

  overrides = self: super: {
    hnock = self.callPackage ({ mkDerivation, parsec }:
      mkDerivation {
        pname = "hnock";
        version = "0.1.0.0";
        src = nixpkgs.fetchgit {
         url = "git@github.com:aupiff/hnock.git";
         rev = "b75de0188ae33509ea270b46d13ec9fa552d84dd";
         sha256 = "1636kqi1qk7b692ipingwb3kjih0bz570iki4hs04yzzglhzdlyx";
        };
        buildDepends = [
          parsec
        ];
        jailbreak = true;
        license = null;
      }
    ) {};

  };
}

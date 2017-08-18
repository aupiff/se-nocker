{ reflex-platform, ... }:
let inherit (reflex-platform) nixpkgs;
in reflex-platform.ghcjs.override {

  overrides = self: super: {
    hnock = self.callPackage ({ mkDerivation, parsec, either}:
      mkDerivation {
        pname = "hnock";
        version = "0.1.0.0";
        src = nixpkgs.fetchgit {
         url = "git@github.com:aupiff/hnock.git";
         rev = "2c253de0374466704fa89c58d89ebce448a3cebb";
         sha256 = "195jv19gkzagpfnzf8a72nj8vqlqni30c1gcpygg8v3n32f4pby3";
        };
        buildDepends = [
          parsec either
        ];
        jailbreak = true;
        license = null;
      }
    ) {};

  };
}

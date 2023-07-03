{
  reflex-platform ? import ./dep/reflex-platform { }
}: (reflex-platform.project ({ pkgs, thunkSource, ... }: {
  name = "hnix-store";
  src = ./.;
  compiler-nix-name = "ghc8107";
  shells = p: with p; [ hnix-store-core hnix-store-remote ];
  shellTools = {
    #haskell-language-server = "1.5.0.0";
    cabal-install = "3.2.0.0";
    #tasty-discover = "5.0.0";
    haskell-language-server = "1.8.0.0";
  };
})).extend (self: super: {
  shells = super.shells // {
    ghc = self.shell-driver {
      exactDeps = false;
      additional = p: with p; [ tasty-discover ];
    };
  };
})

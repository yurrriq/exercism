{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
    devShells.haskell = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        (
          emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          }
        )
        cabal-install
        ghc
        ghcid
        haskell-language-server
      ] ++ (with haskellPackages; [
        apply-refact
        hpack
        hlint
        ormolu
        pointfree
      ]);
    };

    treefmt = {
      programs = {
        hlint.enable = true;
        ormolu = {
          enable = true;
          ghcOpts = [
            "OverloadedStrings"
            "TemplateHaskell"
          ];
        };
      };
      settings.formatter = {
        hlint = {
          excludes = [
            "haskell/**/*_test.hs"
            "haskell/*/test/*.hs"
          ];
        };
        ormolu = {
          excludes = [
            "haskell/*/*_test.hs"
            "haskell/*/test/*.hs"
          ];
        };
      };
    };
  };
}

{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
    devShells.go = pkgs.mkShell {
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
        go
        gopls
        gotools
        revive
      ];
    };

    pre-commit.settings.hooks = {
      revive = {
        enable = true;
        excludes = [
          ".+_test.go$"
        ];
      };
    };

    treefmt = {
      programs = {
        gofumpt.enable = true;
      };
      settings.formatter = {
        gofumpt = {
          excludes = [
            "go/*/*_test.go"
          ];
        };
      };
    };
  };
}

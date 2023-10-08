{ ... }:

{
  perSystem = { config, pkgs, self', system, ... }: {
    devShells.java = pkgs.mkShell {
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
        gradle
        jdt-language-server
        openjdk
      ];

      inherit (config.pre-commit.devShell) shellHook;
    };
  };
}

{ ... }:

{
  perSystem = { config, pkgs, self', system, ... }: {
    devShells.java = pkgs.mkShell {
      inputsFrom = [
        self'.devShells.default
      ];
      nativeBuildInputs = with pkgs; [
        gradle
        jetbrains.idea-community
        openjdk
      ];

      inherit (config.pre-commit.devShell) shellHook;
    };
  };
}

{ ... }:

{
  perSystem = { config, pkgs, self', ... }: {
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
